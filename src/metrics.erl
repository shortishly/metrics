%% Copyright (c) 2022 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(metrics).


-export([all/0]).
-export([callback_mode/0]).
-export([counter_add/1]).
-export([domain/2]).
-export([exposition/1]).
-export([gauge_add/1]).
-export([gauge_sub/1]).
-export([handle_event/4]).
-export([histogram/1]).
-export([info/1]).
-export([init/1]).
-export([start_link/0]).
-export([value/1]).
-export([with_name/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


domain(LogEvent, Extra) ->
    erlang:display(#{log_event => LogEvent, extra => Extra}),
    log.


histogram(#{name := Name, label := Label, value := Value} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),

    case ets:lookup(?MODULE, {Name, Label}) of
        [{_, #{type := histogram,
               buckets := Buckets,
               sum := Sum,
               infinity := Infinity,
               counter := Counter}}] ->
            ok = histogram_update(Counter, Buckets, Infinity, Sum, Value);

        [{_, _}] ->
            error(type_mismatch);

        [] ->
            Buckets = lists:usort(maps:get(buckets, Arg)),
            Counter = counters:new(length(Buckets), []),
            Infinity = counters:new(1, []),
            Sum = atomics:new(1, [{signed, false}]),
            ok = histogram_update(Counter, Buckets, Infinity, Sum, Value),

            NameLabel = {{Name, Label},
                         #{type => histogram,
                           buckets => Buckets,
                           sum => Sum,
                           infinity => Infinity,
                           counter => Counter}},

            ets:insert_new(
              ?MODULE,
              case ets:lookup(?MODULE, Name) of
                  [{_, histogram}] ->
                      NameLabel;

                  [{_, _}] ->
                      error(type_mismatch);

                  [] ->
                      [{Name, histogram}, NameLabel]
              end) orelse ?FUNCTION_NAME(Arg)
    end.


histogram_update(Counter, Buckets, Infinity, Sum, Value) ->
    histogram_update_buckets(Counter, Buckets, Value),
    histogram_update_sum(Sum, Value),
    counters:add(Infinity, 1, 1).


histogram_update_sum(Sum, Delta) ->
    ?FUNCTION_NAME(Sum, Delta, atomics:get(Sum, 1)).


histogram_update_sum(Sum, Delta, Expected) ->
    case atomics:compare_exchange(
           Sum,
           1,
           Expected,
           int64(float64(Expected) + Delta)) of

        ok ->
            ok;

        Actual ->
            ?FUNCTION_NAME(Sum, Delta, Actual)
    end.


histogram_update_buckets(Counter, Buckets, Value) ->
    lists:foreach(
      fun
          ({Ix, _}) ->
              counters:add(Counter, Ix, 1)
      end,
      lists:dropwhile(
        fun
            ({_, LessOrEqual}) ->
                Value > LessOrEqual
        end,
        lists:zip(lists:seq(1, length(Buckets)), Buckets))).


gauge_add(#{name := _, label := _, value := Value} = Arg) ->
    observation(Arg#{type => gauge,
                     op => add(#{ix => 1, value => Value})}),
    ok;

gauge_add(#{name := _, label := _} = Arg) ->
    ?FUNCTION_NAME(Arg#{value => 1}).


gauge_sub(#{name := _, label := _, value := Value} = Arg) ->
    observation(Arg#{type => gauge,
                     op => sub(#{ix => 1, value => Value})}),
    ok;

gauge_sub(#{name := _, label := _} = Arg) ->
    ?FUNCTION_NAME(Arg#{value => 1}).


counter_add(#{name := _, label := _, value := Value} = Arg) ->
    observation(Arg#{type => counter,
                     op => add(#{ix => 1, value => Value})}),
    ok;

counter_add(#{name := _, label := _} = Arg) ->
    ?FUNCTION_NAME(Arg#{value => 1}).


observation(#{name := Name,
              label := Label,
              type := Type,
              op := Op} = Arg) ->
    case ets:lookup(?MODULE, {Name, Label}) of
        [{_, #{type := Type, counter := Counter}}] ->
            ok = Op(Counter);

        [{_, _}] ->
            error(type_mismatch);

        [] ->
            Counter = counters:new(1, []),
            ok = Op(Counter),

            ets:insert_new(
              ?MODULE,
              case ets:lookup(?MODULE, Name) of
                  [{_, Type}] ->
                      {{Name, Label}, #{type => Type, counter => Counter}};

                  [{_, _}] ->
                      error(type_mismatch);

                  [] ->
                      [{Name, Type},
                       {{Name, Label}, #{type => Type, counter => Counter}}]
              end) orelse ?FUNCTION_NAME(Arg)
    end.


int64(F) when is_integer(F); is_float(F) ->
    <<I:64/integer>> = <<F/float>>,
    I.


float64(I) when is_integer(I) ->
    <<F/float>> = <<I:64/integer>>,
    F.


value(#{name := Name, label := Label}) ->
    case ets:lookup(?MODULE, {Name, Label}) of
        [{_,
          #{type := histogram,
            buckets := Buckets,
            sum := Sum,
            infinity := Infinity,
            counter := Counter}}] ->

            Count = counters:get(Infinity, 1),

            lists:foldl(
              fun
                  ({Ix, LE}, A) ->
                      A#{LE => counters:get(Counter, Ix)}
              end,
              #{sum => float64(atomics:get(Sum, 1)),
                infinity => Count,
                count => Count},
              lists:zip(lists:seq(1, length(Buckets)), Buckets));

        [{_, #{type := _Type, counter := Counter}}] ->
            counters:get(Counter, 1);

        [] ->
            error(badarg, [#{name => Name, label => Label}])
    end.


info(#{name := _, label := _} = Arg) ->
    op(Arg#{apply => fun (Counter) -> counters:info(Counter) end}).


op(#{name := Name, label := Label, apply := Apply}) ->
    case ets:lookup(?MODULE, {Name, Label}) of
        [{_, #{counter := Counter}}] ->
            Apply(Counter);

        [] ->
            error(badarg, [#{name => Name, label => Label}])
    end.


with_name(Name) ->
    select(
      ets:fun2ms(
        fun
            ({{FoundName, Label}, _}) when (Name == FoundName) ->
                {FoundName, Label}
        end)).


select(Pattern) ->
    ets:select(?MODULE, Pattern).


add(#{ix := Ix, value := Value}) ->
    fun
        (Counter) ->
            counters:add(Counter, Ix, Value)
    end.


sub(#{ix := Ix, value := Value}) ->
    fun
        (Counter) ->
            counters:sub(Counter, Ix, Value)
    end.


all() ->
    ets:foldl(
      fun
          ({{Name, Label},
            #{type := Type, counter := Counter}}, A) ->
              [#{name => Name,
                 label => Label,
                 type => Type,
                 value => counters:get(Counter, 1)} | A];

          (_, A) ->
              A
      end,
      [],
      ?MODULE).


exposition(Type) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Type}).


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ?MODULE = ets:new(?MODULE, [ordered_set, named_table, public]),
    {ok, ready, #{}, nei(collect)}.


callback_mode() ->
    handle_event_function.


handle_event({call, From}, {exposition, MediaType}, _, Data) ->
    case Data of
        #{MediaType := Representation} ->
            {keep_state_and_data, {reply, From, Representation}};

        #{} ->
            {keep_state_and_data, {reply, From, {error, not_found}}}
    end;

handle_event(internal, collect, _, _) ->
    {keep_state_and_data,
     lists:foldl(
       fun
           (MediaType, A) ->
               [nei({collect, MediaType}) | A]
       end,
       [generic_timeout(collect)],
       media_types())};

handle_event(internal, {collect, MediaType}, _, Data) ->
    {keep_state, Data#{MediaType => as(MediaType)}};

handle_event({timeout, _}, collect, _, _) ->
    {keep_state_and_data, nei(collect)}.


nei(Event) ->
    {next_event, internal, Event}.


generic_timeout(Name) ->
    {{timeout, Name}, metrics_config:timeout(Name), Name}.


media_types() ->
    [<<"text/plain; version=0.0.4">>].


as(<<"text/plain; version=0.0.4">>) ->
    ets:foldl(
      fun
          ({{Name, Label},
            #{type := histogram,
              buckets := Buckets,
              sum := Sum,
              infinity := Infinity,
              counter := Counter}},
           A) ->
              Count = PlusInf = counters:get(Infinity, 1),

              [A,
               lists:map(
                 fun
                     ({Ix, LE}) ->
                         [nlv(#{name => Name,
                                suffix => bucket,
                                label => Label#{le => LE},
                                value => counters:get(Counter, Ix)})]
                 end,
                 lists:zip(lists:seq(1, length(Buckets)), Buckets)),

               nlv(#{name => Name,
                     suffix => bucket,
                     label => Label#{le => "+Inf"},
                     value => PlusInf}),
               nlv(#{name => Name,
                     suffix => count,
                     label => Label,
                     value => Count}),
               nlv(#{name => Name,
                     suffix => sum,
                     label => Label,
                     value => float64(atomics:get(Sum, 1))})];


          ({{Name, Label}, #{type := Type, counter := Counter}}, A)
            when Type == counter;
                 Type == gauge ->
              [A, nlv(#{name => Name,
                        label => Label,
                        value => counters:get(Counter, 1)})];


          ({Name, Type}, A) ->
              [A, io_lib:format("# TYPE ~p ~p~n", [Name, Type])]
      end,
      [],
      ?MODULE).


nlv(#{name := Name, label := Label, value := Value} = Arg) ->
    [case maps:find(suffix, Arg) of
         {ok, Suffix} ->
             io_lib:format("~p_~p", [Name, Suffix]);

         error ->
             io_lib:format("~p", [Name])
     end,
     "{",
     lists:join(
       ",",
       lists:map(
         fun
             ({K, V}) ->
                 io_lib:format("~p=\"~p\"", [K, V])
         end,
         lists:sort(maps:to_list(Label)))),
     "} ",
     io_lib:format("~p", [Value]),
     "\n"].
