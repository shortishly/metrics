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
-export([counter/1]).
-export([delete/1]).
-export([float64/1]).
-export([gauge/1]).
-export([histogram/1]).
-export([info/1]).
-export([start/0]).
-export([value/1]).
-export([with_name/1]).
-include_lib("stdlib/include/ms_transform.hrl").


-type histogram_value() :: #{number() := non_neg_integer(),
                             count := non_neg_integer(),
                             infinity := non_neg_integer(),
                             sum := number()}.

-type counter_value() :: integer().

-type gauge_value() :: integer().


-spec histogram(#{name := atom(),
                  label := map(),
                  value := number(),
                  buckets => [number()]}) -> ok | no_return().

histogram(#{name := Name, label := Label, value := Value} = Arg) ->
    NL = {Name, Label},

    case ets:lookup(?MODULE, NL) of
        [{_, #{type := histogram,
               buckets := Buckets,
               sum := Sum,
               infinity := Infinity,
               counter := Counter}}] ->
            ok = histogram_update(Counter, Buckets, Infinity, Sum, Value);

        [{_, _}] ->
            error(type_mismatch, [Arg]);

        [] ->
            Buckets = lists:usort(maps:get(buckets, Arg)),
            Counter = counters:new(length(Buckets), []),
            Infinity = counters:new(1, []),
            Sum = atomics:new(1, [{signed, false}]),
            ok = histogram_update(Counter, Buckets, Infinity, Sum, Value),

            Histogram = {NL,
                         #{type => histogram,
                           buckets => Buckets,
                           sum => Sum,
                           infinity => Infinity,
                           counter => Counter}},

            ets:insert_new(
              ?MODULE,
              case ets:lookup(?MODULE, Name) of
                  [{_, histogram}] ->
                      Histogram;

                  [{_, _}] ->
                      error(type_mismatch, [Arg]);

                  [] ->
                      [{Name, histogram}, Histogram]
              end) orelse ?FUNCTION_NAME(Arg)
    end;

histogram(Arg) when not(is_map_key(label, Arg)) ->
    ?FUNCTION_NAME(Arg#{label => #{}}).


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


gauge(#{name := _, label := _, value := Value} = Arg) ->
    observation(Arg#{type => gauge,
                     op => put(#{ix => 1, value => Value})}),
    ok;

gauge(#{name := _, label := _, delta := Value} = Arg) ->
    observation(Arg#{type => gauge,
                     op => delta(#{ix => 1, value => Value})}),
    ok;

gauge(#{name := _, label := _} = Arg) ->
    ?FUNCTION_NAME(Arg#{delta => 1});

gauge(#{name := _} = Arg) when not(is_map_key(label, Arg)) ->
    ?FUNCTION_NAME(Arg#{label => #{}});

gauge([H | T]) ->
    ok = ?FUNCTION_NAME(H),
    ?FUNCTION_NAME(T);

gauge([]) ->
    ok.


counter(#{name := Name, label := Label, value := Value} = Arg) ->
    case ets:lookup(?MODULE, {Name, Label}) of
        [{_, #{type := counter, counter := Counter}}] ->
            ?FUNCTION_NAME(Arg, Counter, atomics:get(Counter, 1));

        [{_, #{type := _}}] ->
            error(type_mismatch, [Arg]);

        [] ->
            ?FUNCTION_NAME(
               maps:with(
                 [name, label, delta],
                 Arg#{delta => Value}))
    end;

counter(#{name := _, label := _, delta := Value} = Arg) ->
    observation(Arg#{type => counter,
                     op => add(#{ix => 1, value => Value})}),
    ok;

counter(#{name := _, label := _} = Arg) ->
    ?FUNCTION_NAME(Arg#{delta => 1});

counter(#{name := _} = Arg) when not(is_map_key(label, Arg)) ->
    ?FUNCTION_NAME(Arg#{label => #{}});

counter([H | T]) ->
    ok = ?FUNCTION_NAME(H),
    ?FUNCTION_NAME(T);

counter([]) ->
    ok.


counter(#{value := Value} = Arg, Counter, Existing) when Value > Existing ->
    case atomics:compare_exchange(
           Counter,
           1,
           Existing,
           Value) of

        ok ->
            ok;

        Actual ->
            ?FUNCTION_NAME(Arg, Counter, Actual)
    end;

counter(Arg, Counter, Existing) ->
    error(badarg, [Arg, Counter, Existing]).


delete(#{name := Name}) ->
    ets:select_delete(
      ?MODULE,
      ets:fun2ms(
        fun
            ({{FoundName, _}, _}) ->
                Name == FoundName;

            ({FoundName, _}) ->
                Name == FoundName
        end)),
    ok;

delete(Arg) when not(is_map_key(label, Arg)) ->
    ?FUNCTION_NAME(Arg#{label => #{}});

delete([H | T]) ->
    ?FUNCTION_NAME(H),
    ?FUNCTION_NAME(T);

delete([]) ->
    ok.




observation(#{name := Name,
              label := Label,
              type := Type,
              op := Op} = Arg) ->
    NL = {Name, Label},

    case ets:lookup(?MODULE, NL) of
        [{_, #{type := Type, counter := Counter}}] ->
            ok = Op(Counter);

        [{_, _}] ->
            error(type_mismatch, [Arg]);

        [] ->
            Counter = counters:new(1, []),
            ok = Op(Counter),

            ets:insert_new(
              ?MODULE,
              case ets:lookup(?MODULE, Name) of
                  [{_, Type}] ->
                      {NL, #{type => Type, counter => Counter}};

                  [{_, _}] ->
                      error(type_mismatch, [Arg]);

                  [] ->
                      [{Name, Type},
                       {NL, #{type => Type, counter => Counter}}]
              end) orelse ?FUNCTION_NAME(Arg)
    end.


int64(F) when is_integer(F); is_float(F) ->
    <<I:64/integer>> = <<F/float>>,
    I.


float64(I) when is_integer(I) ->
    <<F/float>> = <<I:64/integer>>,
    F.


-type value() :: histogram_value()
               | counter_value()
               | gauge_value().

-spec value(#{name := atom(), label => map()}) -> value().

value(#{name := Name, label := Label} = Arg) ->
    NL = {Name, Label},

    case ets:lookup(?MODULE, NL) of
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
            error(badarg, [Arg])
    end;

value(Arg) when not(is_map_key(label, Arg)) ->
    ?FUNCTION_NAME(Arg#{label => #{}}).


info(#{name := _, label := _} = Arg) ->
    op(Arg#{apply => fun (Counter) -> counters:info(Counter) end});

info(Arg) when not(is_map_key(label, Arg)) ->
    ?FUNCTION_NAME(Arg#{label => #{}}).


op(#{name := Name, label := Label, apply := Apply} = Arg) ->
    NL = {Name, Label},

    case ets:lookup(?MODULE, NL) of
        [{_, #{counter := Counter}}] ->
            Apply(Counter);

        [] ->
            error(badarg, [Arg])
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


put(#{ix := Ix, value := Value}) ->
    fun
        (Counter) ->
            counters:put(Counter, Ix, Value)
    end.


delta(#{value := Value} = Arg) when Value > 0 ->
    add(Arg);

delta(#{value := Value} = Arg) ->
    sub(Arg#{value := abs(Value)}).


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
            #{type := histogram,
              buckets := Buckets,
              sum := Sum,
              infinity := Infinity,
              counter := Counter}}, A0) ->

              Count = PlusInf = counters:get(Infinity, 1),

              lists:foldr(
                fun
                    ({Ix, LE}, A1) ->
                        [#{name => Name,
                           type => histogram,
                           suffix => bucket,
                           less_or_equal => LE,
                           label => Label,
                           value => counters:get(Counter, Ix)} | A1]
                end,
                [#{name => Name,
                   type => histogram,
                   suffix => bucket,
                   less_or_equal => "+Inf",
                   label => Label,
                   value => PlusInf},

                 #{name => Name,
                   type => histogram,
                   suffix => count,
                   label => Label,
                   value => Count},

                 #{name => Name,
                   type => histogram,
                   suffix => sum,
                   label => Label,
                   value => float64(atomics:get(Sum, 1))} | A0],
                lists:zip(lists:seq(1, length(Buckets)), Buckets));

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


start() ->
    application:ensure_all_started(metrics).
