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


-module(metrics_prom).


-export([callback_mode/0]).
-export([exposition/1]).
-export([handle_event/4]).
-export([init/1]).
-export([nlv/1]).
-export([start_link/0]).
-import(metrics_statem, [generic_timeout/1]).
-import(metrics_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


ji(Label) ->
    maps:merge(
      Label,
      lists:foldl(
        fun
            (Name, A) ->
                A#{Name => metrics_config:exposition(Name)}
        end,
        #{},
        [job, instance])).


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


exposition(MediaType) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, MediaType}).


init([]) ->
    {ok, unready, #{}}.


callback_mode() ->
    handle_event_function.


handle_event({call, _}, {exposition, _}, unready, _) ->
    {keep_state_and_data, [postpone, nei(collect)]};

handle_event({call, From}, {exposition, MediaType}, ready, Data) ->
    case Data of
        #{MediaType := Representation} ->
            {keep_state_and_data, {reply, From, Representation}};

        #{} ->
            {keep_state_and_data, {reply, From, {error, not_found}}}
    end;

handle_event(internal, collect, _, Data) ->
    {next_state,
     ready,
     Data,
     lists:foldl(
       fun
           (MediaType, A) ->
               [nei({collect, MediaType}) | A]
       end,
       [generic_timeout(#{name => collect})],
       media_types())};

handle_event(internal, {collect, MediaType}, _, Data) ->
    {keep_state, Data#{MediaType => as(MediaType)}};

handle_event({timeout, _}, collect, _, _) ->
    {keep_state_and_data, nei(collect)}.


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
                     value => metrics:float64(atomics:get(Sum, 1))})];


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
      metrics).


nlv(#{name := Name, value := Value} = Arg) ->
    [case maps:find(suffix, Arg) of
         {ok, Suffix} ->
             io_lib:format("~p_~p", [Name, Suffix]);

         error ->
             io_lib:format("~p", [Name])
     end,
     l(Arg),
     " ",
     io_lib:format("~p", [Value]),
     "\n"].


l(#{label := Label}) ->
    ["{",
     lists:join(
       ",",
       lists:map(
         fun
             ({K, V}) when is_list(V); is_binary(V) ->
                 io_lib:format("~p=\"~s\"", [K, V]);

             ({K, V}) ->
                 io_lib:format("~p=\"~p\"", [K, V])
         end,
         lists:sort(maps:to_list(ji(Label))))),
     "}"];

l(#{} = Arg) ->
    ?FUNCTION_NAME(Arg#{label => #{}}).
