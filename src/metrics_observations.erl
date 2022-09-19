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


-module(metrics_observations).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([start_link/1]).


start_link(Args) ->
    gen_statem:start_link(?MODULE, [Args], []).


init([#{} = Args]) ->
    {ok, ready, Args, metrics_statem:nei(collect)}.


handle_event({timeout, _}, collect, _, _) ->
    {keep_state_and_data, metrics_statem:nei(collect)};

handle_event(internal,
             collect,
             _,
             #{observation := Type, periodic := Periodic}) ->
    observation(Type),
    {keep_state_and_data, {{timeout, periodic}, Periodic, collect}}.


callback_mode() ->
    handle_event_function.


observation(memory = Type) ->
    ?FUNCTION_NAME(beam, Type, erlang:memory());

observation(processes = Type) ->
    Processes = processes(),
    ?FUNCTION_NAME(
       beam,
       Type,
       lists:foldl(
         fun
             (Pid, A) ->
                 case erlang:process_info(Pid, status) of
                     {status, Status} ->
                         orddict:update_counter(Status, 1, A);

                     undefined ->
                         A
                 end
         end,
         [{total, length(Processes)}],
      Processes));

observation(ports = Type) ->
    ?FUNCTION_NAME(beam, Type, recon:port_types()).


observation(Prefix, Type, Observations) ->
    ?FUNCTION_NAME(metrics_util:snake_case([Prefix, Type]), Observations).


observation(Name, Observations) ->
    metrics:gauge(
      lists:map(
        fun
            ({Key, Value}) ->
                #{name => Name, label => #{type => Key}, value => Value}
        end,
        Observations)).
