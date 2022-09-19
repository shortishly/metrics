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


-module(metrics_app).


-behaviour(application).
-export([prep_stop/1]).
-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
    _ = ets:new(metrics, [ordered_set, named_table, public]),

    {ok, Sup} = metrics_sup:start_link(),

    case metrics_config:enabled(http) of
        false ->
            {ok, Sup};

        true ->
            {ok, _} = cowboy:start_clear(
                        http,
                        [{port, metrics_config:http(port)}],
                        #{env => #{dispatch => dispatch()}}),
            {ok, Sup, [http]}
    end.


dispatch() ->
    cowboy_router:compile([{'_', [{"/metrics", metrics_exposition_h, []}]}]).


prep_stop([]) ->
    ok;

prep_stop([Ref]) ->
    ok = cowboy:stop_listener(Ref),
    [].


stop(_State) ->
    ets:delete(metrics),
    ok.
