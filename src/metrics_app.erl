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
-export([ensure_all_started/0]).
-export([start/2]).
-export([stop/1]).


ensure_all_started() ->
    application:ensure_all_started(metrics).


start(_Type, _Args) ->
    {ok, _} = cowboy:start_clear(
                http,
                [{port, 8080}],
                #{env => #{dispatch => dispatch()}}),
    metrics_sup:start_link().


dispatch() ->
    cowboy_router:compile([{'_', [{"/metrics", metrics_exposition_h, []}]}]).


prep_stop(State) ->
    ok = cowboy:stop_listener(http),
    State.


stop(_State) ->
    ok.
