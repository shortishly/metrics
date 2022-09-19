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


-module(metrics_config).


-export([enabled/1]).
-export([exposition/1]).
-export([http/1]).
-export([timeout/1]).


timeout(#{name := Name, default := Default}) ->
    envy(to_integer,
         metrics_util:snake_case([Name, timeout]),
         Default);

timeout(#{name := _} = Arg) ->
    ?FUNCTION_NAME(Arg#{default => timer:minutes(1)}).


enabled(Name) ->
    envy(to_boolean,
         metrics_util:snake_case([Name, ?FUNCTION_NAME]),
         false).


http(port = Name) ->
    envy(to_integer,
         metrics_util:snake_case([?FUNCTION_NAME, Name]),
         9100).


exposition(instance = Name) ->
    envy(to_list,
         metrics_util:snake_case([?FUNCTION_NAME, Name]),
         begin
             {ok, Hostname} = inet:gethostname(),
             Hostname
         end);

exposition(job = Name) ->
    envy(to_list,
         metrics_util:snake_case([?FUNCTION_NAME, Name]),
         job()).


job() ->
    case file:consult("/releases/RELEASES") of
        {ok, [[{release, Release, _, _, _, _}]]} ->
            Release;

        {error, _} ->
            application()
    end.


application() ->
    case application:get_application() of
        {ok, Application} ->
            Application;

        undefined ->
            undefined
    end.


envy(To, Name, Default) ->
    envy:To(metrics, Name, default(Default)).


default(Default) ->
    %% Enable all configuration to be overriden by OS environment
    %% variables, very useful for Docker.
    [os_env, app_env, {default, Default}].
