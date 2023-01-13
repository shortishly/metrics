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


-module(metrics_prom_tests).


-include_lib("eunit/include/eunit.hrl").


nlv_test_() ->
    {setup,
     fun
         () ->
             application:set_env(metrics, exposition_instance, "instance"),
             application:set_env(metrics, exposition_job, "job")
     end,
     fun
         (_) ->
             application:unset_env(metrics, exposition_instance),
             application:unset_env(metrics, exposition_job)
     end,
     [?_assertEqual(
         <<"abc{instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc, label => #{}, value => 123}))),

      ?_assertEqual(
         <<"abc{instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc, value => 123}))),

      ?_assertEqual(
         <<"abc{a=\"1.0\",instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc,
                              label => #{a => 1.0},
                              value => 123}))),

      ?_assertEqual(
         <<"abc{a=\"b\",instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc,
                              label => #{a => b},
                              value => 123}))),

      ?_assertEqual(
         <<"abc{a=\"def\",instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc,
                              label => #{a => "def"},
                              value => 123}))),

      ?_assertEqual(
         <<"abc{a=\"def\",instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc,
                              label => #{a => <<"def">>},
                              value => 123}))),
      ?_assertEqual(
         <<"abc{a=\"1\",instance=\"instance\",job=\"job\"} 123\n">>,
         iolist_to_binary(
           metrics_prom:nlv(#{name => abc,
                              label => #{a => 1},
                              value => 123})))]}.
