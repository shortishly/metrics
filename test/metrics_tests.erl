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


-module(metrics_tests).


-include_lib("eunit/include/eunit.hrl").


metric_test_() ->
    {foreach, setup(), cleanup(), tests()}.

tests() ->
    Name0 = abc,
    Name1 = cba,
    Name2 = cab,
    LabelA = #{a => 1},
    LabelB = #{b => 1},

    N0LA = #{name => Name0, label => LabelA},
    N1LA = #{name => Name1, label => LabelA},
    N2LA = #{name => Name2, label => LabelA},

    N0LB = #{name => Name0, label => LabelB},

    [?_assertEqual(
        2,
        begin
            metrics:gauge_add(N0LA),
            metrics:gauge_add(N0LA),

            metrics:value(N0LA)
        end),

     ?_assertEqual(
        6,
        begin
            metrics:gauge_add(N0LA),
            metrics:gauge_add(N0LA),
            metrics:gauge_put(N0LA#{value => 6}),

            metrics:value(N0LA)
        end),

     ?_assertEqual(
        #{0.005 => 0,
          0.01 => 0,
          10 => 1,
          count => 1,
          infinity => 1,
          sum => 6.0},
        begin
            metrics:histogram(
              N0LA#{buckets => [0.005, 0.01, 10], value => 6}),
            metrics:value(N0LA)
        end),

     ?_assertEqual(
        #{0.005 => 1,
          0.01 => 1,
          10 => 1,
          count => 1,
          infinity => 1,
          sum => -1.1},
        begin
            metrics:histogram(
              N0LA#{buckets => [0.005, 0.01, 10], value => -1.1}),
            metrics:value(N0LA)
        end),

     ?_assertEqual(
        #{0.005 => 0,
          0.01 => 0,
          10 => 0,
          count => 1,
          infinity => 1,
          sum => 11.23},
        begin
            metrics:histogram(
              N0LA#{buckets => [0.005, 0.01, 10], value => 11.23}),
            metrics:value(N0LA)
        end),

     ?_assertEqual(
        #{0.005 => 1,
          0.01 => 1,
          10 => 1,
          count => 2,
          infinity => 2,
          sum => 10.13},
        begin
            metrics:histogram(
              N0LA#{buckets => [0.005, 0.01, 10], value => 11.23}),
            metrics:histogram(N0LA#{value => -1.1}),
            metrics:value(N0LA)
        end),

     ?_assertEqual(
        {3, 2, 1},
        begin
            metrics:gauge_add(N0LA#{value => 3}),
            metrics:gauge_add(N1LA#{value => 2}),
            metrics:gauge_add(N2LA#{value => 1}),

            {metrics:value(N0LA),
             metrics:value(N1LA),
             metrics:value(N2LA)}
        end),

     ?_assertError(badarg, metrics:value(N0LA)),

     ?_assertError(badarg, metrics:info(N0LA)),

     ?_assertMatch(
        #{size := 1},
        begin
            metrics:gauge_add(N0LA),
            metrics:info(N0LA)
        end),

     ?_assertMatch(
        #{size := 1},
        begin
            metrics:counter_add(N0LA),
            metrics:info(N0LA)
        end),

     ?_assertError(
        type_mismatch,
        begin
            metrics:gauge_add(N0LA),
            metrics:counter_add(N0LA)
        end),

     ?_assertError(
        type_mismatch,
        begin
            metrics:gauge_add(N0LA),
            metrics:counter_add(N0LB)
        end),

     ?_assertError(
        type_mismatch,
        begin
            metrics:counter_add(N0LA),
            metrics:gauge_add(N0LA)
        end),

     ?_assertError(
        type_mismatch,
        begin
            metrics:counter_add(N0LA),
            metrics:gauge_add(N0LB)
        end),

     ?_assertEqual(
        0,
        begin
            metrics:gauge_add(N0LA),
            metrics:gauge_sub(N0LA),

            metrics:value(N0LA)
        end),

     ?_assertEqual(
        -1,
        begin
            metrics:gauge_sub(N0LA),
            metrics:value(N0LA)
        end),

     ?_assertEqual(
        2,
        begin
            metrics:counter_add(N0LA),
            metrics:counter_add(N0LA),

            metrics:value(N0LA)
        end)].


setup() ->
    fun () ->
            {ok, _} = application:ensure_all_started(metrics)
    end.


cleanup() ->
    fun
        (_) ->
            application:stop(metrics)
    end.
