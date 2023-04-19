%%--------------------------------------------------------------------
%% Copyright (c) 2023 k32. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(emonad_trans_test).

-compile(nowarn_export_all).
-compile(export_all).

-compile({parse_transform, emonad}).

-include_lib("eunit/include/eunit.hrl").

-import(emonad_list, [return/1]).

trans_00_test() ->
  Result = [do/emonad_list ||
             A <- [a, b],
             return(A)],
  ?assertMatch([a, b], Result).

trans_01_test() ->
  Result = [do/emonad_list ||
             A <- [a, b],
             B <- [0, 1],
             return({A, B})],
  ?assertMatch([{a, 0}, {a, 1}, {b, 0}, {b, 1}], Result).

trans_02_test() ->
  Result = [do/emonad_list ||
             A <- [a, b],
             B <- [1, 2],
             X = B - 1,
             return({A, X})],
  ?assertMatch([{a, 0}, {a, 1}, {b, 0}, {b, 1}], Result).

trans_03_test() ->
  Result = [do/emonad_list ||
             X = 1,
             Y = 2,
             return({X, Y})],
  ?assertMatch([{1, 2}], Result).

trans_04_lc_inside_test() ->
  Result = [do/emonad_list ||
             X <- [A * 2 || A <- [1, 2]],
             return(X)],
  ?assertMatch([2, 4], Result).

trans_05_branching_test() ->
  Result = [do/emonad_list ||
             X <- [1, 2],
             case X of
               1 -> return({X, one});
               2 -> return({X, two})
             end],
  ?assertMatch([{1, one}, {2, two}], Result).

trans_06_nesting_test() ->
  Result = [do/emonad_list ||
             X <- [1, 2],
             case X of
               1 -> [do/emonad_list ||
                      Y <- [1, 2],
                      return({X, Y})];
               2 -> return({X, two})
             end],
  ?assertMatch([{1, 1}, {1, 2}, {2, two}], Result).

trans_07_nesting_test() ->
  Result = [do/emonad_list ||
             X <- [1, 2],
             [do/emonad_list ||
               Y <- [1, 2],
               return({X, Y})]],
  ?assertMatch([{1, 1}, {1, 2}, {2, 1}, {2, 2}], Result).

trans_08_nesting_test() ->
  Result = [do/emonad_list ||
             X = [do/emonad_list ||
                   Y <- [1, 2],
                   return(Y + 1)],
             Y <- [a, b],
             return({Y, X})],
  ?assertMatch([{a, [2, 3]}, {b, [2, 3]}], Result).

trans_09_nesting_test() ->
  Result = [do/emonad_list ||
             X <- [do/emonad_list ||
                    Y <- [1],
                    return(Y)],
             Y <- [a, b],
             return({X, Y})],
  ?assertMatch([{1, a}, {1, b}], Result).

trans_10_nomatch_test() ->
  Result = [do/emonad_list ||
             A <- [a, b],
             A <- [a, b],
             return(A)],
  ?assertMatch([a, {nomatch, b}, {nomatch, a}, b], Result).

references() ->
    _Ref = fun(A) ->
               case A of
                 1 -> A, A;
                 _ -> A, A, A
               end
           end,
  ok.
