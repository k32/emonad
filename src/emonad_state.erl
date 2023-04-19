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

%% @doc Combination of state and cont monads
-module(emonad_state).

-behavior(emonad).

%% API:
-export([run_state/2, feed/3, get_state/0, put_state/1, consume/0]).

%% behavior callbacks:
-export([bind/2, return/1, nomatch/1]).

-export_type([ret/3, t/3]).

%%================================================================================
%% Type declarations
%%================================================================================

-type ret(Input, State, Result) ::
        {done, State, Result} |
        {more, State, fun((Input, State) -> ret(Input, State, Result))}.

-type t(Input, State, Result) :: fun((State) -> ret(Input, State, Result)).

%%================================================================================
%% API
%%================================================================================

-spec run_state(t(Input, State, Result), State) ->
        ret(Input, State, Result).
run_state(M, State) ->
  M(State).

-spec feed(fun((Input, State) -> ret(Input, State, Result)), State, Input) ->
        ret(Input, State, Result).
feed(Cont, State, Input) ->
  Cont(State, Input).

-spec get_state() -> t(_Input, State, State).
get_state() ->
  fun(State) ->
      {done, State, State}
  end.

-spec put_state(State) -> t(_Input, State, undefined).
put_state(State) ->
  fun(_) ->
      {done, State, undefined}
  end.

-spec consume() -> t(Input, _State, Input).
consume() ->
  fun(State) ->
      {more, State,
       fun(State1, Input) ->
           {done, State1, Input}
       end}
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

return(A) ->
  fun(State) ->
      {done, State, A}
  end.

bind(Prev, Next) ->
  fun(State0) ->
      case Prev(State0) of
        {done, State, Result} ->
          run_state(Next(Result), State);
        {more, State, Cont} ->
          {more, State,
           fun(State1, Input) ->
               {done, State2, Result} = feed(Cont, State1, Input),
               run_state(Next(Result), State2)
           end}
      end
  end.

nomatch(A) ->
  throw({nomatch, A}).
