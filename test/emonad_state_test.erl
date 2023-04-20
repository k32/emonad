-module(emonad_state_test).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, emonad}).

-import(emonad_state, [return/1, get_state/0, put_state/1, consume/0, run_state/2, feed/3]).

state_01_test() ->
  M = [do/emonad_state ||
        A <- return(1),
        B <- return(2),
        return(A + B)],
  ?assertMatch({done, state, 3}, run_state(M, state)).

state_02_test() ->
  M = [do/emonad_state ||
        A <- return(1),
        A <- return(2),
        return(A + A)],
  ?assertThrow({nomatch, _}, run_state(M, state)).

state_03_test() ->
  M = [do/emonad_state ||
        A <- return(1),
        B <- [do/emonad_state ||
               _ <- return(42),
               return(2)],
        return(A + B)],
  ?assertMatch({done, state1, 3}, run_state(M, state1)).

state_get_01_test() ->
  M = [do/emonad_state ||
        S <- get_state(),
        return(S)],
  ?assertMatch({done, state1, state1}, run_state(M, state1)).


state_get_put_test() ->
  M = [do/emonad_state ||
        S1 <- get_state(),
        put_state(state2),
        S2 <- get_state(),
        put_state(state3),
        S3 <- get_state(),
        return({S1, S2, S3})],
  ?assertMatch({done, state3, {state1, state2, state3}}, run_state(M, state1)).


state_get_put_nested_test() ->
  M = [do/emonad_state ||
        put_state(state2),
        Val <- [do/emonad_state ||
                 S2 <- get_state(),
                 put_state(state3),
                 return(S2)],
        return(Val)],
  ?assertMatch({done, state3, state2}, run_state(M, state1)).

wait_input_00_test() ->
  M = [do/emonad_state ||
        Val <- consume(),
        return(Val)],
  {more, state1, Cont} = run_state(M, state1),
  {done, state2, input1} = feed(Cont, state2, input1).

wait_input_01_test() ->
  M = [do/emonad_state ||
        put_state(state2),
        Val <- consume(),
        return(Val)],
  {more, state2, Cont} = run_state(M, state1),
  {done, state4, input1} = feed(Cont, state4, input1).

wait_input_02_test() ->
  M = [do/emonad_state ||
        put_state(state2),
        Val1 <- consume(),
        State <- get_state(),
        put_state(state5),
        Val2 <- consume(),
        return({State, Val1, Val2})],
  {more, state2, Cont1} = run_state(M, state1),
  {more, state5, Cont2} = feed(Cont1, state4, input1),
  {done, state6, {state4, input1, input2}} = feed(Cont2, state6, input2).

wait_input_03_test() ->
  M = [do/emonad_state ||
        #{a := A} <- consume(),
        #{b := B} <- consume(),
        return({pair, A, B})],
  {more, state1, Cont1} = run_state(M, state1),
  ?assertThrow({nomatch, _}, feed(Cont1, state1, input1)),
  {more, state1, Cont2} = feed(Cont1, state1, #{a => 1}),
  {done, state1, {pair, 1, 2}} = feed(Cont2, state1, #{b => 2}).

wait_input_04_test() ->
  M = [do/emonad_state ||
        #{a := A} <- consume(),
        #{b := A} <- consume(),
        return({pair, A})],
  {more, state1, Cont1} = run_state(M, state1),
  %% Prime the parser, so `A' variable gets bound:
  {more, state1, Cont2} = feed(Cont1, state1, #{a => 1}),
  %% Try to push a value that doesn't match with `A':
  ?assertThrow({nomatch, _}, feed(Cont2, state1, #{b => 2})),
  %% Push a valid value:
  {done, state1, {pair, 1}} = feed(Cont2, state1, #{b => 1}).

wait_input_nested_00_test() ->
  M = [do/emonad_state ||
        Ret <- [do/emonad_state ||
                 Val <- consume(),
                 return(Val)],
        return(Ret)],
  {more, state1, Cont} = run_state(M, state1),
  {done, state2, input1} = feed(Cont, state2, input1).

wait_input_nested_01_test() ->
  M = [do/emonad_state ||
        put_state(state2), %% TODO: remove
        Ret <- [do/emonad_state ||
                 Val <- consume(),
                 return(Val)],
        return(Ret)],
  {more, state2, Cont} = run_state(M, state1),
  {done, state2, input1} = feed(Cont, state2, input1).

wait_input_nested_02_test() ->
  M = [do/emonad_state ||
        put_state(state2),
        Val1 <- consume(),
        Ret <- [do/emonad_state ||
                 State <- get_state(),
                 put_state(state5),
                 Val2 <- consume(),
                 return({State, Val1, Val2})],
        return(Ret)],
  {more, state2, Cont1} = run_state(M, state1),
  {more, state5, Cont2} = feed(Cont1, state4, input1),
  {done, state6, {state4, input1, input2}} = feed(Cont2, state6, input2).

wait_input_nested_03_test() ->
  M = [do/emonad_state ||
        Ret <- [do/emonad_state ||
                 Ret1 <- [do/emonad_state ||
                           Val <- consume(),
                           return({wrap1, Val})],
                 return({wrap2, Ret1})],
        return({wrap3, Ret})],
  {more, state1, Cont1} = run_state(M, state1),
  {done, state2,
   {wrap3, {wrap2, {wrap1, input1}}}} = feed(Cont1, state2, input1).
