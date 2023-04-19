-module(emonad_maybe_test).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, emonad}).

-import(emonad_maybe, [return/1]).

maybe_01_test() ->
  Result = [do/emonad_maybe ||
             A <- return(1),
             B <- return(2),
             return(A + B)],
  ?assertMatch({just, 3}, Result).

maybe_02_test() ->
  Result = [do/emonad_maybe ||
             A <- return(1),
             A <- return(2),
             return(A + A)],
  ?assertMatch(nothing, Result).

maybe_03_test() ->
  Result = [do/emonad_maybe ||
             A <- return(1),
             B <- [do/emonad_maybe ||
                    _ <- return(42),
                    return(2)],
             return(A + B)],
  ?assertMatch({just, 3}, Result).
