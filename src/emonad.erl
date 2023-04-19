-module(emonad).

-export(['>>'/3, fmap/3, join/2]).

-export([parse_transform/2]).

-callback bind(m(A), fun((A) -> m(B))) -> m(B).

-callback return(A) -> m(A).

-callback nomatch(_A) -> m(_B).

-optional_callbacks([nomatch/1]).

-type m(_A) :: term().

-compile({parse_transform, erlang_qq}).

-include_lib("erlang_qq/include/erlang_qq.hrl").

%%================================================================================
%% Monad stuff
%%================================================================================

-spec '>>'(module(), m(_A), m(B)) -> m(B).
'>>'(M, A, B) ->
  M:bind(A, fun(_) -> B end).

-spec fmap(module(), m(A), fun((A) -> B)) -> m(B).
fmap(M, A, F) ->
  M:bind(A, fun(Arg) -> M:return(F(Arg)) end).

-spec join(module(), m(m(A))) -> m(A).
join(M, A) ->
  M:bind(A, fun(A_) -> A_ end).

%%================================================================================
%% Parse transform stuff
%%================================================================================

%-define(debug, true).

-ifdef(debug).
log(Fmt, Args) ->
  io:format(user, Fmt ++ "~n", Args).
-else.
log(_, _) ->
  ok.
-endif.

parse_transform(Forms, _Options) ->
  log("Input forms:~n  ~p", [Forms]),
  normal(Forms).

%%================================================================================
%% Internal functions
%%================================================================================

normal({lc, Loc, {op, _, '/', ?ATOM(_, do), Module__AST = ?ATOM(_, _)}, Forms}) -> %% Magic list comprehension
  Ret = monadic(Module__AST, normal(Forms)),
  log("Monadic at ~p:~n  ~p~nResult:~n  ~p", [Loc, Forms, Ret]),
  Ret;
normal(L) when is_list(L) -> %% Everything else
  lists:map(fun normal/1, L);
normal(T) when is_tuple(T) ->
  list_to_tuple(lists:map(fun normal/1, tuple_to_list(T)));
normal(T) ->
  T.

monadic(_Module, [A]) ->
  A;
monadic(Module__AST, [{generate, Line, Pattern__AST, Body__AST} | Rest]) ->
  Var__AST = gensym(Line),
  '$$'(Module__AST:bind(Body__AST,
                         fun(Var__AST) ->
                             case Var__AST of
                               Pattern__AST -> '$'(monadic(Module__AST, Rest));
                               _            -> Module__AST:nomatch(Var__AST)
                             end
                         end));
monadic(Module__AST, [Match__AST = {match, Line, Pattern__AST, Body__AST} | Rest]) ->
  {block, Line, [normal(Match__AST), monadic(Module__AST, normal(Rest))]};
monadic(Module__AST, [Form__AST | Rest]) ->
  '$$'(Module__AST:bind(Form__AST,
                        fun(_) ->
                            '$'(monadic(Module__AST, normal(Rest)))
                        end)).

gensym(Line) ->
  case get(emonad_gensym) of
    undefined -> N = 0;
    N         -> ok
  end,
  put(emonad_gensym, N + 1),
  {var, Line, list_to_atom("__EmonadTMPVar" ++ integer_to_list(N))}.
