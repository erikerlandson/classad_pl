:- module(classad_unparse, [
          unparse/1,
          unparse/2
          ]).

unparse(Expr) :- unparse(Expr, []).

unparse(Expr, Args) :- up(0, Args, Expr).

up(_Lev, _Args, N) :- integer(N), format("~d", [N]).

up(_Lev, _Args, N) :- float(N), format("~6e", [N]).

up(_Lev, _Args, '[str]'(S)) :- format("\"~a\"", [S]).

up(Lev, Args, L) :- is_list(L), format("{"), uphead(Lev, Args, L), format("}").
uphead(_Lev, _Args, []).
uphead(Lev, Args, [H|R]) :- up(Lev, Args, H), uprest(Lev, Args, R).
uprest(_Lev, _Args, []).
uprest(Lev, Args, [H|R]) :- format(","), up(Lev, Args, H), uprest(Lev, Args, R).
