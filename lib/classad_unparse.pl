:- module(classad_unparse, [
          unparse/1,
          unparse/2
          ]).

:- use_module('classad_reltime_parser').

unparse(Expr) :- unparse(Expr, []).

unparse(Expr, Args) :- up(0, Args, Expr).

up(_Lev, _Args, error) :- format("error").
up(_Lev, _Args, undefined) :- format("undefined").
up(_Lev, _Args, true) :- format("true").
up(_Lev, _Args, false) :- format("false").

up(_Lev, _Args, N) :- integer(N), format("~d", [N]).

up(_Lev, _Args, N) :- float(N), format("~6e", [N]).

up(_Lev, _Args, '[str]'(S)) :- format("\"~a\"", [S]).

up(_Lev, _Args, '[reltime]'(S)) :- format("reltime(\""), unparse_reltime(S), format("\")").

up(_Lev, _Args, '[abstime]'(S, Z)) :- stamp_date_time(S, DT, Z), format_time(atom(T), "%FT%T.%3f%z", DT), format("reltime(\"~a\")", T).

up(Lev, Args, L) :- is_list(L), format("{"), uphead(Lev, Args, L), format("}").
uphead(_Lev, _Args, []).
uphead(Lev, Args, [H|R]) :- up(Lev, Args, H), uprest(Lev, Args, R).
uprest(_Lev, _Args, []).
uprest(Lev, Args, [H|R]) :- format(","), up(Lev, Args, H), uprest(Lev, Args, R).

