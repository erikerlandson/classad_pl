:- module(classad_unparse, [
          unparse/1,
          unparse/2
          ]).

:- use_module('classad_reltime_parser').

unparse(Expr) :- unparse(Expr, []), !.

unparse(Expr, Args) :- up(0, Args, Expr), !.

:- discontiguous(up/3).

up(_Lev, _Args, error) :- format("error").
up(_Lev, _Args, undefined) :- format("undefined").
up(_Lev, _Args, true) :- format("true").
up(_Lev, _Args, false) :- format("false").

up(_Lev, _Args, N) :- integer(N), format("~d", [N]).

up(_Lev, _Args, N) :- float(N), format("~6e", [N]).

up(_Lev, _Args, '[str]'(S)) :- format("\"~a\"", [S]).

up(_Lev, _Args, '[reltime]'(S)) :- format("reltime(\""), unparse_reltime(S), format("\")").

up(_Lev, _Args, '[abstime]'(S, Z)) :- stamp_date_time(S, DT, Z), format_time(atom(T), "%FT%T.%3f%z", DT), format("reltime(\"~a\")", T).

up(Lev, Args, '[classad]'(M)) :- 
    assoc_to_list(M, ML),
    Lev1 is Lev+1,
    format("["),
    up_ca_head(Lev1, Args, ML),
    nlindent(Lev, Args),
    format("]"),
    ((Lev=<0,member(nl, Args)) -> format("\n") ; true).

up_ca_head(_Lev, _Args, []).
up_ca_head(Lev, Args, ['-'(V,E)|R]) :-
    nlindent(Lev, Args),
    up(Lev, Args, V), format("="), up(Lev, Args, E), 
    up_ca_rest(Lev, Args, R).

up_ca_rest(_Lev, _Args, []).
up_ca_rest(Lev, Args, ['-'(V,E)|R]) :- 
    format(";"), nlindent(Lev, Args), 
    up(Lev, Args, V), format("="), up(Lev, Args, E), 
    up_ca_rest(Lev, Args, R).

nlindent(Lev, Args) :-
    (member(nl, Args) -> 
        format("\n"),
        (member(indent(T), Args) -> 
            (Tab is T * Lev, tab(Tab)) ; true)
    ; true).
    

up(Lev, Args, L) :- is_list(L), format("{"), uphead(Lev, Args, L), format("}").

uphead(_Lev, _Args, []).
uphead(Lev, Args, [H|R]) :- up(Lev, Args, H), uprest(Lev, Args, R).
uprest(_Lev, _Args, []).
uprest(Lev, Args, [H|R]) :- format(","), up(Lev, Args, H), uprest(Lev, Args, R).

% define this after unparse for list, because [] is an atom:
up(_Lev, _Args, V) :- atom(V), format("~a", [V]).

% '?:' trinary op
up(Lev, Args, '?:'(TE, LE, RE)) :- format("("), up(Lev, Args, TE), format("?"), up(Lev, Args, LE), format(":"), up(Lev, Args, RE), format(")").

% '[]' indexing op
up(Lev, Args, '[]'(BE, IE)) :- up(Lev, Args, BE), format("["), up(Lev, Args, IE), format("]").

% binary ops
up(Lev, Args, E) :- E=..[BO, LSE, RSE], opname(BO, BN), format("("), up(Lev, Args, LSE), format("~a",[BN]), up(Lev, Args, RSE), format(")").

% unary ops
up(Lev, Args, E) :- E=..[UO, SE], unaryop(UO), opname(UO, UN), format("("), format("~a",[UN]), up(Lev, Args, SE), format(")").

opname('[sel]', '.').
opname(BO, BO).

unaryop('!').
unaryop('~').
unaryop('+').
unaryop('-').

% anything not matched above is function call:
up(Lev, Args, E) :- E=..[FN, AL], atom(FN), is_list(AL), format("~a(", [FN]), uphead(Lev, Args, AL), format(")").

% something went off the rails:
up(_, _, E) :- format("unparse FAILED on: ~q\,", [E]).
up(_, _, _) :- format("unparse FAILED\n").
