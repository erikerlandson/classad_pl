:- module(classad_eval,
          [eval/3            % eval(+Expr, +Context, -Result)
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).

:- use_module(classad_parser).

:- op(1, fx, user:as_expr).

eval(as_expr S, C, R) :- parse(S, E), eval(E, C, R), !.

eval(E, C, R) :- functor(C, '[classad]', 1), eval(E, [C], R), !.
eval(E, C, R) :- is_list(C), forall(member(X, C), functor(X, '[classad]', 1)), ev([C,E], [_,R]), !.

% These all evaluate as themselves, independent of any context.
% Declare these first.
ev([C, error], [C, error]).
ev([C, undefined], [C, undefined]).
ev([C, true], [C, true]).
ev([C, false], [C, false]).
ev([C, E], [C, E]) :- number(E).
ev([C, E], [C, E]) :- functor(E, '[str]', 1).
ev([C, E], [C, E]) :- functor(E, '[classad]', 1).

% a list evaluates by evaluating each of its elements:
pair(A, B, [A,B]).
ev([C, E], [C, R]) :- is_list(E), maplist(pair(C), E, T), maplist(ev, T, RT), maplist(nth(2), RT, R).

% other reserved atoms are intercepted above as atomic expressions.
is_var(A) :- atom(A), A \= 'parent'.

% evaluating a variable:
ev([[], V], [[],undefined]) :- is_var(V).
ev([[C|P], V], R) :- is_var(V), '[classad]'(M)=C, ((get_assoc(V, M, E), ev([[C|P], E], R)) ; ev([P, V], R)).

% parent
ev([[], parent], [[], undefined]).
ev([[C|[]], parent], [[], undefined]).
ev([[C|[CP|CR]], parent], [CR, CP]).

% select op
ev([C, '[sel]'(SE, V)], R) :- (is_var(V) ; V=parent), ev([C, SE], [SC, SR]), functor(SR, '[classad]', 1), ev([[SR|SC], V], R).

% This is a catchall - has to be declared last.
% TODO: consider some other special error value for this,
% or perhaps throwing an exception.
ev([C, _], [C, error]).
