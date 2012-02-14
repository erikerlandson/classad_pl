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

% utility for constructing pairs
pair(A, B, [A,B]).

% atomic expressions:
% I am ordering these so that most commonly expected literals
% get tested first
atomic_expr(N) :- number(N).
atomic_expr('[str]'(_)).
atomic_expr(true).
atomic_expr(false).
atomic_expr(undefined).
atomic_expr(error).
atomic_expr('[classad]'(_)).

variable(V) :- atom(V), V \= [], V \= parent, \+atomic_expr(V).

% Atomic expressions evaluate as themselves, independent of any context.
% Attempt to match these first
ev([C, E], [C, E]) :- atomic_expr(E).

% 'parent' is an atom, but does not evaluate as atomic
% parent evaluates to parent classad, and also pops up context
ev([[], parent], [[], undefined]).
ev([[_C|[]], parent], [[], undefined]).
ev([[_C|[CP|CR]], parent], [CR, CP]).

% evaluating a variable:
ev([[], V], [[],undefined]) :- variable(V).
ev([[C|P], V], R) :- variable(V), '[classad]'(M)=C, ((get_assoc(V, M, E), ev([[C|P], E], R)) ; ev([P, V], R)).

% select op
% here we know from grammar that 'V' is variable, or 'parent'
ev([C, '[sel]'(SE, V)], R) :- ev([C, SE], [SC, SR]), functor(SR, '[classad]', 1), ev([[SR|SC], V], R).

% a list evaluates by evaluating each of its elements:
% match this prior to atom/var below, because '[]' is considered an atom.
ev([C, E], [C, R]) :- is_list(E), maplist(pair(C), E, T), maplist(ev, T, RT), maplist(nth(2), RT, R).

% This is a catchall - has to be declared last.
% TODO: consider some other special error value for this,
% or perhaps throwing an exception.
ev([C, _], [C, error]).
