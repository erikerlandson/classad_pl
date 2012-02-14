:- module(classad_eval,
          [eval/3            % eval(+Expr, +Context, -Result)
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).

:- use_module(classad_parser).


eval(E, C, R) :- ev([C], E, R).

% These all evaluate as themselves, independent of any context.
% Declare these first.
ev(_, error, error).
ev(_, undefined, undefined).
ev(_, true, true).
ev(_, false, false).
ev(_, E, E) :- number(E).
ev(_, E, E) :- functor(E, '[str]', 1).
ev(_, E, E) :- functor(E, '[classad]', 1).

% a list evaluates by evaluating each of its elements:
ev(C, E, R) :- is_list(E), maplist(ev(C), E, R).

% other reserved atoms are intercepted above as atomic expressions.
is_var(A) :- atom(A), A \= 'parent'.

% evaluating a variable:
ev([], V, undefined) :- is_var(V).
ev([C|P], V, R) :- is_var(V), '[classad]'(M)=C, ((get_assoc(V, M, E), ev([C|P], E, R)) ; ev(P, V, R)).

% This is a catchall - has to be declared last.
% TODO: consider some other special error value for this,
% or perhaps throwing an exception.
ev(_, _, error).
