:- module(classad_eval,
          [eval/3            % eval(+Expr, +Context, -Result)
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).

:- use_module(classad_parser).

% Used to denote "parse the following into an expression":
:- op(20, fx, user:as_expr).

% Used to keep track of variable "goal stack", for cyclic expr detection:
:- dynamic evvg/2.

% Parse string S into expression E, then evaluate:
eval(as_expr S, C, R) :- parse(S, E), eval(E, C, R), !.

% Evaluate E using context of the given classad:
eval(E, C, R) :- functor(C, '[classad]', 1), eval(E, [C], R), !.

% Evaluate E where C is given as a 'stack' of contexts:
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

arithmetic_op('+').
arithmetic_op('-').
arithmetic_op('*').
arithmetic_op('/').

promote_to_numeric(N, N) :- number(N).
promote_to_numeric(true, 1).
promote_to_numeric(false, 0).
promote_to_numeric(undefined, undefined).
promote_to_numeric(_, error).

promote_to_boolean(true, true).
promote_to_boolean(false, false).
promote_to_boolean(N, false) :- (N is 0) ; (N is 0.0).
promote_to_boolean(N, true) :- number(N).
promote_to_boolean(undefined, undefined).
promote_to_boolean(_, error).

% This predicate assumes that all arguments have already been type checked/promoted
% in a way that is appropriate for the given operator:
ev_strict_binary(_, error, _, error).
ev_strict_binary(_, _, error, error).
ev_strict_binary(_, undefined, _, undefined).
ev_strict_binary(_, _, undefined, undefined).
ev_strict_binary('+', X, Y, R) :- R is X + Y.
ev_strict_binary('-', X, Y, R) :- R is X - Y.
ev_strict_binary('*', X, Y, R) :- R is X * Y.
ev_strict_binary('/', _, 0, error).
ev_strict_binary('/', _, 0.0, error).
ev_strict_binary('/', X, Y, R) :- integer(X), integer(Y), R is X // Y.
ev_strict_binary('/', X, Y, R) :- R is X / Y.
ev_strict_binary(_, _, _, error).

ev_and(error, _, error).
ev_and(_, error, error).
ev_and(false, _, false).
ev_and(_, false, false).
ev_and(_, undefined, undefined).
ev_and(undefined, _, undefined).
ev_and(true, true, true).
ev_and(_, _, error).

ev_or(error, _, error).
ev_or(_, error, error).
ev_or(true, _, true).
ev_or(_, true, true).
ev_or(_, undefined, undefined).
ev_or(undefined, _, undefined).
ev_or(false, false, false).
ev_or(_, _, error).

% these may arise from select operator, possibly others
ev([[undefined|P], _], [P, undefined]).
ev([[C|P], _], [P, error]) :- \+functor(C, '[classad]', 1).

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
ev([[C|P], V], R) :- 
    variable(V), '[classad]'(M)=C,
    % check for V in the current context:
    (get_assoc(V, M, E),
        % we found V in the current context. check for cyclic dependency:
        ((evvg(V, [C|P]),
            % V is aready a goal, so we have detected a cyclic dependency:
            R = [[C|P], undefined])
        ; % else
            % V is not on the current goal list, so it is safe to evaluate
            % assert V as an active evaluation goal until we are finished evaluating.
            (assertz(evvg(V, [C|P])), ev([[C|P], E], R), retract(evvg(V, [C|P])))) 
    ; % else
        % V was not in current context, so try popping to parent context:
        ev([P, V], R)).

% a list evaluates by evaluating each of its elements:
% match this prior to atom/var below, because '[]' is considered an atom.
ev([C, E], [C, R]) :- is_list(E), maplist(pair(C), E, T), maplist(ev, T, RT), maplist(nth(2), RT, R).

% select op
% here we know from grammar that 'V' is variable, or 'parent'
ev([C, '[sel]'(SE, V)], R) :- ev([C, SE], [SC, SR]), ev([[SR|SC], V], R).

% arithmetic ops
ev([C, E], [C, R]) :- 
    E=..[OP, SL, SR], arithmetic_op(OP), 
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    promote_to_numeric(LR, LN), promote_to_numeric(RR, RN), 
    ev_strict_binary(OP, LN, RN, R).

% && operator
ev([C, '&&'(LE, RE)], [C, R]) :-
    % evaluate left subexpr:
    ev([C, LE], [_,LR]), promote_to_boolean(LR, LB),
    % false or error allows lazy eval:
    (((LB = false ; LB = error), R = LB)
    ; % else
    % otherwise we also eval right subexpr and compute:
    (ev([C, RE], [_,RR]), promote_to_boolean(RR, RB), ev_and(LB, RB, R))).

% || operator
ev([C, '||'(LE, RE)], [C, R]) :-
    % evaluate left subexpr:
    ev([C, LE], [_,LR]), promote_to_boolean(LR, LB),
    % true or error allows lazy eval:
    (((LB = true ; LB = error), R = LB)
    ; % else
    % otherwise we also eval right subexpr and compute:
    (ev([C, RE], [_,RR]), promote_to_boolean(RR, RB), ev_or(LB, RB, R))).

% This is a catchall - has to be declared last.
% TODO: consider some other special error value for this,
% or perhaps throwing an exception.
ev([C, _], [C, error]).
