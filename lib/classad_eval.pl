:- module(classad_eval,
          [classad_eval/3,            % classad_eval(+Expr, +Context, -Result)
           classad_eval/4,            % classad_eval(+Expr, +Context, +RescopeList, -Result)
           classad_eval_native/3,     % classad_eval_native(+String, +Context, -Result)
           classad_eval_native/4,     % classad_eval_native(+String, +Context, +RescopeList, -Result)
           is_context/1,
           is_rescope_list/1,
           rescope_classad/2,
           classad_register_function/2,
           classad_register_function/3
          ]).

% yap specific: accesses swi date/time manipulation predicates
:- expects_dialect(swi).

% standard libs:
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).
:- use_module(library(date)).

% classad libs:
:- use_module(classad_common).
:- use_module(classad_parser).
:- use_module(classad_reltime_parser).


% Evaluate Expr where Context is given as a 'stack' of contexts:
classad_eval(Expr, Context, Result) :- context_stack(Context), !,
    ev([Context,Expr], [_,Result]), !.

% Evaluate Expr using context of the given classad:
classad_eval(Expr, Context, Result) :- functor(Context, '[classad]', 1), !,
    classad_eval(Expr, [Context], Result), !.

% Parse String as a native syntax classad expression, and evaluate it in Context
classad_eval_native(String, Context, Result) :-
    parse(String, Expr), classad_eval(Expr, Context, Result), !.

classad_eval(Expr, Context, RescopeList, Result) :-
    rescope_classad(RescopeList, RescopeCA),
    flatten([Context, RescopeCA], ContextRS),
    classad_eval(Expr, ContextRS, Result), !.

classad_eval_native(String, Context, RescopeList, Result) :- !,
    parse(String, Expr), classad_eval(Expr, Context, RescopeList, Result).

% Used to keep track of variable "goal stack", for cyclic expr detection:
:- dynamic evvg/2.

% these can be updated by registration of functions:
:- dynamic registered_function/2.
:- dynamic strict_function/3.

% other definitions may appear between declarations for ev
:- discontiguous(ev/2).
:- discontiguous(evf/2).

% succeeds if argument is a valid context stack
context_stack(L) :- ground(L), L == []. 
context_stack(['[classad]'(_) | R]) :- context_stack(R), !.

is_context('[classad]'(_)).
is_context(Stack) :- context_stack(Stack), !.

is_rescope_list(L) :- ground(L), L == [].
is_rescope_list(['='(V, C)|R]) :- atom(V), is_context(C), is_rescope_list(R), !.

% assemble a classad from a list [var1 = classad1, var2 = classad2, ...]
rescope_classad(RescopeList, '[classad]'(RescopeMap)) :-
    list_to_assoc([], NewMap),
    rescope_classad_work(RescopeList, NewMap, RescopeMap), !.
rescope_classad_work([], M, M).
rescope_classad_work(['='(V, Context) | R], IM, OM) :-
    atom(V), downcase_atom(V, VD), 
    variable(VD),
    rescope_stack(VD, Context, Sel, Var, ClassadRS),
    compose_sel(Sel, SelExpr),
    (atom(SelExpr) -> 
        put_assoc(VD, IM, ClassadRS, TM)
     ;
        (put_assoc(VD, IM, SelExpr, TM2),
         put_assoc(Var, TM2, ClassadRS, TM))),
    rescope_classad_work(R, TM, OM), !.

% no empty list
compose_sel([V], V).
compose_sel([V|R], E) :- compose_sel_work(R, V, E), !.

compose_sel_work([], E, E).
compose_sel_work([V|R], LE, E) :-
    compose_sel_work(R, '[sel]'(LE, V), E), !.

% no empty stacks by internal convention
rescope_stack(Var, '[classad]'(M), SE, V, CA) :-
    rescope_stack_work(0, Var, ['[classad]'(M)], SE, V, CA), !.
rescope_stack(Var, Stack, SE, V, CA) :-
    reverse(Stack, StackR),
    rescope_stack_work(0, Var, StackR, SE, V, CA), !.

% this assumes stack has been reversed from normal convention 
% (so last element of list is innermost-context)
rescope_stack_work(Lev, Var, [CA], [V], V, CA) :-
    with_output_to(atom(V), format("___~a_~d___", [Var, Lev])), !.
rescope_stack_work(Lev, Var, ['[classad]'(M)|[H|R]], [VarCA|SelT], VarCA, '[classad]'(MRS)) :-
    LevT is Lev+1,
    rescope_stack_work(LevT, Var, [H|R], SelT, VarCAT, CAT),
    with_output_to(atom(VarCA), format("___~a_~d___", [Var, Lev])),
    put_assoc(VarCAT, M, CAT, MRS), !.

% this will be different on other dialects.
max_int(Z) :- yap_flag(max_tagged_integer, Z).

% utility for constructing pairs
pair(A, B, [A,B]).

% why do I have to write this?
fmod(_, 0, error).
fmod(_, 0.0, error).
fmod(N, D, R) :- D < 0, DA is -D, NA is -N, fmod(NA, DA, T), R is -T, !.
fmod(N, D, R) :- N < 0, A is -N, fmod(A, D, T), R is D-T, !.
fmod(N, D, R) :- Q is N/D, F is floor(Q), R is (Q-F)*D, !.

% atomic expressions:
% I am ordering these so that most commonly expected literals
% get tested first
atomic_expr(N) :- number(N).
atomic_expr('[str]'(_)).
atomic_expr(true).
atomic_expr(false).
atomic_expr(undefined).
atomic_expr('[abstime]'(_,_)).
atomic_expr('[reltime]'(_)).
atomic_expr('[classad]'(_)).
atomic_expr(error).

variable(V) :- atom(V), V \= [], \+classad_common:reserved_word(V).

arithmetic_op('+').
arithmetic_op('-').
arithmetic_op('*').
arithmetic_op('/').
arithmetic_op('%').

sign_op('+').
sign_op('-').

comparison_op('==').
comparison_op('!=').
comparison_op('<').
comparison_op('>').
comparison_op('<=').
comparison_op('>=').

bitwise_op('|').
bitwise_op('^').
bitwise_op('&').

relaxed_comp_op('=?=').
relaxed_comp_op('=!=').

shift_op('>>>').
shift_op('>>').
shift_op('<<').

promote_for_bitwise(I, I) :- integer(I).
promote_for_bitwise(true, true).
promote_for_bitwise(false, false).
promote_for_bitwise(undefined, undefined).
promote_for_bitwise(_, error).

promote_for_shift(I, I) :- integer(I).
promote_for_shift(undefined, undefined).
promote_for_shift(_, error).

promote_to_numeric(N, N) :- number(N).
promote_to_numeric(true, 1).
promote_to_numeric(false, 0).
promote_to_numeric(undefined, undefined).
promote_to_numeric(_, error).

promote_for_arithmetic(N, N) :- number(N).
promote_for_arithmetic(true, 1).
promote_for_arithmetic(false, 0).
promote_for_arithmetic(undefined, undefined).
promote_for_arithmetic('[abstime]'(T,Z), '[abstime]'(T,Z)).
promote_for_arithmetic('[reltime]'(T), '[reltime]'(T)).
promote_for_arithmetic('[str]'(S), '[str]'(S)).
promote_for_arithmetic(L, L) :- is_list(L).
promote_for_arithmetic(_, error).

promote_to_boolean(true, true).
promote_to_boolean(false, false).
promote_to_boolean(0, false).
promote_to_boolean(0.0, false).
promote_to_boolean(N, true) :- number(N).
promote_to_boolean(undefined, undefined).
promote_to_boolean(_, error).

promote_for_comparison(N, N) :- number(N).
promote_for_comparison('[str]'(X), '[str]'(X)).
promote_for_comparison(true, 1).
promote_for_comparison(false, 0).
promote_for_comparison(undefined, undefined).
promote_for_comparison('[abstime]'(T,Z), '[abstime]'(T,Z)).
promote_for_comparison('[reltime]'(T), '[reltime]'(T)).
promote_for_comparison(_, error).

% relaxed comparisons return false if either argument is undefined (and not error).
ev_relaxed_comp(_, error, _, error).
ev_relaxed_comp(_, _, error, error).
ev_relaxed_comp(_, undefined, _, false).
ev_relaxed_comp(_, _, undefined, false).
ev_relaxed_comp('=?=', X, Y, R) :- ev_strict_binary('==', X, Y, R).
ev_relaxed_comp('=!=', X, Y, R) :- ev_strict_binary('!=', X, Y, R).
ev_relaxed_comp(_, _, _, error).

% This predicate assumes that all arguments have already been type checked/promoted
% in a way that is appropriate for the given operator:
ev_strict_binary(_, error, _, error).
ev_strict_binary(_, _, error, error).
ev_strict_binary(_, undefined, _, undefined).
ev_strict_binary(_, _, undefined, undefined).
ev_strict_binary('+', X, Y, R) :- number(X), number(Y), R is X + Y.
ev_strict_binary('+', '[abstime]'(X,Z), '[reltime]'(Y), '[abstime]'(R,Z)) :- R is X+Y.
ev_strict_binary('+', '[reltime]'(X), '[abstime]'(Y,Z), '[abstime]'(R,Z)) :- R is X+Y.
ev_strict_binary('+', '[reltime]'(X), '[reltime]'(Y), '[reltime]'(R)) :- R is X+Y.
ev_strict_binary('+', '[str]'(S1), '[str]'(S2), '[str]'(R)) :- concat_atom([S1,S2], R).
ev_strict_binary('+', L1, L2, R) :- is_list(L1), is_list(L2), append(L1, L2, R).
ev_strict_binary('-', X, Y, R) :- number(X), number(Y), R is X - Y.
ev_strict_binary('-', '[abstime]'(X,_), '[abstime]'(Y,_), '[reltime]'(R)) :- R is X-Y.
ev_strict_binary('-', '[abstime]'(X,Z), '[reltime]'(Y), '[abstime]'(R,Z)) :- R is X-Y.
ev_strict_binary('-', '[reltime]'(X), '[reltime]'(Y), '[reltime]'(R)) :- R is X-Y.
ev_strict_binary('*', X, Y, R) :- number(X), number(Y), R is X * Y.
ev_strict_binary('/', _, 0, error).
ev_strict_binary('/', _, 0.0, error).
ev_strict_binary('/', X, Y, R) :- integer(X), integer(Y), R is X // Y.
ev_strict_binary('/', X, Y, R) :- number(X), number(Y), R is X / Y.
ev_strict_binary('%', _, 0, error).
ev_strict_binary('%', _, 0.0, error).
ev_strict_binary('%', X, Y, R) :- integer(X), integer(Y), R is X mod Y.
ev_strict_binary('%', X, Y, R) :- number(X), number(Y), fmod(X, Y, R).
ev_strict_binary('==', '[abstime]'(X,_), '[abstime]'(Y,_), R) :- (X=:=Y) -> R = true ; R = false.
ev_strict_binary('==', '[reltime]'(X), '[reltime]'(Y), R) :- (X=:=Y) -> R = true ; R = false.
ev_strict_binary('==', '[str]'(X), '[str]'(Y), R) :- ((X==Y, R = true); R = false).
ev_strict_binary('==', X, Y, R) :- number(X), number(Y), ((X=:=Y, R = true); R = false).
ev_strict_binary('!=', '[abstime]'(X,_), '[abstime]'(Y,_), R) :- (X=\=Y) -> R = true ; R = false.
ev_strict_binary('!=', '[reltime]'(X), '[reltime]'(Y), R) :- (X=\=Y) -> R = true ; R = false.
ev_strict_binary('!=', '[str]'(X), '[str]'(Y), R) :- ((X\==Y, R = true); R = false).
ev_strict_binary('!=', X, Y, R) :- number(X), number(Y), ((X=\=Y, R = true); R = false).
ev_strict_binary('<', '[abstime]'(X,_), '[abstime]'(Y,_), R) :- (X<Y) -> R = true ; R = false.
ev_strict_binary('<', '[reltime]'(X), '[reltime]'(Y), R) :- (X<Y) -> R = true ; R = false.
ev_strict_binary('<', '[str]'(X), '[str]'(Y), R) :- ((X @< Y, R = true); R = false).
ev_strict_binary('<', X, Y, R) :- number(X), number(Y), ((X < Y, R = true); R = false).
ev_strict_binary('>', '[abstime]'(X,_), '[abstime]'(Y,_), R) :- (X>Y) -> R = true ; R = false.
ev_strict_binary('>', '[reltime]'(X), '[reltime]'(Y), R) :- (X>Y) -> R = true ; R = false.
ev_strict_binary('>', '[str]'(X), '[str]'(Y), R) :- ((X @> Y, R = true); R = false).
ev_strict_binary('>', X, Y, R) :- number(X), number(Y), ((X > Y, R = true); R = false).
ev_strict_binary('<=', '[abstime]'(X,_), '[abstime]'(Y,_), R) :- (X=<Y) -> R = true ; R = false.
ev_strict_binary('<=', '[reltime]'(X), '[reltime]'(Y), R) :- (X=<Y) -> R = true ; R = false.
ev_strict_binary('<=', '[str]'(X), '[str]'(Y), R) :- ((X @=< Y, R = true); R = false).
ev_strict_binary('<=', X, Y, R) :- number(X), number(Y), ((X =< Y, R = true); R = false).
ev_strict_binary('>=', '[abstime]'(X,_), '[abstime]'(Y,_), R) :- (X>=Y) -> R = true ; R = false.
ev_strict_binary('>=', '[reltime]'(X), '[reltime]'(Y), R) :- (X>=Y) -> R = true ; R = false.
ev_strict_binary('>=', '[str]'(X), '[str]'(Y), R) :- ((X @>= Y, R = true); R = false).
ev_strict_binary('>=', X, Y, R) :- number(X), number(Y), ((X >= Y, R = true); R = false).
ev_strict_binary('|', X, Y, R) :- integer(X), integer(Y), R is X \/ Y.
ev_strict_binary('|', true, true, true).
ev_strict_binary('|', false, true, true).
ev_strict_binary('|', true, false, true).
ev_strict_binary('|', false, false, false).
ev_strict_binary('&', X, Y, R) :- integer(X), integer(Y), R is X /\ Y.
ev_strict_binary('&', false, false, false).
ev_strict_binary('&', true, false, false).
ev_strict_binary('&', false, true, false).
ev_strict_binary('&', true, true, true).
ev_strict_binary('^', X, Y, R) :- integer(X), integer(Y), R is X >< Y.
ev_strict_binary('^', false, false, false).
ev_strict_binary('^', true, false, true).
ev_strict_binary('^', false, true, true).
ev_strict_binary('^', true, true, false).
ev_strict_binary('<<', X, Y, R) :- integer(X), integer(Y), R is X << Y.
ev_strict_binary('>>', X, Y, R) :- integer(X), integer(Y), R is X >> Y.
ev_strict_binary('>>>', X, Y, R) :- integer(X), integer(Y), Y =< 0, R is X << Y.
ev_strict_binary('>>>', X, Y, R) :- integer(X), integer(Y), X >= 0, R is X >> Y.
ev_strict_binary('>>>', X, Y, R) :- integer(X), integer(Y), max_int(Z), R is ((X >> 1) /\ Z) >> (Y-1).
ev_strict_binary(_, _, _, error).

ev_strict_unary(_, error, error).
ev_strict_unary(_, undefined, undefined).
ev_strict_unary('+', X, X) :- number(X).
ev_strict_unary('+', '[abstime]'(T,Z), '[abstime]'(T,Z)).
ev_strict_unary('+', '[reltime]'(T), '[reltime]'(T)).
ev_strict_unary('-', X, Y) :- number(X), Y is -X.
ev_strict_unary('-', '[reltime]'(T), '[reltime]'(R)) :- R is -T.
ev_strict_unary('!', true, false).
ev_strict_unary('!', false, true).
ev_strict_unary('~', X, Y) :- integer(X), Y is \X.
ev_strict_unary('~', true, false).
ev_strict_unary('~', false, true).
ev_strict_unary(_, _, error).

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

% I wrote these to simplify the logic of the select operator '[sel]'/'.' but
% they also probably provide some useful sanity checking
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
ev([C, E], [C, R]) :- is_list(E), maplist(pair(C), E, T), maplist(ev, T, RT), maplist(nth(2), RT, R).

% select op
% here we know from grammar that 'V' is variable, or 'parent'
ev([C, '[sel]'(SE, V)], R) :- ev([C, SE], [SC, SR]), ev([[SR|SC], V], R).

% arithmetic ops
ev([C, E], [C, R]) :- 
    E=..[OP, SL, SR], arithmetic_op(OP), 
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    promote_for_arithmetic(LR, LN), promote_for_arithmetic(RR, RN), 
    ev_strict_binary(OP, LN, RN, R).

% unary sign ops
ev([C, E], [C, R]) :- 
    E=..[OP, SE], sign_op(OP), 
    ev([C, SE], [_, SR]),
    promote_for_arithmetic(SR, SN),
    ev_strict_unary(OP, SN, R).

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

% ! operator
ev([C, '!'(SE)], [C, R]) :-
    ev([C, SE], [_, SR]),
    promote_to_boolean(SR, SB),
    ev_strict_unary('!', SB, R).

% '?:' conditional operator
ev([C, '?:'(TE, LE, RE)], [RC, R]) :-
    ev([C, TE], [_, TR]), promote_to_boolean(TR, TB),
    ((TB == undefined, RC = C, R = undefined)
    ;
    (TB == true, ev([C, LE], [RC, R]))
    ;
    (TB == false, ev([C, RE], [RC, R]))
    ;
    (RC = C, R = error)).

% standard comparison operators
ev([C, E], [C, R]) :- 
    E=..[OP, SL, SR], comparison_op(OP), 
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    promote_for_comparison(LR, LC), promote_for_comparison(RR, RC), 
    ev_strict_binary(OP, LC, RC, R).

% relaxed comparison operators
ev([C, E], [C, R]) :- 
    E=..[OP, SL, SR], relaxed_comp_op(OP), 
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    promote_for_comparison(LR, LC), promote_for_comparison(RR, RC), 
    ev_relaxed_comp(OP, LC, RC, R).

ev([C, 'is'(SL, SR)], [C, R]) :-
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    ev_is(LR, RR, R).
% Two values are equal wrt 'is' iff they are structurally identical.
% Note, this slightly deviates from classad language standard, for types list and 
% classad/record, as the language spec states that two values of that type must be
% the same structure, not just two structures that are the same.  Prolog makes
% that distinction difficult to implement, and I dont think it is a very
% important sematic.
ev_is(V, V, true).
ev_is(_, _, false).

ev([C, 'isnt'(SL, SR)], [C, R]) :-
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    ev_isnt(LR, RR, R).
ev_isnt(V, V, false).
ev_isnt(_, _, true).


% bitwise binary ops
ev([C, E], [C, R]) :- 
    E=..[OP, SL, SR], bitwise_op(OP), 
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    promote_for_bitwise(LR, LN), promote_for_bitwise(RR, RN), 
    ev_strict_binary(OP, LN, RN, R).

% biwise ~ operator
ev([C, '~'(SE)], [C, R]) :-
    ev([C, SE], [_, SR]),
    promote_for_bitwise(SR, SB),
    ev_strict_unary('~', SB, R).

% shift binary ops
ev([C, E], [C, R]) :- 
    E=..[OP, SL, SR], shift_op(OP), 
    ev([C, SL], [_, LR]), ev([C, SR], [_, RR]),
    promote_for_shift(LR, LN), promote_for_shift(RR, RN), 
    ev_strict_binary(OP, LN, RN, R).

% indexing [] operator
ev([C, '[]'(BE, IE)], [RC, R]) :-
    ev([C, BE], [BC, BR]), ev([C, IE], [_, IR]),
    ev_idxop(BC, BR, IR, RC, R).
ev_idxop(RC, error, _, RC, error).
ev_idxop(RC, _, error, RC, error).
ev_idxop(RC, undefined, _, RC, undefined).
ev_idxop(RC, _, undefined, RC, undefined).
ev_idxop(BC, '[classad]'(M), '[str]'(V), RC, R) :- ev([BC, '[sel]'('[classad]'(M), V)], [RC, R]). 
ev_idxop(RC, [H|T], I, RC, R) :- integer(I), I>=0, nth0(I, [H|T], R).
ev_idxop(RC, L, '[str]'(S), RC, R) :- is_list(L), maplist(ev_idxop_pair(RC, '[str]'(S)), L, T), maplist(ev, T, RT), maplist(nth(2), RT, R).
ev_idxop(RC, _, _, RC, error).
ev_idxop_pair(C, I, B, [C, '[]'(B, I)]).

% function calls
ev([C, '[func]'(FN, AE)], [RC, R]) :-
    % evaluate argument expression list
    ev([C, AE], [_, AR]),
    % now invoke check for strict/non strict function call:
    evfc([C, FN, AR], [RC, R]).

% strict functions: propagate error/undefined in the function arg list
evfc([C, FN, AL], [RC, R]) :- 
    strict_function(FN, MinA, MaxA),
    (((wrong_arg_count(AL, MinA, MaxA) ; member(error, AL)), RC = C, R = error)
    ;
    (member(undefined, AL), RC = C, R = undefined)
    ;
    evf([C, FN, AL], [RC, R])).

% non-strict functions are fall-thru
evfc([C, FN, AL], [RC, R]) :- evf([C, FN, AL], [RC, R]).

% registered functions
evf([C, FN, AL], [C, R]) :- registered_function(FN, MapFN), call(MapFN, AL, R).

% anything else evaluates to error:
evf([C, _, _], [C, error]).

wrong_arg_count(AL, MinA, MaxA) :- length(AL, Len), (Len < MinA ; MaxA < Len). 

% 2nd argument needs to be scoped to calling module:
:- meta_predicate classad_register_function(+, :).
:- meta_predicate classad_register_function(+, :, +).

classad_register_function(NativeName, PredicateName) :-
    atom(NativeName), downcase_atom(NativeName, NativeNameD),
    assert(registered_function(NativeNameD, PredicateName)).

classad_register_function(NativeName, PredicateName, strict(ArgMin, ArgMax)) :-
    integer(ArgMin), integer(ArgMax), ArgMin >= 0, ArgMax >= ArgMin,
    atom(NativeName), downcase_atom(NativeName, NativeNameD),
    assert(registered_function(NativeNameD, PredicateName)),
    assert(strict_function(NativeNameD, ArgMin, ArgMax)).

classad_register_function(NativeName, PredicateName, strict(ArgMax)) :-
    integer(ArgMax), ArgMax >= 0,
    atom(NativeName), downcase_atom(NativeName, NativeNameD),
    assert(registered_function(NativeNameD, PredicateName)),
    assert(strict_function(NativeNameD, 0, ArgMax)).

classad_register_function(NativeName, PredicateName, strict) :-
    atom(NativeName), downcase_atom(NativeName, NativeNameD),
    assert(registered_function(NativeNameD, PredicateName)),
    assert(strict_function(NativeNameD, 0, 1000000000)).

% function time()
f_time([], R) :- get_time(T), R is integer(T).
:- classad_register_function(time, f_time, strict(0)).

% function abstime() 
f_abstime([], '[abstime]'(T,Z)) :- get_time(T), local_tzo(Z).
f_abstime([T,Z], '[abstime]'(T,WZ)) :- number(T), integer(Z), WZ is -Z.
f_abstime([T], '[abstime]'(T,Z)) :- number(T), local_tzo(Z).
f_abstime(['[str]'(TS)], '[abstime]'(T,Z)) :- parse_time(TS, T), local_tzo(Z).
:- classad_register_function(abstime, f_abstime, strict(2)).

local_tzo(Z) :- stamp_date_time(0, DT, local), date_time_value(utc_offset, DT, Z).

f_reltime(['[str]'(TA)], '[reltime]'(S)) :- atom_codes(TA, TS), parse_reltime(TS, S).
f_reltime([S], '[reltime]'(S)) :- number(S).
:- classad_register_function(reltime, f_reltime, strict(1, 1)).

f_interval([S], '[str]'(SS)) :- number(S), with_output_to(atom(SS), unparse_reltime(S)).
:- classad_register_function(interval, f_interval, strict(1,1)).

% This is a catchall - has to be declared last.
% TODO: consider some other special error value for this,
% or perhaps throwing an exception.
ev([C, _], [C, error]).
