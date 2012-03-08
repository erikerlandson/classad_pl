:- module(classad, [
          is_classad/1,
          new_classad/1,

          classad_assign_raw/4,
          classad_assign_native/4,
          classad_assign/3,
          classad_assign/4,
          classad_assign/5,

          classad_lookup_raw/4,
          classad_lookup/2,
          classad_lookup/3,
          classad_lookup/4,
          classad_lookup/5,

          classad_serialize/1,
          classad_serialize/2,
          classad_serialize/3,

          classad_deserialize/1,
          classad_deserialize/2,

          classad_match/4
          ]).

:- reexport(classad_eval).

% yap specific: accesses swi date/time manipulation predicates
:- expects_dialect(swi).

% standard libs:
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).
:- use_module(library(date)).

% classad libs:
:- use_module(with_input_from).
:- use_module(classad_parser).
:- use_module(classad_reltime_parser).
:- use_module(classad_unparse).

% create a new empty classad:
new_classad('[classad]'(NewMap)) :- list_to_assoc([], NewMap).

is_classad('[classad]'(M)) :- is_assoc(M).


% there is one other type 'novar' that denotes lookup/4 was
% unable to find the requested variable in the given context
value_type(I, integer) :- integer(I).
value_type(R, real) :- float(R).
value_type(N, number) :- number(N).  % this one is bound-Type checking only
value_type(true, boolean).
value_type(false, boolean).
value_type(error, error).
value_type(undefined, undefined).
value_type([], list).
value_type([_|_], list).
value_type('[str]'(_), string).
value_type('[abstime]'(_,_), abstime).
value_type('[reltime]'(_), reltime).
value_type('[classad]'(_), classad).
value_type(_, badtype).

value_type_in(I, integer) :- integer(I).
value_type_in(R, real) :- float(R).
value_type_in(N, number) :- number(N).  % this one is bound-Type checking only
value_type_in(true, boolean).
value_type_in(false, boolean).
value_type_in(error, error).
value_type_in(undefined, undefined).
value_type_in([], list).
value_type_in([_|_], list).
value_type_in(S, string) :- atom(S).
value_type_in(abstime(_,_), abstime).
value_type_in(date(_,_,_,_,_,_,_,_,_), abstime).
value_type_in(reltime(_), reltime).
value_type_in('[classad]'(_), classad).
value_type_in(_, badtype).

classad_assign_raw(Var, Expr, '[classad]'(Map), '[classad]'(MapR)) :-
    atom(Var), downcase_atom(Var, VarD), 
    classad_eval:variable(VarD),
    put_assoc(VarD, Map, Expr, MapR), !.

classad_assign_raw(Var, Expr, [C|R], [NC|R]) :-
    classad_eval:context_stack([C|R]),
    classad_assign_raw(Var, Expr, C, NC), !.

classad_assign_native(Var, String, Context, ContextR) :-
    parse(String, Expr), classad_assign_raw(Var, Expr, Context, ContextR), !.

classad_assign([], Context, Context).
classad_assign(['='(Var, Value)|R], Context, ContextR) :-
    classad_assign(Var, Value, Context, ContextT, _Type),
    classad_assign(R, ContextT, ContextR), !.
classad_assign([[Var, Value]|R], Context, ContextR) :-
    classad_assign(Var, Value, Context, ContextT, _Type),
    classad_assign(R, ContextT, ContextR), !.
classad_assign([[Var, Value, Type]|R], Context, ContextR) :-
    classad_assign(Var, Value, Context, ContextT, Type),
    classad_assign(R, ContextT, ContextR), !.

classad_assign(Var, Value, Context, ContextR) :-
    classad_assign(Var, Value, Context, ContextR, _Type), !.

classad_assign(Var, Value, Context, ContextR, Type) :-
    ground(Type), as(AsType) = Type, !,
    value_type_in(Value, T),
    promote_in(T, AsType, Value, V),
    classad_assign_raw(Var, V, Context, ContextR), !.

classad_assign(Var, Value, Context, ContextR, Type) :-
    value_type_in(Value, Type),
    internal_val(Type, Value, Expr),
    classad_assign_raw(Var, Expr, Context, ContextR), !.

lookup_context(Var, [C|R], Expr, Context) :-
    C = '[classad]'(Map), !,
    get_assoc(Var, Map, Expr) -> Context = [C|R] ; lookup_context(Var, R, Expr, Context), !.

lookup_context(Var, '[classad]'(Map), Expr, ['[classad]'(Map)]) :-
    get_assoc(Var, Map, Expr), !.

classad_lookup_raw(Var, Context, Expr, VarContext) :-
    atom(Var), is_context(Context), !,
    downcase_atom(Var, VarD), 
    (lookup_context(VarD, Context, Expr, VarContext) ; (Expr='[noexpr]', VarContext=[])), !.

classad_lookup(Context, []) :- is_context(Context), !.
classad_lookup(Context, ['='(Var, Result)|R]) :-
    classad_lookup(Var, Context, Result),
    classad_lookup(Context, R), !.
classad_lookup(Context, [[Var, Result]|R]) :-
    classad_lookup(Var, Context, Result),
    classad_lookup(Context, R), !.
classad_lookup(Context, [[Var, Result, Type]|R]) :-
    classad_lookup(Var, Context, Result, Type),
    classad_lookup(Context, R), !.

classad_lookup(Context, RescopeList, []) :- is_context(Context), is_rescope_list(RescopeList), !.
classad_lookup(Context, RescopeList, ['='(Var, Result)|R]) :-
    classad_lookup(Var, Context, RescopeList, Result),
    classad_lookup(Context, RescopeList, R), !.
classad_lookup(Context, RescopeList, [[Var, Result]|R]) :-
    classad_lookup(Var, Context, RescopeList, Result),
    classad_lookup(Context, RescopeList, R), !.
classad_lookup(Context, RescopeList, [[Var, Result, Type]|R]) :-
    classad_lookup(Var, Context, RescopeList, Result, Type),
    classad_lookup(Context, RescopeList, R), !.

classad_lookup(Var, Context, Result) :-
    atom(Var), is_context(Context), !,
    classad_lookup(Var, Context, Result, _Type), !.

classad_lookup(Var, Context, RescopeList, Result) :-
    atom(Var), is_context(Context), is_rescope_list(RescopeList), !,
    classad_lookup(Var, Context, RescopeList, Result, _Type), !.

classad_lookup(Var, Context, Result, Type) :-
    atom(Var), is_context(Context), 
    ground(Type), as(AsType) = Type, !,
    classad_lookup(Var, Context, R, T),
    promote(T, AsType, R, Result), !.

classad_lookup(Var, Context, Result, Type) :-
    atom(Var), is_context(Context), !,
    classad_lookup_raw(Var, Context, Expr, ExprContext),
    ((Expr == '[noexpr]') -> 
        (Result = undefined, Type = novar)
     ;
        (classad_eval(Expr, ExprContext, R), value_type(R, Type), external_val(R, Result))), !.

classad_lookup(Var, Context, RescopeList, Result, Type) :-
    atom(Var), is_context(Context), is_rescope_list(RescopeList),
    ground(Type), as(AsType) = Type, !,
    classad_lookup(Var, Context, RescopeList, R, T),
    promote(T, AsType, R, Result), !.

classad_lookup(Var, Context, RescopeList, Result, Type) :- 
    atom(Var), is_context(Context), is_rescope_list(RescopeList), !,
    classad_lookup_raw(Var, Context, Expr, ExprContext),
    ((Expr == '[noexpr]') -> 
        (Result = undefined, Type = novar)
     ;
        (classad_eval(Expr, ExprContext, RescopeList, R), value_type(R, Type), external_val(R, Result))), !.

internal_val(integer, I, I).
internal_val(real, R, R).
internal_val(number, N, N).
internal_val(boolean, B, B).
internal_val(error, E, E).
internal_val(undefined, U, U).
internal_val(list, L, IVL) :- maplist(iv_traverse, L, IVL).
internal_val(string, S, '[str]'(S)).
internal_val(abstime, abstime(T, Z), '[abstime]'(T, Z)).
internal_val(abstime, Date, '[abstime]'(TS, Z)) :- 
    functor(Date, date, 9), 
    date_time_stamp(Date, TS), 
    date_time_value(utc_offset, Date, Z).
internal_val(reltime, reltime(T), '[reltime]'(T)).
internal_val(classad, C, C).

iv_traverse(V, E) :- value_type_in(V, T), internal_val(T, V, E).

external_val('[str]'(S), S).
external_val('[abstime]'(S, Z), abstime(S, Z)).
external_val('[reltime]'(S), reltime(S)).
external_val(L, EL) :- is_list(L), maplist(external_val, L, EL).
external_val(E, E).

% identity promotions:
promote(Type, Type, V, V).

promote(real, integer, V, P) :- P is integer(V).
promote(reltime, integer, reltime(S), P) :- P is integer(S).
promote(abstime, integer, abstime(S, _Z), P) :- P is integer(S).
promote(boolean, integer, true, 1).
promote(boolean, integer, false, 0).

promote(integer, real, V, P) :- P is float(V). 
promote(reltime, real, reltime(S), P) :- P is float(S).
promote(abstime, real, abstime(S, _Z), P) :- P is float(S).
promote(boolean, real, true, 1.0).
promote(boolean, real, false, 0.0).

promote(integer, number, V, V).
promote(real, number, V, V).
promote(reltime, number, reltime(S), S).
promote(abstime, number, abstime(S, _Z), S).
promote(boolean, number, true, 1).
promote(boolean, number, false, 0).

promote(integer, abstime, S, abstime(S, Z)) :- classad_eval:local_tzo(Z).
promote(real, abstime, S, abstime(S, Z)) :- classad_eval:local_tzo(Z).

promote(integer, reltime, S, reltime(S)).
promote(real, reltime, S, reltime(S)).

promote(abstime, date, abstime(S, Z), Date) :- stamp_date_time(S, Date, Z).
promote(integer, date, S, Date) :- classad_eval:local_tzo(Z), stamp_date_time(S, Date, Z).
promote(real, date, S, Date) :- classad_eval:local_tzo(Z), stamp_date_time(S, Date, Z).

promote(string, codelist, SA, CL) :- atom_codes(SA, CL).


% identity promotions:
promote_in(Type, Type, V, V).

promote_in(integer, real, V, R) :- R is float(V).
promote_in(real, integer, V, R) :- R is integer(V).

promote_in(integer, number, V, V).
promote_in(real, number, V, V).

promote_in(list, string, CL, '[str]'(S)) :- atom_codes(S, CL).

promote_in(integer, reltime, S, '[reltime]'(S)).
promote_in(real, reltime, S, '[reltime]'(S)).

promote_in(integer, abstime, S, '[abstime]'(S, Z)) :- classad_eval:local_tzo(Z).
promote_in(real, abstime, S, '[abstime]'(S, Z)) :- classad_eval:local_tzo(Z).


classad_serialize(Classad) :- !, classad_serialize(Classad, [nl, indent(4)]).

classad_serialize(Stream, Classad, Args) :- is_stream(Stream), !,
    with_output_to(Stream, classad_serialize(Classad, Args)).

classad_serialize(Stream, Classad) :- is_stream(Stream), !,
    with_output_to(Stream, classad_serialize(Classad)), !.

classad_serialize('[classad]'(M), Args) :- is_list(Args), !, unparse('[classad]'(M), Args), !.

classad_deserialize(Stream, Classad) :- is_stream(Stream), !,
    with_input_from(Stream, classad_deserialize(Classad)), !.

classad_deserialize('[classad]'(M)) :-
    \+at_end_of_stream,
    stream_position(P0),
    (load_classad_block(String) ; (set_stream_position(P0), fail)),
    (parse(String, '[classad]'(M)); (set_stream_position(P0), fail)), !.

load_classad_block(String) :-
    get(91),
    lcb_work(1, [91], String).

lcb_work(0, Str, Str).
lcb_work(Lev, StrI, StrO) :- !,
    get0(C), C >= 0,
    ((C == 93) ->
        (LevN is Lev-1)
    ; ((C == 91) ->
        (LevN is Lev+1)
    ;
        (LevN is Lev))),
    append(StrI, [C], StrT),
    lcb_work(LevN, StrT, StrO).


eqpair('='(A,B), '='(A,B)) :- !.
eqpair(A, '='(A,A)) :- !.

classad_match(ContextL, ContextR, '='(VarL, VarR), '='(RSVL, RSVR)) :-
    atom(VarL), atom(VarR), atom(RSVL), atom(RSVR), is_context(ContextL), is_context(ContextR), !,
    classad_lookup(VarL, ContextL, [RSVL=ContextR], true, as(boolean)),
    classad_lookup(VarR, ContextR, [RSVR=ContextL], true, as(boolean)), !.

classad_match(ContextL, ContextR, Var, RSV) :- 
    eqpair(Var, VarP), eqpair(RSV, RSVP), !,
    classad_match(ContextL, ContextR, VarP, RSVP), !.
