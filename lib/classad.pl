:- module(classad, [
          is_classad/1,
          new_classad/1,

          assign_raw/4,
          assign_native/4,
          assign/4,
          assign/5,

          lookup_raw/4,
          lookup/3,
          lookup/4,

          serialize/2,
          serialize/3,
          deserialize/2
          ]).

% yap specific: accesses swi date/time manipulation predicates
:- expects_dialect(swi).

% standard libs:
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(apply_macros)).
:- use_module(library(date)).

% classad libs:
:- use_module(classad_parser).
:- use_module(classad_reltime_parser).
:- use_module(classad_eval).
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

assign_raw(Var, Expr, '[classad]'(Map), '[classad]'(MapR)) :-
    atom(Var), downcase_atom(Var, VarD), 
    classad_eval:variable(VarD),
    put_assoc(VarD, Map, Expr, MapR).

assign_raw(Var, Expr, [C|R], [NC|R]) :-
    classad_eval:context_stack([C|R]),
    assign_raw(Var, Expr, C, NC).

assign_native(Var, String, Classad, ClassadR) :-
    parse(String, Expr), assign_raw(Var, Expr, Classad, ClassadR).

assign(Var, Value, Classad, ClassadR) :- assign(Var, Value, Classad, ClassadR, _Type).

assign(Var, Value, Classad, ClassadR, Type) :-
    ground(Type), as(AsType) = Type,
    value_type_in(Value, T),
    promote_in(T, AsType, Value, V),
    assign_raw(Var, V, Classad, ClassadR).

assign(Var, Value, Classad, ClassadR, Type) :-
    value_type_in(Value, Type),
    internal_val(Type, Value, Expr),
    assign_raw(Var, Expr, Classad, ClassadR).

lookup_context(Var, [C|R], Expr, Context) :-
    C = '[classad]'(Map),
    get_assoc(Var, Map, Expr) -> Context = [C|R] ; lookup_context(Var, R, Expr, Context).

lookup_context(Var, '[classad]'(Map), Expr, ['[classad]'(Map)]) :-
    get_assoc(Var, Map, Expr).

lookup_raw(Var, Context, Expr, VarContext) :-
    atom(Var), downcase_atom(Var, VarD),
    (functor(Context, '[classad]', 1) ; classad_eval:context_stack(Context)),
    (lookup_context(VarD, Context, Expr, VarContext) ; (Expr='[noexpr]', VarContext=[])).

lookup(Var, Context, Result) :- lookup(Var, Context, Result, _Type).

lookup(Var, Context, Result, Type) :-
    ground(Type), as(AsType) = Type,
    lookup(Var, Context, R, T),
    promote(T, AsType, R, Result).

lookup(Var, Context, Result, Type) :-
    lookup_raw(Var, Context, Expr, ExprContext),
    ((Expr == '[noexpr]') -> 
        (Result = undefined, Type = novar)
     ;
        (eval(Expr, ExprContext, R), value_type(R, Type), external_val(R, Result))).

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

promote(integer, integer, V, V).
promote(real, integer, V, P) :- P is integer(V).
promote(reltime, integer, reltime(S), P) :- P is integer(S).
promote(abstime, integer, abstime(S, _Z), P) :- P is integer(S).
promote(boolean, integer, true, 1).
promote(boolean, integer, false, 0).

promote(integer, real, V, P) :- P is float(V). 
promote(real, real, V, V).
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

promote(abstime, date, abstime(S, Z), Date) :- stamp_date_time(S, Date, Z).
promote(integer, date, S, Date) :- classad_eval:local_tzo(Z), stamp_date_time(S, Date, Z).
promote(real, date, S, Date) :- classad_eval:local_tzo(Z), stamp_date_time(S, Date, Z).

promote(string, codelist, SA, CL) :- atom_codes(SA, CL).


promote_in(list, string, CL, '[str]'(S)) :- atom_codes(S, CL).

promote_in(integer, reltime, S, '[reltime]'(S)).
promote_in(real, reltime, S, '[reltime]'(S)).

promote_in(integer, abstime, S, '[abstime]'(S, Z)) :- classad_eval:local_tzo(Z).
promote_in(real, abstime, S, '[abstime]'(S, Z)) :- classad_eval:local_tzo(Z).


serialize(Stream, Classad) :- !, serialize(Stream, Classad, [nl, indent(4)]).
serialize(Stream, Classad, Args) :- !, is_stream(Stream), with_output_to(Stream, unparse(Classad, Args)).

deserialize(Stream, '[classad]'(M)) :-
    !,
    is_stream(Stream),
    \+at_end_of_stream(Stream),
    stream_position(Stream, P0),
    (load_classad_block(Stream, String) ; (set_stream_position(Stream, P0), fail)),
    (parse(String, '[classad]'(M)); (set_stream_position(Stream, P0), fail)), !.

load_classad_block(Stream, String) :-
    get(Stream, 91),
    lcb_work(1, Stream, [91], String).

lcb_work(0, _Stream, Str, Str).
lcb_work(Lev, Stream, StrI, StrO) :-
    get0(Stream, C), C >= 0,
    ((C == 93) ->
        (LevN is Lev-1)
    ; ((C == 91) ->
        (LevN is Lev+1)
    ;
        (LevN is Lev))),
    append(StrI, [C], StrT),
    lcb_work(LevN, Stream, StrT, StrO).
