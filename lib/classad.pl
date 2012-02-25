:- module(classad, [
          new_classad/1,     % new_classad(-Classad)
          assign_raw/4,
          assign_native/4,
          assign/4,
          lookup_raw/4,
          lookup/3,
          lookup/4
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

% create a new empty classad:
new_classad('[classad]'(NewMap)) :- list_to_assoc([], NewMap).

% this spans both internal and 'public' formats
% there is one other type 'novar' that denotes lookup/4 was
% unable to find the requested variable in the given context
value_type(as(_, _), promote).
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
value_type(S, string) :- atom(S).
value_type('[abstime]'(_,_), abstime).
value_type(abstime(_,_), abstime).
value_type(date(_,_,_,_,_,_,_,_,_), abstime).
value_type('[reltime]'(_), reltime).
value_type(reltime(_), reltime).
value_type('[classad]'(_), classad).
value_type(_, badtype).

assign_raw(Var, Expr, '[classad]'(Map), '[classad]'(MapR)) :-
    atom(Var), downcase_atom(Var, VarD), 
    classad_eval:variable(VarD),
    put_assoc(VarD, Map, Expr, MapR).

assign_raw(Var, Expr, [C|R], [NC|R]) :-
    classad_eval:context_stack([C|R]),
    assign_raw(Var, Expr, C, NC).

assign_native(Var, String, Classad, ClassadR) :-
    parse(String, Expr), assign_raw(Var, Expr, Classad, ClassadR).

assign(Var, Value, Classad, ClassadR) :-
    value_type(Value, Type),
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

lookup(Var, Context, Result) :- lookup(Var, Context, Result, _).

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
internal_val(promote, as(string, CL), S) :- is_list(CL), atom_codes(S, CL).
internal_val(promote, as(reltime, S), '[reltime]'(S)) :- number(S).
internal_val(promote, as(abstime, S), '[abstime]'(S, Z)) :- number(S), classad_eval:local_tzo(Z).

iv_traverse(V, E) :- value_type(V, T), internal_val(T, V, E).

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
