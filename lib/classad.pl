:- module(classad, [
          new_classad/1,     % new_classad(-Classad)
          value_type/2,      % value_type(+Value, -Type)
          assign_raw/4,
          assign_native/4,
          assign/4
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

% this spans both internal and 'public' formats:
value_type(I, integer) :- integer(I).
value_type(R, real) :- float(R).
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
value_type('[reltime]'(_), reltime).
value_type(reltime(_), reltime).
value_type('[classad]'(_), classad).
value_type(_, badtype).

assign_raw(Var, Expr, '[classad]'(Map), '[classad]'(MapR)) :-
    atom(Var), downcase_atom(Var, VarD), 
    classad_eval:variable(VarD),
    put_assoc(VarD, Map, Expr, MapR).

assign_native(Var, String, Classad, ClassadR) :-
    parse(String, Expr), assign_raw(Var, Expr, Classad, ClassadR).

assign(Var, Value, Classad, ClassadR) :-
    value_type(Value, Type),
    internal_val(Type, Value, Expr),
    assign_raw(Var, Expr, Classad, ClassadR).

internal_val(integer, I, I).
internal_val(real, R, R).
internal_val(boolean, B, B).
internal_val(error, E, E).
internal_val(undefined, U, U).
internal_val(list, L, L).
internal_val(string, S, '[str]'(S)).
internal_val(abstime, abstime(T, Z), '[abstime]'(T, Z)).
internal_val(reltime, reltime(T, Z), '[reltime]'(T, Z)).
internal_val(classad, C, C).
