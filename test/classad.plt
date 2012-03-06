:- use_module('../lib/classad.pl').

:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(date)).

:- begin_tests(classad).

test('new_classad-1') :-
    new_classad('[classad]'(M)),
    is_assoc(M),
    assoc_to_list(M, []).

test('is_classad-1') :-
    new_classad(CA),
    is_classad(CA).

test('is_classad-2') :-
    \+is_classad(a),
    \+is_classad('[classad]'(foo)).

test('classad_assign_native-1') :-
    new_classad(CA0),
    classad_assign_native(vV, "4+2", CA0, '[classad]'(M)),
    assoc_to_list(M, [vv-'+'(4,2)]).

test('classad_assign/4-1') :-
    new_classad(CA0),
    classad_assign(xX, 0, CA0, '[classad]'(M)),
    assoc_to_list(M, [xx-0]).

test('classad_assign/3-1') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([x1 = error,
                    x2 = undefined,
                    x3 = 42,
                    x4 = 3.14,
                    x5 = 'A String',
                    x6 = ['a', ['list']],
                    x7 = true,
                    x8 = CAZ
                    ],
                   CA0, '[classad]'(M)),
    assoc_to_list(M,
                  [x1-error, 
                   x2-undefined,
                   x3-42,
                   x4-3.14,
                   x5-'[str]'('A String'),
                   x6-['[str]'(a),['[str]'(list)]],
                   x7-true,
                   x8-'[classad]'(MZ)]),
    assoc_to_list(MZ, [z-1]).

test('classad_assign/3-time') :-
    new_classad(CA0),
    classad_assign(['AT1' = date(2012,2,24,6,12,31.0,25200,'MST',false),
                    'AT2' = abstime(1330089151.0, 25200),
                    'AT3' = reltime(60)
                    ],
                   CA0, '[classad]'(M)),
    assoc_to_list(M,
                  [at1-'[abstime]'(1330089151.0, 25200),
                   at2-'[abstime]'(1330089151.0, 25200),
                   at3-'[reltime]'(60)]).

test('classad_assign/3-mixed') :-
    new_classad(CA0),
    classad_assign([x1 = 42,
                    [x2, 3.14],
                    [x3, true, T3],
                    [x4, false, boolean],
                    [x5, "String", as(string)]],
                    CA0, '[classad]'(M)),
    T3 == boolean,
    assoc_to_list(M,
                  [x1-42,
                   x2-3.14,
                   x3-true,
                   x4-false,
                   x5-'[str]'('String')]).

:- end_tests(classad).
