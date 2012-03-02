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


:- end_tests(classad).
