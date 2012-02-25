:- use_module('../lib/classad_unparse.pl').
:- use_module('../lib/classad_parser.pl').

:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(date)).

uptoatom(S, A) :-
    parse(S, E),
    with_output_to(atom(A), unparse(E)).

:- begin_tests(unparse).

test('int-1') :- uptoatom("1", '1').

test('real-1') :- uptoatom("1.0", '1.000000e+00').

test('str-1') :- uptoatom("\"string\"", '"string"').

test('list-0') :- uptoatom("{}", '{}').
test('list-1') :- uptoatom("{1}", '{1}').
test('list-2') :- uptoatom("{1,2}", '{1,2}').



:- end_tests(unparse).
