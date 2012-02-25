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

test('reserved-1') :-
    uptoatom("error", 'error'),
    uptoatom("undefined", 'undefined'),
    uptoatom("true", 'true'),
    uptoatom("false", 'false').

test('int-1') :- uptoatom("1", '1').

test('real-1') :- uptoatom("1.0", '1.000000e+00').

test('str-1') :- uptoatom("\"string\"", '"string"').

test('list-0') :- uptoatom("{}", '{}').
test('list-1') :- uptoatom("{1}", '{1}').
test('list-2') :- uptoatom("{1,2}", '{1,2}').

test('reltime-1') :- with_output_to(atom('reltime("1.000")'), unparse('[reltime]'(1))).
test('abstime-1') :- with_output_to(atom('abstime("2012-02-25T09:24:03.100-0700")'), unparse('[abstime]'(1330187043.1, 25200))).

:- end_tests(unparse).
