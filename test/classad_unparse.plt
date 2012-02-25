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
test('list-2') :- uptoatom("{1, 2}", '{1,2}').

test('reltime-1') :- with_output_to(atom('reltime("1.000")'), unparse('[reltime]'(1))).
test('abstime-1') :- with_output_to(atom('abstime("2012-02-25T09:24:03.100-0700")'), unparse('[abstime]'(1330187043.1, 25200))).

test('uop!-1') :- uptoatom("!a", '(!a)').
test('uop!-2') :- uptoatom("!!a", '(!(!a))').
test('uop~-1') :- uptoatom("~a", '(~a)').
test('uop--1') :- uptoatom("-a", '(-a)').
test('uop+-1') :- uptoatom("+a", '(+a)').

test('op+-1') :- uptoatom("a + b", '(a+b)').

test('op?:-1') :- uptoatom("a ? 1 : 0", '(a?1:0)').
test('op?:-2') :- uptoatom("a ? {3} : \"z\"", '(a?{3}:"z")').

test('op[]-1') :- uptoatom("v[j]", 'v[j]').
test('op[]-2') :- uptoatom("v[j][1 + k]", 'v[j][(1+k)]').

test('op-sel-1') :- uptoatom("parent.a.b", '((parent.a).b)').
test('op-sel-2') :- uptoatom("parent.a.b[k]", '((parent.a).b)[k]').

test('func-0') :- uptoatom("time ()", 'time()').
test('func-1') :- uptoatom("fname ( a1 )", 'fname(a1)').
test('func-2') :- uptoatom("fname ( a1 , v[4+k] )", 'fname(a1,v[(4+k)])').

test('classad-0') :- uptoatom("[]", '[]').
test('classad-1') :- uptoatom("[z = 42]", '[z=42]').
test('classad-1b') :- uptoatom("[z = 42;]", '[z=42]').
test('classad-2') :- uptoatom("[a = 42; q = 6*9]", '[a=42;q=(6*9)]').

:- end_tests(unparse).
