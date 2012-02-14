:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module('../lib/classad_parser.pl').
:- use_module('../lib/classad_eval.pl').

:- begin_tests(classad_eval_ut).

test('error 1') :-
    parse("[a = error;]", C),
    eval(a, C, R),
    assertion(R == error).

test('undefined 1') :-
    parse("[a = undefined;]", C),
    eval(a, C, R),
    assertion(R == undefined).

test('true 1') :-
    parse("[a = true;]", C),
    eval(a, C, R),
    assertion(R == true).

test('false 1') :-
    parse("[a = false;]", C),
    eval(a, C, R),
    assertion(R == false).

test('number 1') :-
    parse("[a = 0;]", C),
    eval(a, C, R),
    assertion(R == 0).

test('string 1') :-
    parse("[a = \"fred and wilma\";]", C),
    eval(a, C, R),
    assertion(R == '[str]'('fred and wilma')).

test('classad 1') :-
    parse("[a = [b=0;];]", C),
    eval(a, C, R),
    assertion(functor(R,'[classad]',1)),
    eval(b, R, R2),
    assertion(R2 == 0).

test('list 0') :-
    parse("[a = {};]", C),
    eval(a, C, R),
    assertion(R == []).

test('list 1') :-
    parse("[a = {0};]", C),
    eval(a, C, R),
    assertion(R == [0]).

test('list 2') :-
    parse("[a = {0, true};]", C),
    eval(a, C, R),
    assertion(R == [0, true]).

test('list 3') :-
    parse("[x = 42; a = {0, true, x};]", C),
    eval(a, C, R),
    assertion(R == [0, true, 42]).

test('undefined var 1') :-
    parse("[]", C),
    eval(a, C, R),
    assertion(R == undefined).

test('select 1') :-
    parse("[a = [b=0;];]", C),
    eval(as_expr "a.b", C, R),
    assertion(R == 0).

test('select 2') :-
    parse("[a = [b=[c=42;];];]", C),
    eval(as_expr "a.b.c", C, R),
    assertion(R == 42).

test('parent 1') :-
    parse("[a = [b=parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    assertion(R == 42).

test('parent 2') :-
    parse("[a = [b=[c=parent.parent.x; x=4;]; x=2;]; x = 42;]", C),
    eval(as_expr "a.b.c", C, R),
    assertion(R == 42).

:- end_tests(classad_eval_ut).
