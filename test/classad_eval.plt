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

test('select 3') :-
    parse("[a = [b=[c=42;];];]", C),
    eval(as_expr "a.z.c", C, R),
    assertion(R == undefined).

test('parent 1') :-
    parse("[a = [b=parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    assertion(R == 42).

test('parent 2') :-
    parse("[a = [b=[c=parent.parent.x; x=4;]; x=2;]; x = 42;]", C),
    eval(as_expr "a.b.c", C, R),
    assertion(R == 42).

test('parent 3') :-
    parse("[a = [b=parent.parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    assertion(R == undefined).

test('parent 4') :-
    parse("[a = [b=parent.parent.parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    assertion(R == undefined).

test('add 1') :-
    parse("[a = 1 + 2;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 3).

test('add 2') :-
    parse("[a = 1 + x; x=3.0;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 4.0).

test('add 3') :-
    parse("[a = 1 + x; x=3.0;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 4.0).

test('add 4') :-
    parse("[a = 1 + x;]", C),
    eval(as_expr "a", C, R),
    assertion(R == undefined).

test('add 5') :-
    parse("[a = 1 + x; x = true;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 2).

test('add 6') :-
    parse("[a = 1 + x; x = \"s\";]", C),
    eval(as_expr "a", C, R),
    assertion(R == error).

test('sub 1') :-
    parse("[a = 3 - 2;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 1).

test('mul 1') :-
    parse("[a = 3.0 * 2;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 6.0).

test('mul 2') :-
    parse("[a = 3 * 2;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 6).

test('divide 1') :-
    parse("[a = 10 / 2;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 5).

test('divide 2') :-
    parse("[a = 10 / 2.0;]", C),
    eval(as_expr "a", C, R),
    assertion(R == 5.0).

:- end_tests(classad_eval_ut).
