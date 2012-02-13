:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).

:- use_module('../lib/classad_lexer.pl').

:- begin_tests(classad_lexer_ut).

test('empty string') :-
    lex("", T),
    assertion(T == []).

test('whitespace only') :-
    lex(" \t ", T),
    assertion(T == []).

test('string token') :-
    lex("\"a string\"", T),
    assertion(T == ['[str]'('a string')]).

test('integer') :-
    lex("42", T),
    assertion(T == [42]).

test('floating point') :-
    lex("3.14", T),
    assertion(T == [3.14]).

test('floating point') :-
    lex("3.", T),
    assertion(T == [3.0]).

test('exp notation') :-
    lex("31.4e-1", T),
    assertion(T == [3.14]).

test('exp notation 2') :-
    lex("1e1", T),
    assertion(T == [10.0]).

test('exp notation 3') :-
    lex("1.e+1", T),
    assertion(T == [10.0]).

test('variable') :-
    lex("a", T),
    assertion(T == ['a']).

test('variable 2') :-
    lex("a2", T),
    assertion(T == ['a2']).

test('variable 3') :-
    lex("a2 b4", T),
    assertion(T == ['a2', 'b4']).

test('variables with whitespace') :-
    lex(" a2 b4 ", T),
    assertion(T == ['a2', 'b4']).

test('symbol =?=') :-
    lex("=?=", T),
    assertion(T == ['=?=']).

test('symbol ==') :-
    lex("==", T),
    assertion(T == ['==']).

test('all symbols') :-
    lex("=?==!=||&&<=>===!=()[]{},<>+-*/!:=?;.", T),
    assertion(T == ['=?=','=!=','||','&&','<=','>=','==','!=','(',')','[',']','{','}',',','<','>','+','-','*','/','!',':','=','?',';','.']).

test('expression 1') :-
    lex("4*a+b-c/4e1*e", T),
    assertion(T == [4, '*', 'a', '+', 'b', '-', 'c', '/', 40.0, '*', 'e']).

test('expression 2') :-
    lex("ifthenelse(name =!= \"fred\", name, \"wilma\")", T),
    assertion(T == ['ifthenelse', '(', 'name', '=!=', '[str]'('fred'), ',', 'name', ',', '[str]'('wilma'), ')']).

test('identifiers') :-
    lex("Scope._CamelCase_Ident", T),
    assertion(T == ['scope', '.', '_camelcase_ident']).

:- end_tests(classad_lexer_ut).
