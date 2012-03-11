:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).

:- add_to_path('../lib').
:- use_module(classad_lexer).

:- begin_tests(lexer).

test('empty string') :-
    lex("", T),
    T == [].

test('whitespace only') :-
    lex(" \t ", T),
    T == [].

test('string token') :-
    lex("\"a string\"", T),
    T == ['[str]'('a string')].

test('integer') :-
    lex("42", T),
    T == [42].

test('floating point') :-
    lex("3.14", T),
    T == [3.14].

test('floating point') :-
    lex("3.", T),
    T == [3.0].

test('exp notation') :-
    lex("31.4e-1", T),
    T == [3.14].

test('exp notation 2') :-
    lex("1e1", T),
    T == [10.0].

test('exp notation 3') :-
    lex("1.e+1", T),
    T == [10.0].

test('error') :-
    lex("error", [error]).

test('undefined') :-
    lex("undefined", [undefined]).

test('true') :-
    lex("true", [true]).

test('false') :-
    lex("false", [false]).

test('parent') :-
    lex("parent", [parent]).

test('is') :-
    lex("is", [is]).

test('isnt') :-
    lex("isnt", [isnt]).

test('variable') :-
    lex("a", T),
    T == ['[id]'('a')].

test('variable 2') :-
    lex("a2", T),
    T == ['[id]'('a2')].

test('variable 3') :-
    lex("a2 b4", T),
    T == ['[id]'('a2'), '[id]'('b4')].

test('variables with whitespace') :-
    lex(" a2 b4 ", T),
    T == ['[id]'('a2'), '[id]'('b4')].

test('symbol =?=') :-
    lex("=?=", T),
    T == ['=?='].

test('symbol ==') :-
    lex("==", T),
    T == ['=='].

test('all symbols') :-
    lex("=?==!=>>>||&&<=>===!=>><<()[]{},<>+-*/!:=?;.|^&~$", T),
    T == ['=?=','=!=','>>>','||','&&','<=','>=','==','!=','>>','<<','(',')','[',']','{','}',',','<','>','+','-','*','/','!',':','=','?',';','.','|','^','&','~', '$'].

test('expression 1') :-
    lex("4*a+b-c/4e1*e", T),
    T == [4, '*', '[id]'('a'), '+', '[id]'('b'), '-', '[id]'('c'), '/', 40.0, '*', '[id]'('e')].

test('expression 2') :-
    lex("ifthenelse(name =!= \"fred\", name, \"wilma\")", T),
    T == ['[id]'('ifthenelse'), '(', '[id]'('name'), '=!=', '[str]'('fred'), ',', '[id]'('name'), ',', '[str]'('wilma'), ')'].

test('identifiers') :-
    lex("Scope._CamelCase_Ident", T),
    T == ['[id]'('scope'), '.', '[id]'('_camelcase_ident')].

test('whitespace nl and comments') :-
    lex("
        x  # left operand
        +  # operator
        y  # right operand
        ",
        ['[id]'('x'), '+', '[id]'('y')]).

test('string variations') :-
    lex("'single \"quote\" string'\"double 'quote' string\"",
        ['[str]'('single "quote" string'), '[str]'('double \'quote\' string')]).

:- end_tests(lexer).
