:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).

:- add_to_path('../lib').
:- use_module(classad_parser).
:- use_module(classad_eval).

:- begin_tests(parser).

test('ident expr') :-
    parse("a", E),
    E == 'a'.

test('str expr') :-
    parse("\"a\"", E),
    E == '[str]'('a').

test('num expr') :-
    parse("42", E),
    E == 42.

test('func 0') :-
    parse("f()", E),
    E == '[func]'(f,[]).

test('func 1') :-
    parse("f(a)", E),
    E == '[func]'(f,['a']).

test('func 2') :-
    parse("f(a, 1)", E),
    E == '[func]'(f, ['a', 1]).

test('paren') :-
    parse("(a)", E),
    E == 'a'.

test('op !') :-
    parse("!true", E),
    E == '!'('true').

test('op -') :-
    parse("-1.0", E),
    E == '-'(1.0).

test('op +') :-
    parse("+1.0", E),
    E == '+'(1.0).

test('composed unary 1') :-
    parse("--1.0", E),
    E == '-'('-'(1.0)).

test('composed unary 2') :-
    parse("!+--1.0", E),
    E == '!'('+'('-'('-'(1.0)))).

test('arg nesting') :-
    parse("f(g(-1), -(-2))", E),
    E == '[func]'(f,['[func]'(g,['-'(1)]), '-'('-'(2))]).

test('* seq 1') :-
    parse("2 * a", E),
    E == '*'(2,a).

test('/ seq 1') :-
    parse("2 / a", E),
    E == '/'(2,a).

test('*/ seq 1') :-
    parse("2 * b / a", E),
    E == '/'('*'(2,b), a).

test('*/ seq 2') :-
    parse("-2 * b / +a", E),
    E == '/'('*'('-'(2),b), '+'(a)).

test('*/ seq 3') :-
    parse("-2 * f(b) / +a", E),
    E == '/'('*'('-'(2),'[func]'(f,[b])), '+'(a)).

test('+ seq 1') :-
    parse("2+a", E),
    E == '+'(2,a).

test('- seq 1') :-
    parse("2-a", E),
    E == '-'(2,a).

test('+- seq 1') :-
    parse("2-a+b", E),
    E == '+'('-'(2,a), b).

test('+- seq 2') :-
    parse("2*a-a/3+5*b", E),
    E == '+'('-'('*'(2,a),'/'(a,3)), '*'(5,b)).

test('+- seq 3') :-
    parse("2*a-a/-3+-5*b", E),
    E == '+'('-'('*'(2,a),'/'(a,'-'(3))), '*'('-'(5),b)).

test('comp ==') :-
    parse("a   ==b", E),
    E == '=='(a,b).

test('comp =?=') :-
    parse("name =?= \"fred\"", E),
    E == '=?='(name, '[str]'(fred)).

test('comp <') :-
    parse("-2*a   <   b + -4 - c", E),
    E == '<'('*'('-'(2),a), '-'('+'(b, '-'(4)), c)).

test('and 1') :-
    parse("true && false", E),
    E == '&&'(true, false).

test('and 2') :-
    parse("true && false && b", E),
    E == '&&'('&&'(true, false), b).

test('or 1') :-
    parse("true || false", E),
    E == '||'(true, false).

test('or 2') :-
    parse("true || false || b", E),
    E == '||'('||'(true, false), b).

test('or and 1') :-
    parse("2 < 3  &&  3 > 2   ||   1+2 < 1+3  &&  2+3 < 2*3", E),
    E == '||'('&&'('<'(2,3), '>'(3, 2)), '&&'('<'('+'(1,2), '+'(1,3)),'<'('+'(2,3), '*'(2,3)))).

test('index 1') :-
    parse("a[0]", E),
    E == '[]'(a, 0).

test('index 2') :-
    parse("a[0][1]", E),
    E == '[]'('[]'(a, 0), 1).

test('select 1') :-
    parse("a.b", E),
    E == '[sel]'(a,b).

test('select 2') :-
    parse("a.b.c", E),
    E == '[sel]'('[sel]'(a,b), c).

test('select 3') :-
    parse("parent.parent.c", E),
    E == '[sel]'('[sel]'(parent,parent), c).

test('index and select 1') :-
    parse("a.b.c[j+1]", E),
    E == '[]'('[sel]'('[sel]'(a,b), c), '+'(j,1)).

test('list 0') :-
    parse("{}", E),
    E == [].

test('list 1') :-
    parse("{1}", E),
    E == [1].

test('list 2') :-
    parse("  {1  ,  1 + e  }  ", E),
    E == [1, '+'(1,e)].

test('classad 0') :-
    parse("[]", E),
    E = '[classad]'(M), assoc_to_list(M, L),
    L == [].

test('classad 1') :-
    parse("[x=0;]", E),
    E = '[classad]'(M), assoc_to_list(M, L),
    L == [x-0].

test('classad 2') :-
    parse("[x=0; y=2*x;]", E),
    E = '[classad]'(M), assoc_to_list(M, L),
    L == [x-0, y-'*'(2,x)].

test('classad 3') :-
    parse("[x=0; y=2*x; z=[a=0;];]", E),
    E = '[classad]'(M), assoc_to_list(M, L),
    [M1, M2, z-'[classad]'(MM)] = L, assoc_to_list(MM, LL),
    M1 == x-0,
    M2 == y-'*'(2,x),
    LL == [a-0].

test('classad 4') :-
    parse("[x=0]", E),
    E = '[classad]'(M), assoc_to_list(M, L),
    L == [x-0].

test('classad 5') :-
    parse("[x=0; y=2*x]", E),
    E = '[classad]'(M), assoc_to_list(M, L),
    L == [x-0, y-'*'(2,x)].

test('conditional 1') :-
    parse("(a <= 0) ? 0 : 2*a", E),
    E == '?:'('<='(a, 0), 0, '*'(2,a)). 

test('conditional 2') :-
    parse("a || b  ?  c || d ? 0 : 1   :  e || f", E),
    E == '?:'('||'(a,b), '?:'('||'(c,d), 0, 1), '||'(e,f)). 

test('op| 1') :-
    parse("x | y | z", E),
    E == '|'('|'(x,y),z).

test('op^ 1') :-
    parse("x ^ y ^ z", E),
    E == '^'('^'(x,y),z).

test('op& 1') :-
    parse("x & y & z", E),
    E == '&'('&'(x,y),z).

test('op~ 1') :-
    parse("~~x", E),
    E == '~'('~'(x)).

test('bitwise 1') :-
    parse("x & ~y ^ z | ~w ^ y & z", E),
    E == '|'('^'('&'(x,'~'(y)), z),'^'('~'(w),'&'(y,z))).

test('macro-1') :-
    parse("[x=1; y=$x; x=2; z=$x;]", '[classad]'(M)),
    assoc_to_list(M, [x-2, y-1, z-2]).

test('macro-2') :-
    parse("[x=1; y=$x; x=2; z=$(x+y)]", '[classad]'(M)),
    assoc_to_list(M, [x-2, y-1, z-3]).

test('macro-3') :-
    parse("[x=1; y=$x; x=2; ca=[x=4; z=$(x+y)]; z=$(x+y);]", '[classad]'(M)),
    assoc_to_list(M, [ca-'[classad]'(M2), x-2, y-1, z-3]),
    assoc_to_list(M2, [x-4, z-5]).

test('macro-4') :-
    parse("
[
  x = 5;
  y = [ x = 6; y = $(x + 1); ];
  z = [ y = $(parent.x) ]
]
", '[classad]'(M)),
assoc_to_list(M, [x-5, y-'[classad]'(MY), z-'[classad]'(MZ)]),
assoc_to_list(MY, [x-6, y-7]),
assoc_to_list(MZ, [y-5]).


test('macro-5') :-
    get_time(T0),
    parse("[x = $(time())]", '[classad]'(M)),
    get_time(T1),
    assoc_to_list(M, [x-T]),
    T0 =< T, T =< T1.


test('macro-6') :-
parse("
[
  x = 3;
  y = [ x = 4 ];
  z = $x + $y.x; # should evaluate to 7?
  x = $x;
]
", CA),
classad_eval(z, CA, 7), 
classad_eval(x, CA, 3).

:- end_tests(parser).
