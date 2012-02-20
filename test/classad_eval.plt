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
    R == error.

test('undefined 1') :-
    parse("[a = undefined;]", C),
    eval(a, C, R),
    R == undefined.

test('true 1') :-
    parse("[a = true;]", C),
    eval(a, C, R),
    R == true.

test('false 1') :-
    parse("[a = false;]", C),
    eval(a, C, R),
    R == false.

test('number 1') :-
    parse("[a = 0;]", C),
    eval(a, C, R),
    R == 0.

test('string 1') :-
    parse("[a = \"fred and wilma\";]", C),
    eval(a, C, R),
    R == '[str]'('fred and wilma').

test('classad 1') :-
    parse("[a = [b=0;];]", C),
    eval(a, C, R),
    functor(R,'[classad]',1),
    eval(b, R, R2),
    R2 == 0.

test('list 0') :-
    parse("[a = {};]", C),
    eval(a, C, R),
    R == [].

test('list 1') :-
    parse("[a = {0};]", C),
    eval(a, C, R),
    R == [0].

test('list 2') :-
    parse("[a = {0, true};]", C),
    eval(a, C, R),
    R == [0, true].

test('list 3') :-
    parse("[x = 42; a = {0, true, x};]", C),
    eval(a, C, R),
    R == [0, true, 42].

test('undefined var 1') :-
    parse("[]", C),
    eval(a, C, R),
    R == undefined.

test('select 1') :-
    parse("[a = [b=0;];]", C),
    eval(as_expr "a.b", C, R),
    R == 0.

test('select 2') :-
    parse("[a = [b=[c=42;];];]", C),
    eval(as_expr "a.b.c", C, R),
    R == 42.

test('select 3') :-
    parse("[a = [b=[c=42;];];]", C),
    eval(as_expr "a.z.c", C, R),
    R == undefined.

test('select 4') :-
    parse("[a = [z=0; b=[c=42;];];]", C),
    eval(as_expr "a.z.c", C, R),
    R == error.

test('parent 1') :-
    parse("[a = [b=parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    R == 42.

test('parent 2') :-
    parse("[a = [b=[c=parent.parent.x; x=4;]; x=2;]; x = 42;]", C),
    eval(as_expr "a.b.c", C, R),
    R == 42.

test('parent 3') :-
    parse("[a = [b=parent.parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    R == undefined.

test('parent 4') :-
    parse("[a = [b=parent.parent.parent.x; x=4;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    R == undefined.

test('var context pop 1') :-
    parse("[a = [b=x;]; x = 42;]", C),
    eval(as_expr "a.b", C, R),
    R == 42.

test('var cyclic 1') :-
    parse("[a = b; b = a;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('var cyclic 2') :-
    parse("[a = b+c; b = 1; c = 2 * d; d = a;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('add 1') :-
    parse("[a = 1 + 2;]", C),
    eval(as_expr "a", C, R),
    R == 3.

test('add 2') :-
    parse("[a = 1 + x; x=3.0;]", C),
    eval(as_expr "a", C, R),
    R == 4.0.

test('add 3') :-
    parse("[a = 1 + x; x=3.0;]", C),
    eval(as_expr "a", C, R),
    R == 4.0.

test('add 4') :-
    parse("[a = 1 + x;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('add 5') :-
    parse("[a = 1 + x; x = true;]", C),
    eval(as_expr "a", C, R),
    R == 2.

test('add 6') :-
    parse("[a = 1 + x; x = \"s\";]", C),
    eval(as_expr "a", C, R),
    R == error.

test('sub 1') :-
    parse("[a = 3 - 2;]", C),
    eval(as_expr "a", C, R),
    R == 1.

test('mul 1') :-
    parse("[a = 3.0 * 2;]", C),
    eval(as_expr "a", C, R),
    R == 6.0.

test('mul 2') :-
    parse("[a = 3 * 2;]", C),
    eval(as_expr "a", C, R),
    R == 6.

test('divide 1') :-
    parse("[a = 10 / 2;]", C),
    eval(as_expr "a", C, R),
    R == 5.

test('divide 2') :-
    parse("[a = 10 / 2.0;]", C),
    eval(as_expr "a", C, R),
    R == 5.0.

test('divide 3') :-
    parse("[a = 10 / 0;]", C),
    eval(as_expr "a", C, R),
    R == error.

test('divide 4') :-
    parse("[a = 10 / 0.0;]", C),
    eval(as_expr "a", C, R),
    R == error.

test('mod 1') :-
    parse("[a = 5 % 3;]", C),
    eval(as_expr "a", C, R),
    R == 2.

test('mod 2') :-
    parse("[a = 5 % -3;]", C),
    eval(as_expr "a", C, R),
    R == -1.

test('mod 3') :-
    parse("[a = -5 % 3;]", C),
    eval(as_expr "a", C, R),
    R == 1.

test('mod 4') :-
    parse("[a = -5 % -3;]", C),
    eval(as_expr "a", C, R),
    R == -2.

test('mod 5') :-
    parse("[a = 5.0 % 3;]", C),
    eval(as_expr "a", C, R),
    R == 2.0.

test('mod 6') :-
    parse("[a = 5 % -3.0;]", C),
    eval(as_expr "a", C, R),
    R == -1.0.

test('mod 7') :-
    parse("[a = -5 % 3.0;]", C),
    eval(as_expr "a", C, R),
    R == 1.0.

test('mod 8') :-
    parse("[a = -5.0 % -3;]", C),
    eval(as_expr "a", C, R),
    R == -2.0.

test('mod 9') :-
    parse("[a = 5 % 0;]", C),
    eval(as_expr "a", C, R),
    R == error.

test('mod 10') :-
    parse("[a = 5 % 0.0;]", C),
    eval(as_expr "a", C, R),
    R == error.

test('&& 1') :-
    parse("[a = true && true;]", C),
    eval(as_expr "a", C, R),
    R == true.

test('&& 2') :-
    parse("[a = false && b;]", C),
    eval(as_expr "a", C, R),
    R == false.

test('&& 3') :-
    parse("[a = 0 && \"z\";]", C),
    eval(as_expr "a", C, R),
    R == false.

test('&& 4') :-
    parse("[a = 1 && \"z\";]", C),
    eval(as_expr "a", C, R),
    R == error.

test('&& 5') :-
    parse("[a = \"z\" && false;]", C),
    eval(as_expr "a", C, R),
    R == error.

test('&& 6') :-
    parse("[a = 1.0 && b;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('&& 7') :-
    parse("[a = b && 1.0;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('&& 8') :-
    parse("[a = b && 0;]", C),
    eval(as_expr "a", C, R),
    R == false.

test('|| 1') :-
    parse("[a = false || false;]", C),
    eval(as_expr "a", C, R),
    R == false.

test('|| 2') :-
    parse("[a = true || b;]", C),
    eval(as_expr "a", C, R),
    R == true.

test('|| 3') :-
    parse("[a = 1 || \"z\";]", C),
    eval(as_expr "a", C, R),
    R == true.

test('|| 4') :-
    parse("[a = 0 || \"z\";]", C),
    eval(as_expr "a", C, R),
    R == error.

test('|| 5') :-
    parse("[a = \"z\" || true;]", C),
    eval(as_expr "a", C, R),
    R == error.

test('|| 6') :-
    parse("[a = 0.0 || b;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('|| 7') :-
    parse("[a = b || 0.0;]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('|| 8') :-
    parse("[a = b || 1;]", C),
    eval(as_expr "a", C, R),
    R == true.

test('promotion 1') :-
    parse("[a = true + true]", C),
    eval(as_expr "a", C, R),
    R == 2.

test('promotion 2') :-
    parse("[a = 4.0 * true]", C),
    eval(as_expr "a", C, R),
    R == 4.0.

test('promotion 3') :-
    parse("[a = 4.0 * false]", C),
    eval(as_expr "a", C, R),
    R == 0.0.

test('! op 1') :-
    parse("[a = !true]", C),
    eval(as_expr "a", C, R),
    R == false.

test('! op 2') :-
    parse("[a = !false]", C),
    eval(as_expr "a", C, R),
    R == true.

test('! op 3') :-
    parse("[a = !0]", C),
    eval(as_expr "a", C, R),
    R == true.

test('! op 4') :-
    parse("[a = !1]", C),
    eval(as_expr "a", C, R),
    R == false.

test('! op 5') :-
    parse("[a = !\"z\"]", C),
    eval(as_expr "a", C, R),
    R == error.

test('! op 6') :-
    parse("[a = !b]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('?: op 1') :-
    parse("[a = (true) ? 2.0 : true]", C),
    eval(as_expr "a", C, R),
    R == 2.0.

test('?: op 2') :-
    parse("[a = (false) ? 2.0 : true]", C),
    eval(as_expr "a", C, R),
    R == true.

test('?: op 3') :-
    parse("[a = (b) ? 2.0 : true]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('?: op 4') :-
    parse("[a = (\"z\") ? 2.0 : true]", C),
    eval(as_expr "a", C, R),
    R == error.

test('?: op 5') :-
    parse("[r = [a = ((true) ? parent : q).z; q = [z=42]]; z=69]", C),
    eval(as_expr "r.a", C, R),
    R == 69.

test('?: op 6') :-
    parse("[r = [a = ((false) ? parent : q).z; q = [z=42]]; z=69]", C),
    eval(as_expr "r.a", C, R),
    R == 42.

test('op== 1') :-
    parse("[a = 7 == 7]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op== 2') :-
    parse("[a = 6 == 7]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op== 3') :-
    parse("[a = 7.0 == 7]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op== 4') :-
    parse("[a = \"z\" == \"z\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op== 5') :-
    parse("[a = \"a\" == \"z\"]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op== 6') :-
    parse("[a = \"z\" == 1]", C),
    eval(as_expr "a", C, R),
    R == error.

test('op== 7') :-
    parse("[a = \"z\" == \"z\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op== 8') :-
    parse("[a = \"z\" == b]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('op!= 1') :-
    parse("[a =  1 != 0]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op!= 2') :-
    parse("[a =  1 != 1.0]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op!= 3') :-
    parse("[a =  \"a\" != \"b\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op!= 4') :-
    parse("[a =  \"a\" != \"a\"]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op< 1') :-
    parse("[a =  1 < 2]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op< 2') :-
    parse("[a =  1 < 1.0]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op< 3') :-
    parse("[a =  \"a\" < \"b\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op< 4') :-
    parse("[a =  \"a\" < \"a\"]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op> 1') :-
    parse("[a =  1 > 0]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op> 2') :-
    parse("[a =  1 > 1.0]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op> 3') :-
    parse("[a =  \"b\" > \"a\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op> 4') :-
    parse("[a =  \"a\" > \"a\"]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op<= 1') :-
    parse("[a =  1 <= 2]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op<= 2') :-
    parse("[a =  1 <= 1.0]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op<= 3') :-
    parse("[a =  2 <= 1.0]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op<= 4') :-
    parse("[a =  \"a\" <= \"b\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op<= 5') :-
    parse("[a =  \"a\" <= \"a\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op<= 6') :-
    parse("[a =  \"b\" <= \"a\"]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op>= 1') :-
    parse("[a =  1 >= 2]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op>= 2') :-
    parse("[a =  1 >= 1.0]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op>= 3') :-
    parse("[a =  2 >= 1.0]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op>= 4') :-
    parse("[a =  \"a\" >= \"b\"]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op>= 5') :-
    parse("[a =  \"a\" >= \"a\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op>= 6') :-
    parse("[a =  \"b\" >= \"a\"]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op=?= 1') :-
    parse("[a = 1 =?= 1]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op=?= 2') :-
    parse("[a = 1 =?= b]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op=?= 3') :-
    parse("[a = 1 =?= 2]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op=!= 1') :-
    parse("[a = 1 =!= 1]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op=!= 2') :-
    parse("[a = 1 =!= b]", C),
    eval(as_expr "a", C, R),
    R == false.

test('op=!= 3') :-
    parse("[a = 1 =!= 2]", C),
    eval(as_expr "a", C, R),
    R == true.

test('op[] 1') :-
    parse("[a = {1}[0]]", C),
    eval(as_expr "a", C, R),
    R == 1.

test('op[] 2') :-
    parse("[a = [z=42][\"z\"]; z=69]", C),
    eval(as_expr "a", C, R),
    R == 42.

test('op[] 3') :-
    parse("[a = {1}[3]]", C),
    eval(as_expr "a", C, R),
    R == error.

test('op[] 4') :-
    parse("[a = {[z=42]}[\"z\"]; z=69]", C),
    eval(as_expr "a", C, R),
    R == [42].

test('op[] 5') :-
    parse("[a = {}[\"z\"]; z=69]", C),
    eval(as_expr "a", C, R),
    R == [].

test('op[] 6') :-
    parse("[a = {1}[\"z\"]]", C),
    eval(as_expr "a", C, R),
    R == [error].

test('op[] 7') :-
    parse("[a = {[x=1]}[\"z\"]]", C),
    eval(as_expr "a", C, R),
    R == [undefined].

test('op[] 8') :-
    parse("[a = {}[0]]", C),
    eval(as_expr "a", C, R),
    R == error.

test('op[] 9') :-
    parse("[a = {0}[-1]]", C),
    eval(as_expr "a", C, R),
    R == error.

test('op[] 10') :-
    parse("[a = b[0]]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('op[] 11') :-
    parse("[a = {1}[b]]", C),
    eval(as_expr "a", C, R),
    R == undefined.

test('op[] 12') :-
    parse("[a = {{3},{4}}[1][0]]", C),
    eval(as_expr "a", C, R),
    R == 4.

test('op[] 13') :-
    parse("[a = [b=[c=55];c=44]; c=33; d=a[\"b\"][\"c\"]]", C),
    eval(as_expr "d", C, R),
    R == 55.

test('op[] 14') :-
    parse("[a = [b=[c=parent.c];c=44]; c=33; d=a[\"b\"][\"c\"]]", C),
    eval(as_expr "d", C, R),
    R == 44.

test('op[] 15') :-
    parse("[a = [b=[c=parent.c];c=44]; c=33; d=a[\"parent\"][\"c\"]]", C),
    eval(as_expr "d", C, R),
    R == 33.

:- end_tests(classad_eval_ut).
