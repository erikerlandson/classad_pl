:- expects_dialect(swi).

:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(date)).

:- add_to_path('../lib').
:- use_module(classad_parser).
:- use_module(classad_eval).

local_tzo(Z) :- stamp_date_time(0, DT, local), date_time_value(utc_offset, DT, Z).

:- begin_tests(eval).

test('error 1') :-
    parse("[a = error;]", C),
    classad_eval(a, C, R),
    R == error.

test('undefined 1') :-
    parse("[a = undefined;]", C),
    classad_eval(a, C, R),
    R == undefined.

test('true 1') :-
    parse("[a = true;]", C),
    classad_eval(a, C, R),
    R == true.

test('false 1') :-
    parse("[a = false;]", C),
    classad_eval(a, C, R),
    R == false.

test('number 1') :-
    parse("[a = 0;]", C),
    classad_eval(a, C, R),
    R == 0.

test('string 1') :-
    parse("[a = \"fred and wilma\";]", C),
    classad_eval(a, C, R),
    R == '[str]'('fred and wilma').

test('classad 1') :-
    parse("[a = [b=0;];]", C),
    classad_eval(a, C, R),
    functor(R,'[classad]',1),
    classad_eval(b, R, R2),
    R2 == 0.

test('list 0') :-
    parse("[a = {};]", C),
    classad_eval(a, C, R),
    R == [].

test('list 1') :-
    parse("[a = {0};]", C),
    classad_eval(a, C, R),
    R == [0].

test('list 2') :-
    parse("[a = {0, true};]", C),
    classad_eval(a, C, R),
    R == [0, true].

test('list 3') :-
    parse("[x = 42; a = {0, true, x};]", C),
    classad_eval(a, C, R),
    R == [0, true, 42].

test('undefined var 1') :-
    parse("[]", C),
    classad_eval(a, C, R),
    R == undefined.

test('vars case insensitive 1') :-
    parse("[Abc = 42]", C),
    classad_eval_native("abc", C, 42),
    classad_eval_native("Abc", C, 42),
    classad_eval_native("aBc", C, 42).

test('select 1') :-
    parse("[a = [b=0;];]", C),
    classad_eval_native("a.b", C, R),
    R == 0.

test('select 2') :-
    parse("[a = [b=[c=42;];];]", C),
    classad_eval_native("a.b.c", C, R),
    R == 42.

test('select 3') :-
    parse("[a = [b=[c=42;];];]", C),
    classad_eval_native("a.z.c", C, R),
    R == undefined.

test('select 4') :-
    parse("[a = [z=0; b=[c=42;];];]", C),
    classad_eval_native("a.z.c", C, R),
    R == error.

test('parent 1') :-
    parse("[a = [b=parent.x; x=4;]; x = 42;]", C),
    classad_eval_native("a.b", C, R),
    R == 42.

test('parent 2') :-
    parse("[a = [b=[c=parent.parent.x; x=4;]; x=2;]; x = 42;]", C),
    classad_eval_native("a.b.c", C, R),
    R == 42.

test('parent 3') :-
    parse("[a = [b=parent.parent.x; x=4;]; x = 42;]", C),
    classad_eval_native("a.b", C, R),
    R == undefined.

test('parent 4') :-
    parse("[a = [b=parent.parent.parent.x; x=4;]; x = 42;]", C),
    classad_eval_native("a.b", C, R),
    R == undefined.

test('var context pop 1') :-
    parse("[a = [b=x;]; x = 42;]", C),
    classad_eval_native("a.b", C, R),
    R == 42.

test('var cyclic 1') :-
    parse("[a = b; b = a;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('var cyclic 2') :-
    parse("[a = b+c; b = 1; c = 2 * d; d = a;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('add 1') :-
    parse("[a = 1 + 2;]", C),
    classad_eval_native("a", C, R),
    R == 3.

test('add 2') :-
    parse("[a = 1 + x; x=3.0;]", C),
    classad_eval_native("a", C, R),
    R == 4.0.

test('add 3') :-
    parse("[a = 1 + x; x=3.0;]", C),
    classad_eval_native("a", C, R),
    R == 4.0.

test('add 4') :-
    parse("[a = 1 + x;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('add 5') :-
    parse("[a = 1 + x; x = true;]", C),
    classad_eval_native("a", C, R),
    R == 2.

test('add 6') :-
    parse("[a = 1 + x; x = \"s\";]", C),
    classad_eval_native("a", C, R),
    R == error.

test('add 7') :-
    parse("[a = abstime(\"2012-02\") + reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R = '[abstime]'(T,_),
    parse_time('2012-02', S),
    T =:= S+1.

test('add 8') :-
    parse("[a = reltime(\"1s\") + abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R = '[abstime]'(T,_),
    parse_time('2012-02', S),
    T =:= S+1.

test('add 9') :-
    parse("[a = reltime(\"1s\") + reltime(\"1m\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(T),
    T =:= 61.

test('add 10') :-
    parse("[a = abstime(\"2012-02\") + abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == error.

test('add 11') :-
    parse("[a = abstime(\"2012-02\") + abstime(b)]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('add str') :-
    classad_eval_native("'fred' + ' loves ' + '' + 'wilma'", [], '[str]'('fred loves wilma')).

test('add list') :-
    classad_eval_native("{2} + {} + {3, 5}", [], [2,3,5]).

test('sub 1') :-
    parse("[a = 3 - 2;]", C),
    classad_eval_native("a", C, R),
    R == 1.

test('sub 2') :-
    parse("[a = abstime(\"2012-02-10\") - abstime(\"2012-02-09\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(T),
    T =:= 86400.

test('sub 3') :-
    parse("[a = abstime(\"2012-02\") - reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R = '[abstime]'(T,_),
    parse_time('2012-02', S),
    T =:= S-1.

test('sub 4') :-
    parse("[a = reltime(\"1h\") - reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(T),
    T =:= 3599.

test('sub 5') :-
    parse("[a = reltime(\"1h\") - abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == error.

test('sub 6') :-
    parse("[a = reltime(\"1h\") - abstime(b)]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('mul 1') :-
    parse("[a = 3.0 * 2;]", C),
    classad_eval_native("a", C, R),
    R == 6.0.

test('mul 2') :-
    parse("[a = 3 * 2;]", C),
    classad_eval_native("a", C, R),
    R == 6.

test('divide 1') :-
    parse("[a = 10 / 2;]", C),
    classad_eval_native("a", C, R),
    R == 5.

test('divide 2') :-
    parse("[a = 10 / 2.0;]", C),
    classad_eval_native("a", C, R),
    R == 5.0.

test('divide 3') :-
    parse("[a = 10 / 0;]", C),
    classad_eval_native("a", C, R),
    R == error.

test('divide 4') :-
    parse("[a = 10 / 0.0;]", C),
    classad_eval_native("a", C, R),
    R == error.

test('mod 1') :-
    parse("[a = 5 % 3;]", C),
    classad_eval_native("a", C, R),
    R == 2.

test('mod 2') :-
    parse("[a = 5 % -3;]", C),
    classad_eval_native("a", C, R),
    R == -1.

test('mod 3') :-
    parse("[a = -5 % 3;]", C),
    classad_eval_native("a", C, R),
    R == 1.

test('mod 4') :-
    parse("[a = -5 % -3;]", C),
    classad_eval_native("a", C, R),
    R == -2.

test('mod 5') :-
    parse("[a = 5.0 % 3;]", C),
    classad_eval_native("a", C, R),
    R == 2.0.

test('mod 6') :-
    parse("[a = 5 % -3.0;]", C),
    classad_eval_native("a", C, R),
    R == -1.0.

test('mod 7') :-
    parse("[a = -5 % 3.0;]", C),
    classad_eval_native("a", C, R),
    R == 1.0.

test('mod 8') :-
    parse("[a = -5.0 % -3;]", C),
    classad_eval_native("a", C, R),
    R == -2.0.

test('mod 9') :-
    parse("[a = 5 % 0;]", C),
    classad_eval_native("a", C, R),
    R == error.

test('mod 10') :-
    parse("[a = 5 % 0.0;]", C),
    classad_eval_native("a", C, R),
    R == error.

test('&& 1') :-
    parse("[a = true && true;]", C),
    classad_eval_native("a", C, R),
    R == true.

test('&& 2') :-
    parse("[a = false && b;]", C),
    classad_eval_native("a", C, R),
    R == false.

test('&& 3') :-
    parse("[a = 0 && \"z\";]", C),
    classad_eval_native("a", C, R),
    R == false.

test('&& 4') :-
    parse("[a = 1 && \"z\";]", C),
    classad_eval_native("a", C, R),
    R == error.

test('&& 5') :-
    parse("[a = \"z\" && false;]", C),
    classad_eval_native("a", C, R),
    R == error.

test('&& 6') :-
    parse("[a = 1.0 && b;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('&& 7') :-
    parse("[a = b && 1.0;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('&& 8') :-
    parse("[a = b && 0;]", C),
    classad_eval_native("a", C, R),
    R == false.

test('|| 1') :-
    parse("[a = false || false;]", C),
    classad_eval_native("a", C, R),
    R == false.

test('|| 2') :-
    parse("[a = true || b;]", C),
    classad_eval_native("a", C, R),
    R == true.

test('|| 3') :-
    parse("[a = 1 || \"z\";]", C),
    classad_eval_native("a", C, R),
    R == true.

test('|| 4') :-
    parse("[a = 0 || \"z\";]", C),
    classad_eval_native("a", C, R),
    R == error.

test('|| 5') :-
    parse("[a = \"z\" || true;]", C),
    classad_eval_native("a", C, R),
    R == error.

test('|| 6') :-
    parse("[a = 0.0 || b;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('|| 7') :-
    parse("[a = b || 0.0;]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('|| 8') :-
    parse("[a = b || 1;]", C),
    classad_eval_native("a", C, R),
    R == true.

test('promotion 1') :-
    parse("[a = true + true]", C),
    classad_eval_native("a", C, R),
    R == 2.

test('promotion 2') :-
    parse("[a = 4.0 * true]", C),
    classad_eval_native("a", C, R),
    R == 4.0.

test('promotion 3') :-
    parse("[a = 4.0 * false]", C),
    classad_eval_native("a", C, R),
    R == 0.0.

test('op- 1') :-
    parse("[x = -(1+2)]", C),
    classad_eval_native("x", C, R),
    R == -3.

test('op- 2') :-
    parse("[x = -reltime(\"1m\")]", C),
    classad_eval_native("x", C, R),
    R = '[reltime]'(T),
    T =:= -60.

test('op- 3') :-
    parse("[x = -abstime(\"2012-02\")]", C),
    classad_eval_native("x", C, R),
    R == error.

test('op- 4') :-
    parse("[x = -abstime(b)]", C),
    classad_eval_native("x", C, R),
    R == undefined.

test('op+ 1') :-
    parse("[x = +(1+2)]", C),
    classad_eval_native("x", C, R),
    R == 3.

test('op+ 2') :-
    parse("[x = +reltime(\"1m\")]", C),
    classad_eval_native("x", C, R),
    R = '[reltime]'(T),
    T =:= 60.

test('op+ 3') :-
    parse("[x = +abstime(\"2012-02\")]", C),
    classad_eval_native("x", C, R),
    R = '[abstime]'(T,_),
    parse_time('2012-02', S),
    T =:= S.

test('! op 1') :-
    parse("[a = !true]", C),
    classad_eval_native("a", C, R),
    R == false.

test('! op 2') :-
    parse("[a = !false]", C),
    classad_eval_native("a", C, R),
    R == true.

test('! op 3') :-
    parse("[a = !0]", C),
    classad_eval_native("a", C, R),
    R == true.

test('! op 4') :-
    parse("[a = !1]", C),
    classad_eval_native("a", C, R),
    R == false.

test('! op 5') :-
    parse("[a = !\"z\"]", C),
    classad_eval_native("a", C, R),
    R == error.

test('! op 6') :-
    parse("[a = !b]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('?: op 1') :-
    parse("[a = (true) ? 2.0 : true]", C),
    classad_eval_native("a", C, R),
    R == 2.0.

test('?: op 2') :-
    parse("[a = (false) ? 2.0 : true]", C),
    classad_eval_native("a", C, R),
    R == true.

test('?: op 3') :-
    parse("[a = (b) ? 2.0 : true]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('?: op 4') :-
    parse("[a = (\"z\") ? 2.0 : true]", C),
    classad_eval_native("a", C, R),
    R == error.

test('?: op 5') :-
    parse("[r = [a = ((true) ? parent : q).z; q = [z=42]]; z=69]", C),
    classad_eval_native("r.a", C, R),
    R == 69.

test('?: op 6') :-
    parse("[r = [a = ((false) ? parent : q).z; q = [z=42]]; z=69]", C),
    classad_eval_native("r.a", C, R),
    R == 42.

test('op== 1') :-
    parse("[a = 7 == 7]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op== 2') :-
    parse("[a = 6 == 7]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op== 3') :-
    parse("[a = 7.0 == 7]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op== 4') :-
    parse("[a = \"z\" == \"z\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op== 5') :-
    parse("[a = \"a\" == \"z\"]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op== 6') :-
    parse("[a = \"z\" == 1]", C),
    classad_eval_native("a", C, R),
    R == error.

test('op== 7') :-
    parse("[a = \"z\" == \"z\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op== 8') :-
    parse("[a = \"z\" == b]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('op== 9') :-
    parse("[a = abstime(\"2012-02\")==abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op== 10') :-
    parse("[a = abstime(\"2012-02\")==abstime(\"2012-03\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op== 11') :-
    parse("[a = reltime(\"1h\")==reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op== 12') :-
    parse("[a = reltime(\"2h\")==reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op!= 1') :-
    parse("[a =  1 != 0]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op!= 2') :-
    parse("[a =  1 != 1.0]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op!= 3') :-
    parse("[a =  \"a\" != \"b\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op!= 4') :-
    parse("[a =  \"a\" != \"a\"]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op!= 5') :-
    parse("[a = abstime(\"2012-02\")!=abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op!= 6') :-
    parse("[a = abstime(\"2012-02\")!=abstime(\"2012-03\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op!= 7') :-
    parse("[a = reltime(\"1h\")!=reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op!= 8') :-
    parse("[a = reltime(\"2h\")!=reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op< 1') :-
    parse("[a =  1 < 2]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op< 2') :-
    parse("[a =  1 < 1.0]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op< 3') :-
    parse("[a =  \"a\" < \"b\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op< 4') :-
    parse("[a =  \"a\" < \"a\"]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op< 5') :-
    parse("[a = abstime(\"2012-02\")<abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op< 6') :-
    parse("[a = abstime(\"2012-02\")<abstime(\"2012-03\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op< 7') :-
    parse("[a = reltime(\"1h\")<reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op< 8') :-
    parse("[a = reltime(\"1h\")<reltime(\"2h\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op> 1') :-
    parse("[a =  1 > 0]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op> 2') :-
    parse("[a =  1 > 1.0]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op> 3') :-
    parse("[a =  \"b\" > \"a\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op> 4') :-
    parse("[a =  \"a\" > \"a\"]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op> 5') :-
    parse("[a = abstime(\"2012-02\")>abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op> 6') :-
    parse("[a = abstime(\"2012-03\")>abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op> 7') :-
    parse("[a = reltime(\"1h\")>reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op> 8') :-
    parse("[a = reltime(\"2h\")>reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 1') :-
    parse("[a =  1 <= 2]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 2') :-
    parse("[a =  1 <= 1.0]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 3') :-
    parse("[a =  2 <= 1.0]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op<= 4') :-
    parse("[a =  \"a\" <= \"b\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 5') :-
    parse("[a = abstime(\"2012-02\")<=abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 6') :-
    parse("[a = abstime(\"2012-03\")<=abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op<= 7') :-
    parse("[a = reltime(\"1h\")<=reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 8') :-
    parse("[a = reltime(\"2h\")<=reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op<= 5') :-
    parse("[a =  \"a\" <= \"a\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op<= 6') :-
    parse("[a =  \"b\" <= \"a\"]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op>= 1') :-
    parse("[a =  1 >= 2]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op>= 2') :-
    parse("[a =  1 >= 1.0]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op>= 3') :-
    parse("[a =  2 >= 1.0]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op>= 4') :-
    parse("[a =  \"a\" >= \"b\"]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op>= 5') :-
    parse("[a =  \"a\" >= \"a\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op>= 6') :-
    parse("[a =  \"b\" >= \"a\"]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op>= 7') :-
    parse("[a = abstime(\"2012-02\")>=abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op>= 8') :-
    parse("[a = abstime(\"2012-02\")>=abstime(\"2012-03\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op>= 9') :-
    parse("[a = reltime(\"1h\")>=reltime(\"1h\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op>= 10') :-
    parse("[a = reltime(\"1h\")>=reltime(\"2h\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=?= 1') :-
    parse("[a = 1 =?= 1]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op=?= 2') :-
    parse("[a = 1 =?= b]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=?= 3') :-
    parse("[a = 1 =?= 2]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=?= 4') :-
    parse("[a = abstime(\"2012-02\") =?= abstime(\"2012-02\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op=?= 5') :-
    parse("[a = abstime(\"2012-02\") =?= abstime(b)]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=?= 6') :-
    parse("[a = reltime(\"1s\") =?= reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op=?= 7') :-
    parse("[a = reltime(b) =?= reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=!= 1') :-
    parse("[a = 1 =!= 1]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=!= 2') :-
    parse("[a = 1 =!= b]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=!= 3') :-
    parse("[a = 1 =!= 2]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op=!= 4') :-
    parse("[a = abstime(\"2012-02\") =!= abstime(\"2012-03\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op=!= 5') :-
    parse("[a = abstime(\"2012-02\") =!= abstime(b)]", C),
    classad_eval_native("a", C, R),
    R == false.

test('op=!= 6') :-
    parse("[a = reltime(\"2s\") =!= reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R == true.

test('op=!= 7') :-
    parse("[a = reltime(b) =!= reltime(\"1s\")]", C),
    classad_eval_native("a", C, R),
    R == false.

test('is isnt 1') :-
    classad_eval_native("error is error", [], true),
    classad_eval_native("error isnt error", [], false),
    classad_eval_native("undefined is undefined", [], true),
    classad_eval_native("undefined isnt undefined", [], false),
    classad_eval_native("true is true", [], true),
    classad_eval_native("true isnt true", [], false),
    classad_eval_native("false is false", [], true),
    classad_eval_native("false isnt false", [], false),
    classad_eval_native("error is undefined", [], false),
    classad_eval_native("error isnt undefined", [], true),
    classad_eval_native("error is undefined", [], false),
    classad_eval_native("error isnt undefined", [], true),
    classad_eval_native("true is undefined", [], false),
    classad_eval_native("false isnt undefined", [], true).

test('is isnt 2') :-
    classad_eval_native("\"z\" is \"z\"", [], true),
    classad_eval_native("\"z\" isnt \"z\"", [], false),
    classad_eval_native("\"Z\" is \"z\"", [], false),
    classad_eval_native("\"z\" isnt \"Z\"", [], true).

test('is isnt 3') :-
    classad_eval_native("abstime(\"2012-02\") is abstime(\"2012-02\")", [], true),
    classad_eval_native("abstime(\"2012-02\") isnt abstime(\"2012-02\")", [], false),
    classad_eval_native("abstime(\"2012-03\") is abstime(\"2012-02\")", [], false),
    classad_eval_native("abstime(\"2012-02\") isnt abstime(\"2012-03\")", [], true).

test('is isnt 4') :-
    classad_eval_native("reltime(\"1\") is abstime(\"1\")", [], true),
    classad_eval_native("reltime(\"1\") isnt abstime(\"1\")", [], false),
    classad_eval_native("reltime(\"1\") is abstime(\"2\")", [], false),
    classad_eval_native("reltime(\"2\") isnt abstime(\"1\")", [], true).

test('is isnt 5') :-
    classad_eval_native("42 is 42", [], true),
    classad_eval_native("42 isnt 42", [], false),
    classad_eval_native("69 is 42", [], false),
    classad_eval_native("42 isnt 69", [], true).

test('is isnt 6') :-
    classad_eval_native("42.0 is 42.0", [], true),
    classad_eval_native("42.0 isnt 42.0", [], false),
    classad_eval_native("69.0 is 42.0", [], false),
    classad_eval_native("42.0 isnt 69.0", [], true).

test('is isnt 7') :-
    classad_eval_native("42 is 42.0", [], false),
    classad_eval_native("42.0 isnt 42", [], true).

test('is isnt 8') :-
    classad_eval_native("{0} is {0}", [], true),
    classad_eval_native("{0} isnt {0}", [], false),
    classad_eval_native("{0} is {1}", [], false),
    classad_eval_native("{0} isnt {1}", [], true).

test('is isnt 9') :-
    classad_eval_native("[a=4] is [a=4]", [], true),
    classad_eval_native("[a=4] isnt [a=4]", [], false),
    classad_eval_native("[b=4] is [a=4]", [], false),
    classad_eval_native("[b=4] isnt [a=4]", [], true).

test('op[] 1') :-
    parse("[a = {1}[0]]", C),
    classad_eval_native("a", C, R),
    R == 1.

test('op[] 2') :-
    parse("[a = [z=42][\"z\"]; z=69]", C),
    classad_eval_native("a", C, R),
    R == 42.

test('op[] 3') :-
    parse("[a = {1}[3]]", C),
    classad_eval_native("a", C, R),
    R == error.

test('op[] 4') :-
    parse("[a = {[z=42]}[\"z\"]; z=69]", C),
    classad_eval_native("a", C, R),
    R == [42].

test('op[] 5') :-
    parse("[a = {}[\"z\"]; z=69]", C),
    classad_eval_native("a", C, R),
    R == [].

test('op[] 6') :-
    parse("[a = {1}[\"z\"]]", C),
    classad_eval_native("a", C, R),
    R == [error].

test('op[] 7') :-
    parse("[a = {[x=1]}[\"z\"]]", C),
    classad_eval_native("a", C, R),
    R == [undefined].

test('op[] 8') :-
    parse("[a = {}[0]]", C),
    classad_eval_native("a", C, R),
    R == error.

test('op[] 9') :-
    parse("[a = {0}[-1]]", C),
    classad_eval_native("a", C, R),
    R == error.

test('op[] 10') :-
    parse("[a = b[0]]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('op[] 11') :-
    parse("[a = {1}[b]]", C),
    classad_eval_native("a", C, R),
    R == undefined.

test('op[] 12') :-
    parse("[a = {{3},{4}}[1][0]]", C),
    classad_eval_native("a", C, R),
    R == 4.

test('op[] 13') :-
    parse("[a = [b=[c=55];c=44]; c=33; d=a[\"b\"][\"c\"]]", C),
    classad_eval_native("d", C, R),
    R == 55.

test('op[] 14') :-
    parse("[a = [b=[c=parent.c];c=44]; c=33; d=a[\"b\"][\"c\"]]", C),
    classad_eval_native("d", C, R),
    R == 44.

test('op[] 15') :-
    parse("[a = [b=[c=parent.c];c=44]; c=33; d=a[\"parent\"][\"c\"]]", C),
    classad_eval_native("d", C, R),
    R == 33.

test('op[] 16') :-
    parse("[a = {0,[],[z=77],[z=\"zz\"]}[\"z\"]]", C),
    classad_eval_native("a", C, R),
    R == [error, undefined, 77, '[str]'(zz)].

test('op[] 17') :-
    parse("[a = {0,[],[z=77],[z=\"zz\"]}[\"z\"][2]]", C),
    classad_eval_native("a", C, R),
    R == 77.

test('op[] 18') :-
    parse("[a = {66, 88, 77}; b = [z=1]; c=a[b[\"z\"]]]", C),
    classad_eval_native("c", C, R),
    R == 88.

test('op| 1') :-
    parse("[a = 1 | 4]", C),
    classad_eval('a', C, R),
    R == 5.

test('op| 2') :-
    parse("[a = false | true]", C),
    classad_eval('a', C, R),
    R == true.

test('op| 3') :-
    parse("[a = b | true]", C),
    classad_eval('a', C, R),
    R == undefined.

test('op& 1') :-
    parse("[a = 3 & 6]", C),
    classad_eval('a', C, R),
    R == 2.

test('op& 2') :-
    parse("[a = false & true]", C),
    classad_eval('a', C, R),
    R == false.

test('op& 3') :-
    parse("[a = false & b]", C),
    classad_eval('a', C, R),
    R == undefined.

test('op^ 1') :-
    parse("[a = 3 ^ 6]", C),
    classad_eval('a', C, R),
    R == 5.

test('op^ 2') :-
    parse("[a = false ^ true]", C),
    classad_eval('a', C, R),
    R == true.

test('op^ 3') :-
    parse("[a = b ^ true]", C),
    classad_eval('a', C, R),
    R == undefined.

test('op~ 1') :-
    parse("[a = ~0]", C),
    classad_eval('a', C, R),
    R == -1.

test('op~ 2') :-
    parse("[a = ~b]", C),
    classad_eval('a', C, R),
    R == undefined.

test('op>> 1') :-
    parse("[a = 8 >> 1]", C),
    classad_eval('a', C, R),
    R == 4.

test('op>> 2') :-
    parse("[a = -8 >> 1]", C),
    classad_eval('a', C, R),
    R == -4.

test('op>> 3') :-
    parse("[a = -8 >> b]", C),
    classad_eval('a', C, R),
    R == undefined.

test('op<< 1') :-
    parse("[a = 8 << 1]", C),
    classad_eval('a', C, R),
    R == 16.

test('op<< 2') :-
    parse("[a = -8 << 1]", C),
    classad_eval('a', C, R),
    R == -16.

test('op<< 3') :-
    parse("[a = b << 1]", C),
    classad_eval('a', C, R),
    R == undefined.

test('op>>> 1') :-
    parse("[a = 8 >>> 1]", C),
    classad_eval('a', C, R),
    R == 4.

test('op>>> 2') :-
    parse("[a = -1 >>> 1]", C),
    classad_eval('a', C, R),
    classad_eval:max_int(Z),
    R == Z.

test('op>>> 3') :-
    parse("[a = -1 >>> 2]", C),
    classad_eval('a', C, R),
    classad_eval:max_int(Z),
    R =:= Z/2.

test('func time 1') :-
    get_time(T0),
    parse("[a = time()]", C),
    classad_eval_native("a", C, R),
    get_time(T1),
    integer(R),
    R >= T0,
    T1 >= R.

test('func time 2') :-
    parse("[a = time(0)]", C),
    classad_eval_native("a", C, R),
    R == error.

test('func time 3') :-
    parse("[a = time(b)]", C),
    classad_eval_native("a", C, R),
    R == error.

test('func abstime 1') :-
    get_time(T0),
    local_tzo(Z0),
    parse("[a = abstime()]", C),
    classad_eval_native("a", C, R),
    get_time(T1),
    R = '[abstime]'(T,Z),
    number(T),
    number(Z),
    T >= T0,
    T1 >= T,
    Z =:= Z0.

test('func abstime 2') :-
    local_tzo(Z0),
    parse("[a = abstime(1329798052)]", C),
    classad_eval_native("a", C, R),
    R = '[abstime]'(T,Z),
    number(T),
    number(Z),
    T =:= 1329798052,
    Z =:= Z0.

test('func abstime 2a') :-
    parse("[a = abstime(1329798052,3600)]", C),
    classad_eval_native("a", C, R),
    R = '[abstime]'(T,Z),
    number(T),
    number(Z),
    T =:= 1329798052,
    Z =:= -3600.

test('func abstime 3') :-
    local_tzo(Z0),
    parse("[a = abstime(\"2012-02-20\")]", C),
    classad_eval_native("a", C, R),
    R = '[abstime]'(T,Z),
    number(T),
    number(Z),
    T =:= 1329696000,
    Z =:= Z0.

test('func abstime 4') :-
    parse("[a = abstime(b)]", C),
    classad_eval_native("a", C, R),
    R = undefined.

test('func abstime 5') :-
    parse("[a = abstime([])]", C),
    classad_eval_native("a", C, R),
    R = error.

test('func abstime 6') :-
    parse("[a = abstime({}[1])]", C),
    classad_eval_native("a", C, R),
    R = error.

test('func reltime 1') :-
    parse("[a = reltime(1000)]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(S),
    number(S),
    S == 1000.

test('func reltime 2') :-
    parse("[a = reltime(\"1+01:01:01\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(S),
    number(S),
    S == 90061.

test('func reltime 3') :-
    parse("[a = reltime(\"-1+01:01:01\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(S),
    number(S),
    S == -90061.

test('func reltime 4') :-
    parse("[a = reltime(\"1h 1s\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(S),
    number(S),
    S == 3601.

test('func reltime 5') :-
    parse("[a = reltime(\"1+ 1m\")]", C),
    classad_eval_native("a", C, R),
    R = '[reltime]'(S),
    number(S),
    S == 86460.

test('func reltime 6') :-
    parse("[a = reltime(\"\")]", C),
    classad_eval_native("a", C, R),
    R = error.

test('func reltime 7') :-
    parse("[a = reltime(b)]", C),
    classad_eval_native("a", C, R),
    R = undefined.

test('func interval 1') :-
    parse("[a = interval(1.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('1.100').

test('func interval 2') :-
    parse("[a = interval(-1.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('-1.100').

test('func interval 3') :-
    parse("[a = interval(61.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('1:01.100').

test('func interval 4') :-
    parse("[a = interval(-61.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('-1:01.100').

test('func interval 5') :-
    parse("[a = interval(3661.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('1:01:01.100').

test('func interval 6') :-
    parse("[a = interval(-3661.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('-1:01:01.100').

test('func interval 7') :-
    parse("[a = interval(90061.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('1+01:01:01.100').

test('func interval 8') :-
    parse("[a = interval(-90061.1)]", C),
    classad_eval_native("a", C, R),
    R == '[str]'('-1+01:01:01.100').

:- end_tests(eval).
