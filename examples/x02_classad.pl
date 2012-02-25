% This example demonstrates creating a classad, assigning some data to it,
% and getting data back out using lookup, using some basic assign and lookup
% invocations.

% use the classad and classad_eval modules for this example:
:- use_module('../lib/classad.pl').
:- use_module('../lib/classad_eval.pl').

% a utility for expediting output reporting
report(V, Val, Type) :- print(V), print('= '), print(Val), print('  type= '), print(Type), nl.

:-
% create a new classad
new_classad(CA0),

% insert an integer
assign(int1, 42, CA0, CA1),

% insert a real/float:
assign(real1, 3.14, CA1, CA2),

% insert a string:
assign(str1, 'Hello world!', CA2, CA3),

% insert a boolean:
assign(bool1, true, CA3, CA4),

% insert an abstime from a prolog date term:
assign(at1, date(2012,2,24,6,12,31.0,25200,'MST',false), CA4, CA5),

% an abstime from epoch and tz offset:
assign(at2, abstime(1330089151.0, 25200), CA5, CA6),

% insert a reltime using reltime(Seconds) term:
assign(rt1, reltime(60), CA6, CA7),

% insert a list:
assign(list1, [77, 2.718, 'Pebbles', ['Bam Bam']], CA7, CA8),

% create a classad by parsing a native classad expr:
eval_native('[x = 555 + 111]', [], TCA),
% assign the classad expr:
assign(ca1, TCA, CA8, CA9),

% assign another classad from a native-syntax string:
assign_native(ca2, "[z = 777 - 111]", CA9, CA10),

% note that the value of u1 will be undefined because zz has no definition:
assign_native(u1, "2 * zz", CA10, CA11),

% assign/4 fails on unrecognized data it cannot identify a type for:
(assign(oh_no, bogus(term), CA11, CA12) ; (print('Your shipment of FAIL has arrived'), nl)),

% this will be our classad for lookup:
CA = CA11,

% extract our data using lookup/4 to get the value types:
lookup(int1, CA, R1, T1), report(int1, R1, T1),
lookup(real1, CA, R2, T2), report(real1, R2, T2),
lookup(str1, CA, R3, T3), report(str1, R3, T3),
lookup(bool1, CA, R4, T4), report(str1, R4, T4),
lookup(at1, CA, R5, T5), report(at1, R5, T5),
lookup(at2, CA, R6, T6), report(at2, R6, T6),
lookup(rt1, CA, R7, T7), report(rt1, R7, T7),
lookup(list1, CA, R8, T8), report(list1, R8, T8),

% lookup vars with classad values, and then lookup a variable in the result:
lookup(ca1, CA, TCA9, _T9), lookup(x, TCA9, XV, XT), report(x, XV, XT),
lookup(ca2, CA, TCA10, _T10), lookup(z, TCA10, ZV, ZT), report(z, ZV, ZT),

% the variable u1 is present, but evaluates to undefined:
lookup(u1, CA, R11, T11), report(u1, R11, T11),

% variable u2 is not present.  lookup returns undefined for value, but type = novar:
lookup(u2, CA, R12, T12), report(u2, R12, T12),

% lookup/3 does not distinguish between 'not present' and 'evaluates to undefined'
lookup(u2, CA, R13), report(u2, R13, 'no type from lookup/3'),

% end example
true.
