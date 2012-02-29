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
classad_assign(int1, 42, CA0, CA1),

% insert a real/float:
classad_assign(real1, 3.14, CA1, CA2),

% insert a string:
classad_assign(str1, 'Hello world!', CA2, CA3),

% insert a boolean:
classad_assign(bool1, true, CA3, CA4),

% insert an abstime from a prolog date term:
classad_assign(at1, date(2012,2,24,6,12,31.0,25200,'MST',false), CA4, CA5),

% an abstime from epoch and tz offset:
classad_assign(at2, abstime(1330089151.0, 25200), CA5, CA6),

% insert a reltime using reltime(Seconds) term:
classad_assign(rt1, reltime(60), CA6, CA7),

% insert a list:
classad_assign(list1, [77, 2.718, 'Pebbles', ['Bam Bam']], CA7, CA8),

% create a classad by parsing a native classad expr:
eval_native('[x = 555 + 111]', [], TCA),
% assign the classad expr:
classad_assign(ca1, TCA, CA8, CA9),

% assign another classad from a native-syntax string:
classad_assign_native(ca2, "[z = 777 - 111]", CA9, CA10),

% note that the value of u1 will be undefined because zz has no definition:
classad_assign_native(u1, "2 * zz", CA10, CA11),

% classad_assign/4 fails on unrecognized data it cannot identify a type for:
(classad_assign(oh_no, bogus(term), CA11, CA12) ; (print('Your shipment of FAIL has arrived'), nl)),

% this will be our classad for lookup:
CA = CA11,

% extract our data using classad_lookup/4 to get the value types:
classad_lookup(int1, CA, R1, T1), report(int1, R1, T1),
classad_lookup(real1, CA, R2, T2), report(real1, R2, T2),
classad_lookup(str1, CA, R3, T3), report(str1, R3, T3),
classad_lookup(bool1, CA, R4, T4), report(str1, R4, T4),
classad_lookup(at1, CA, R5, T5), report(at1, R5, T5),
classad_lookup(at2, CA, R6, T6), report(at2, R6, T6),
classad_lookup(rt1, CA, R7, T7), report(rt1, R7, T7),
classad_lookup(list1, CA, R8, T8), report(list1, R8, T8),

% lookup vars with classad values, and then lookup a variable in the result:
classad_lookup(ca1, CA, TCA9, _T9), classad_lookup(x, TCA9, XV, XT), report(x, XV, XT),
classad_lookup(ca2, CA, TCA10, _T10), classad_lookup(z, TCA10, ZV, ZT), report(z, ZV, ZT),

% the variable u1 is present, but evaluates to undefined:
classad_lookup(u1, CA, R11, T11), report(u1, R11, T11),

% variable u2 is not present.  classad_lookup returns undefined for value, but type = novar:
classad_lookup(u2, CA, R12, T12), report(u2, R12, T12),

% classad_lookup/3 does not distinguish between 'not present' and 'evaluates to undefined'
classad_lookup(u2, CA, R13), report(u2, R13, 'no type from classad_lookup/3'),

% end example
true.
