% This example demonstrates some additional features of assign and lookup
% that give you more control of classad input and output.

% use the classad and classad_eval modules for this example:
:- use_module('../lib/classad.pl').
:- use_module('../lib/classad_eval.pl').

% a utility for expediting output reporting
report(V, Val, Type) :- print(V), print('= '), print(Val), print('  type= '), print(Type), nl.

:-
new_classad(CA0),

%%%%%%%%%
% specifying Type argument to require an input type:

% assign a boolean, requiring it to be boolean:
classad_assign(b1, true, CA0, CA1, boolean),

% assign an integer, and require it to be an number (integer or float):
classad_assign(i1, 99, CA1, CA2, number),

% requiring a string here will fail:
(classad_assign(i2, 999, CA2, _, string); (print('requiring a string is terrible fail'), nl)),


%%%%%%%%
% using as(Type) to manipulate input:

% assign a char-code list as a string:
classad_assign(str1, "string!", CA2, CA3, as(string)),

% assign a number as a reltime value, interpreting as seconds:
classad_assign(rt1, 3600, CA3, CA4, as(reltime)),

% assign a number as an abstime val, interpreting as seconds from epoch:
classad_assign(at1, 1330097876.0, CA4, CA5, as(abstime)),

% use CA for lookup
CA = CA5,


%%%%%%%
% specifying that a lookup value must be a particular type:

% specify that the value must be of type integer:
classad_lookup(i1, CA, R1, integer), report(i1, R1, 'required integer'),

% attempting to specify wrong type will cause failure
(classad_lookup(i1, CA, _R2, string) ; (print('epic fail requiring string'), nl)),

% requiring type 'number' will accept either integer or real:
classad_lookup(i1, CA, R3, number), report(i1, R3, 'required number'),


%%%%%%%
% using as(Type) to manipulate output:

% By default, strings are returned as atoms.
% Pass in as(codelist) for Type, to lookup a string as a char-code list:
classad_lookup(str1, CA, R4, as(codelist)), report(str1, R4, 'as a code list'),

% lookup an abstime value as a date/9 tuple:
classad_lookup(at1, CA, R5, as(date)), report(at1, R5, 'as a date/9 term'),

% lookup a reltime value as a real value:
classad_lookup(rt1, CA, R6, as(real)), report(rt1, R6, 'as a real'),

% lookup a boolean value as an integer:
classad_lookup(b1, CA, R7, as(integer)), report(b1, R7, 'as an integer'),

% end example
true.
