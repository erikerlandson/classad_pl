% This example demonstrates some additional features of assign and lookup
% that give you more control of classad input and output.

% use the classad and classad_eval modules for this example:
:- use_module('../lib/classad.pl').
:- use_module('../lib/classad_eval.pl').

% a utility for expediting output reporting
report(V, Val, Type) :- print(V), print('= '), print(Val), print('  type= '), print(Type), nl.

:-
new_classad(CA0),

% assign a char-code list as a string:
assign(str1, as(string, "string!"), CA0, CA1),

% assign a number as a reltime value, interpreting as seconds:
assign(rt1, as(reltime, 3600), CA1, CA2),

% assign a number as an abstime val, interpreting as seconds from epoch:
assign(at1, as(abstime, 1330097876.0), CA2, CA3),

% assign a boolean
assign(b1, true, CA3, CA4),

% assign an integer
assign(i1, 99, CA4, CA5),

% use CA for lookup
CA = CA5,

% By default, strings are returned as atoms.
% Pass in as(codelist) for Type, to lookup a string as a char-code list:
lookup(str1, CA, R1, as(codelist)), report(str1, R1, 'as a code list'),

% lookup an abstime value as a date/9 tuple:
lookup(at1, CA, R2, as(date)), report(at1, R2, 'as a date/9 term'),

% lookup a reltime value as a real value:
lookup(rt1, CA, R3, as(real)), report(rt1, R3, 'as a real'),

% lookup a boolean value as an integer:
lookup(b1, CA, R4, as(integer)), report(b1, R4, 'as an integer'),

% specify that the value must be of type integer:
lookup(i1, CA, R5, integer), report(i1, R5, 'required integer'),

% attempting to specify wrong type will cause failure
(lookup(i1, CA, _R6, string) ; (print('epic fail requiring string'), nl)),

% requiring type 'number' will accept either integer or real:
lookup(i1, CA, R5, number), report(i1, R5, 'required number'),

% end example
true.
