% use the classad module for this example:
:- add_to_path('../lib').
:- use_module(classad).

% a utility for expediting output reporting
report(V, Val, Type) :- format("~a= ~q   type= ~q\n", [V, Val, Type]).


:-
% these are used for rescoping
classad_eval_native("[x = 1;  y = 2]", [], ClassadRS1),
classad_eval_native("[y = 3;  z = 4]", [], ClassadRS2),
classad_eval_native("[x = 5.0;  z = 6.0]", [], ClassadRS3),

classad_eval_native("[a = 10 * other.x]", [], CA1),

% 'a' evaluates to undefined, since other is undefined
classad_lookup(a, CA1, R1, T1), report(a, R1, T1),

% but if we rescope 'other' to ClassadRS1, we get a value for x:
classad_lookup(a, CA1, [other=ClassadRS1], R2, T2), report(a, R2, T2),

% using ClassadRS3 for rescoping 'other' gives a different value:
classad_lookup(a, CA1, [other=ClassadRS3], R3, T3), report(a, R3, T3),

% multiple rescopings can be provided, and contexts can be either a single
% classad or a 'stack' of classads:
classad_eval_native("[a = other1.x + other2.x;  b = other3.z + other3.y + other3.x]", [], CA2),
classad_lookup(a, CA2, [other1 = ClassadRS1, other2 = ClassadRS3, other3=[ClassadRS2, ClassadRS1]], R4, T4), report(a, R4, T4),
classad_lookup(b, CA2, [other1 = ClassadRS1, other2 = ClassadRS3, other3=[ClassadRS2, ClassadRS1]], R5, T5), report(b, R5, T5),

% This demonstrates how rescoping is accomplished by construction of rescoping classad:
rescope_classad([other1 = ClassadRS1, other2 = ClassadRS3, other3=[ClassadRS2, ClassadRS1]], RSA),
classad_serialize(RSA),

% end example
true.
