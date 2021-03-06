% This example demonstrates the evaluation semantic for classad
% expressions, using eval_native predicate:

:- add_to_path('../lib').
:- use_module(classad).

:- % create a simple classad C1 defining variables x and a:
   classad_eval_native("[x = other.a < a;  a = 69]", [], C1),

   % evaluate expression "x" using C1 for the Context:
   classad_eval_native("x", C1, X1),

   % 'other' is not defined, and so x evaluates to undefined:
   print('X1= '), print(X1), nl,

   % now create another classad C2, also defining a variable "a":
   classad_eval_native("[a = 42]", [], C2),

   % evaluate "x" again, this time specifying that 'other' refers to C2,
   % by using [C1, [other = C2]] as the Context stack.
   classad_eval_native("x", C1, [other = C2], X2),

   % now "x" evaluates to 'true':
   print('X2= '), print(X2), nl.
