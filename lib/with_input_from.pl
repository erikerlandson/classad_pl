:- module(with_input_from, [
         with_input_from/2
         ]).

:- use_module(library(memfile)).

:- meta_predicate with_input_from(+,:).

% The following is from Samer Abdallah:
% samer.abdallah@elec.qmul.ac.uk 
% https://lists.iai.uni-bonn.de/pipermail/swi-prolog/2010/004384.html

%% with_input_from( +Source, :Goal) is semidet.
%
%  Temporarily switch current input to object specified by Source while calling Goal as in once/1.
%  Source is a term like that supplied to with_output_to/2 and can be any of:
%     * A stream handle or alias.
%     * atom(+Atom)
%     * codes(+Codes)
%     * chars(+Chars)
%     * string(+String)

with_input_from(atom(A), Goal) :- !,
   setup_call_cleanup(
      atom_to_memory_file(A,MF),
      setup_call_cleanup(
         open_memory_file(MF,read,S),
         with_input_from(S,Goal),
         close(S)),
      free_memory_file(MF)).

with_input_from(codes(Codes), G) :- !, atom_codes(A, Codes), with_input_from(atom(A), G).
with_input_from(chars(Chars), G) :- !, atom_chars(A, Chars), with_input_from(atom(A), G).
with_input_from(string(Str), G)  :- !, string_to_atom(Str, A), with_input_from(atom(A), G).

with_input_from(S,G) :- is_stream(S), !,
   current_input(S0),
   setup_call_cleanup( set_input(S), once(G), set_input(S0)).
