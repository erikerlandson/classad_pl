:- module(classad_lexer,
          [lex/2          % lex(+String, -TokenList)
          ]).

:- use_module(library(lists)).

% invoke the grammar rule predicates on a string to get a token list
lex(S, TL) :- tokseq(TL, S, []), !.

wstype(white).
wstype(end_of_line).

% The top level of the lexing grammar: parse a
% sequence of tokens out of a prolog string.
% a whitespace char is just consumed, and adds nothing
% to the token list: 
tokseq(L) --> wschar, tokseq(L).
% if you consume a token add it to the list:
tokseq([T|R]) --> tok(T), tokseq(R).
% basis case: nothing but empty string is left:
tokseq([]) --> "".

% consume a whitespace character:
wschar --> [C], {char_type(C,T), wstype(T)}.

% strings are tokens
% put this expansion rule first, since we want anything starting with
% quotes to tokenize as a string until next appearance of quotes
tok(T) --> str(T).

% numbers are tokens
% put this token expansion rule second because some components of numbers
% can be otherwise interpreted as other kinds of tokens, and we want to avoid
% that.
tok(T) --> num(T).

% variable names, or identifiers, are tokens
tok(T) --> ident(T).

% various approved symbols/operators are tokens
% put this expansion last, to allow longer kinds of token, such as numbers,
% to take longest-token precedence as first priority.
tok(T) --> sym(T).


% expansion of string tokens
str('[str]'(A)) --> "\"", strseq(SS), "\"", {atom_codes(A, SS)}.

strseq([C|R]) --> regchar(C), strseq(R).
strseq([]) --> "".

regchar(C) --> [C], { [C]\="\"", char_type(C, ascii) }.

% expansion of identifier tokens
% identifiers are case insensitive in classad spec, so I just casefold them here.
ident(I) --> ihead(C), irest(R), { atom_codes(A, [C|R]), downcase_atom(A, I) }.

ihead(C) --> [C], { char_type(C, alpha) ; [C]="_" }.
irest([C|R]) --> [C], { char_type(C, alnum) ; [C]="_" }, irest(R).
irest([]) --> "".


% expansion of number tokens
% I am just tokenizing anything that adheres to syntax of a standard floating point
% number, so integers are included.  Only decimal radix.
num(N) --> dhead(D), drest(R), dpseq(DS), expseq(ES), { flatten([D,R,DS,ES],L), number_codes(N, L) }.

% stick an extra zero at the end because "1." causes yap conversion to blow up
dpseq(S) --> ".", drest(R), {flatten([".",R,"0"],S)}.
dpseq([]) --> "".

expseq(S) --> expchar(E), expsign(ES), dhead(D), drest(R), {flatten([E,ES,D,R], S)}.
expseq([]) --> "".

expchar("e") --> "e".
expchar("e") --> "E".

expsign("+") --> "+".
expsign("-") --> "-".
expsign([]) --> "".

dhead(D) --> [D], {char_type(D, digit)}.
drest([D|R]) --> dhead(D), drest(R).
drest([]) --> "".


% symbol/operator tokens
% to get longest-lex behavior as the first choice from
% prolog proof, define these starting with longest tokens first.
sym('=?=') --> "=?=".
sym('=!=') --> "=!=".
sym('||') --> "||".
sym('&&') --> "&&".
sym('<=') --> "<=".
sym('>=') --> ">=".
sym('==') --> "==".
sym('!=') --> "!=".
sym('(') --> "(".
sym(')') --> ")".
sym('[') --> "[".
sym(']') --> "]".
sym('{') --> "{".
sym('}') --> "}".
sym(',') --> ",".
sym('<') --> "<".
sym('>') --> ">".
sym('=') --> "=".
sym('+') --> "+".
sym('-') --> "-".
sym('*') --> "*".
sym('/') --> "/".
sym('%') --> "%".
sym('!') --> "!".
sym('?') --> "?".
sym(':') --> ":".
sym(';') --> ";".
sym('.') --> ".".
sym('|') --> "|".
sym('&') --> "&".
sym('^') --> "^".
sym('~') --> "~".
