:- module(classad_parser,
          [parse/2,            % parse(+String, -ExprTree)
           classad_eval_native/3,     % classad_eval_native(+String, +Context, -Result)
           classad_eval_native/4      % classad_eval_native(+String, +Context, +RescopeList, -Result)
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(classad_common).
:- use_module(classad_lexer).
:- use_module(classad_eval, [classad_eval/3, classad_eval/4]).


% Parse String as a native syntax classad expression, and evaluate it in Context
classad_eval_native(String, Context, Result) :-
    parse(String, Expr), classad_eval(Expr, Context, Result), !.

classad_eval_native(String, Context, RescopeList, Result) :- !,
    parse(String, Expr), classad_eval(Expr, Context, RescopeList, Result).


:- dynamic macro_context/1.

% lex and parse a string to get an expression list
parse(S, E) :- 
    is_list(S), !, 
    classad_lexer:lex(S, TL),
    parse_tl(TL, E).

% parse an atom as a char-code string:
parse(A, E) :- atom(A), !, atom_codes(A, S), parse(S, E).

% invoke the grammar rule predicates on a token list to get an expr-tree
parse_tl(TL, E) :- call_cleanup(expr(E, TL, []), retractall(macro_context(_))), !.

eq_op('==').
eq_op('!=').
eq_op('=?=').
eq_op('=!=').
eq_op('is').
eq_op('isnt').

comp_op('<=').
comp_op('>=').
comp_op('<').
comp_op('>').

shift_op('>>>').
shift_op('>>').
shift_op('<<').

add_op('+').
add_op('-').

mul_op('*').
mul_op('/').
mul_op('%').

unary_op('!').
unary_op('-').
unary_op('+').
unary_op('~').

expr(E) --> cond(E).

cond(E) --> orseq(C), condrest(C, E).
condrest(C, E) --> ['?'], expr(RT), [':'], expr(RF), { E = '?:'(C, RT, RF) }.
condrest(C, C) --> [].

orseq(E) --> andseq(SE), orrest(SE, E).
orrest(SE, E) --> ['||'], andseq(SE2), { TE = '||'(SE,SE2) }, orrest(TE, E).
orrest(E, E) --> [].

andseq(E) --> bworseq(SE), andrest(SE, E).
andrest(SE, E) --> ['&&'], bworseq(SE2), { TE = '&&'(SE,SE2) }, andrest(TE, E).
andrest(E, E) --> [].

bworseq(E) --> bwxorseq(SE), bworrest(SE, E).
bworrest(SE, E) --> ['|'], bwxorseq(SE2), { TE = '|'(SE,SE2) }, bworrest(TE, E).
bworrest(E, E) --> [].

bwxorseq(E) --> bwandseq(SE), bwxorrest(SE, E).
bwxorrest(SE, E) --> ['^'], bwandseq(SE2), { TE = '^'(SE,SE2) }, bwxorrest(TE, E).
bwxorrest(E, E) --> [].

bwandseq(E) --> equalseq(SE), bwandrest(SE, E).
bwandrest(SE, E) --> ['&'], equalseq(SE2), { TE = '&'(SE,SE2) }, bwandrest(TE, E).
bwandrest(E, E) --> [].

equalseq(E) --> compseq(SE), equalrest(SE, E).
equalrest(SE, E) --> [OP], { eq_op(OP) }, compseq(SE2), { TE =.. [OP,SE,SE2] }, equalrest(TE, E).
equalrest(E, E) --> [].

compseq(E) --> shiftseq(SE), comprest(SE, E).
comprest(SE, E) --> [OP], { comp_op(OP) }, shiftseq(SE2), { TE =.. [OP,SE,SE2] }, comprest(TE, E).
comprest(E, E) --> [].

shiftseq(E) --> addsubseq(SE), shiftrest(SE, E).
shiftrest(SE, E) --> [OP], { shift_op(OP) }, addsubseq(SE2), { TE =.. [OP,SE,SE2] }, shiftrest(TE, E).
shiftrest(E, E) --> [].

addsubseq(E) --> muldivseq(SE), addsubrest(SE, E).
addsubrest(SE, E) --> [OP], { add_op(OP) }, muldivseq(SE2), { TE =.. [OP,SE,SE2] }, addsubrest(TE, E).
addsubrest(E, E) --> [].

muldivseq(E) --> unary(SE), muldivrest(SE, E).
muldivrest(SE, E) --> [OP], { mul_op(OP) }, unary(SE2), { TE =.. [OP,SE,SE2] }, muldivrest(TE, E).
muldivrest(E, E) --> [].

unary(E) --> [OP], { unary_op(OP) }, unary(SE), { E =.. [OP,SE] }.
unary(E) --> idxseq(E).

idxseq(E) --> selseq(SE), idxrest(SE, E).
idxrest(SE, E) --> ['['], expr(SE2), [']'], { TE = '[]'(SE,SE2) }, idxrest(TE, E).
idxrest(E, E) --> [].

selseq(E) --> atomic(SE), selrest(SE, E).
selrest(SE, E) --> ['.'], selref(SE2), { TE = '[sel]'(SE,SE2) }, selrest(TE, E).
selrest(E, E) --> [].
selref(SR) --> ident(SR).
selref('parent') --> ['parent'].

% order matters here:  
% e.g. we definitely want func(E) before paren(E) and ident(E).
% fun fact: abstime and reltime literals are not part of the grammar.
atomic(E) --> classad(E).
atomic(E) --> list(E).
atomic(E) --> macro(E).
atomic(E) --> func(E).
atomic(E) --> paren(E).
atomic(E) --> reserved(E).
atomic(E) --> num(E).
atomic(E) --> str(E).
atomic(E) --> ident(E).

% a classad is a sequence of assignments: var = expr; (last ';' optional)
% I load these into an association list from the standard assoc library
classad('[classad]'(Mo)) --> ['['], 
    { list_to_assoc([], Mi), push_macro_context }, 
    assignseq(Mi, Mo), 
    [']'],
    { pop_macro_context }.


assignseq(Mi, Mo) --> assign(Mi, Mt), assignrest(Mt, Mo).
assignseq(M, M) --> [].
assignrest(Mi, Mo) --> [';'], assignseq(Mi, Mo).
assignrest(M, M) --> [].
assign(Mi, Mo) --> ident(V), ['='], expr(E), { put_assoc(V, Mi, E, Mo), update_macro_context(Mo) }.

% a list is a comma-separated sequence of expressions between {}.
list(E) --> ['{'], exprseq(E), ['}'].

% function calls are of the typical form: f(a1, a2, ...)
func('[func]'(F,A)) --> ident(F), ['('], exprseq(A), [')'].

% a comma-separated sequence of expressions, possibly empty
exprseq([E|R]) --> expr(E), exprrest(R).
exprseq([]) --> [].
exprrest([E|R]) --> [','], expr(E), exprrest(R).
exprrest([]) --> [].

% parenthesized sub-expressions:
paren(E) --> ['('], expr(E), [')'].

% reserved identifiers that are valid atomic expression values
% does not include reserved words that are operators
reserved(R) --> [R], { classad_common:reserved_expr(R) }.

% numbers, strings, identifiers:
num(N) --> [N], { number(N) }.
str(S) --> [S], { S='[str]'(_) }.
ident(I) --> ['[id]'(I)].

% macro eval:
macro(E) --> ['$','('], expr(SE), [')'], {
    (macro_context(C) ; C = []),
    classad_eval(SE, C, E)
    }.
macro(E) --> ['$'], ident(V), {
    (macro_context(C) ; C = []),
    classad_eval(V, C, E)
    }.

ensure_macro_context :- macro_context(_) ; assert(macro_context([])).

push_macro_context :-
    ensure_macro_context,
    macro_context(C),
    retract(macro_context(C)),
    list_to_assoc([], M),
    assert(macro_context(['[classad]'(M)|C])).

pop_macro_context :-
    macro_context([H|R]),
    retract(macro_context([H|R])),
    assert(macro_context(R)).

update_macro_context(M) :-
    macro_context([H|R]),
    retract(macro_context([H|R])),
    assert(macro_context(['[classad]'(M)|R])).
