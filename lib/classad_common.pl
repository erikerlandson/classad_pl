:- module(classad_common, []).

% reserved words in the classad spec
reserved_word(W) :- reserved_expr(W).
reserved_word(W) :- reserved_op(W).

reserved_expr(true).
reserved_expr(false).
reserved_expr(parent).
reserved_expr(undefined).
reserved_expr(error).

reserved_op(is).
reserved_op(isnt).
