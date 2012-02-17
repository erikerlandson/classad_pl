% For now, just execute all tests
main(ArgV) :- run_tests, nl.

% load unit test files and execute 'main' with command line arguments
:- ['classad_lexer.plt', 'classad_parser.plt', 'classad_eval.plt'].
:- unix(argv(ArgV)), main(ArgV).
