% For now, just execute all tests
main([]) :- run_tests, nl.
main(ArgV) :- run_tests(ArgV), nl.

% load unit test files and execute 'main' with command line arguments
:- ['classad_lexer.plt', 'classad_parser.plt', 'classad_reltime_parser.plt', 'classad_eval.plt', 'classad.plt'].
:- unix(argv(ArgV)), time(main(ArgV)).
