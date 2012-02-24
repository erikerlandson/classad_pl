:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module('../lib/classad_reltime_parser.pl').

:- begin_tests(reltime_parser).

test('standard 1') :-
    parse_reltime("1+01:01:01", S),
    S == 90061.

test('standard 2') :-
    parse_reltime("-1+01:01:01", S),
    S == -90061.

test('standard 3') :-
    parse_reltime("1+01:01:01.01", S),
    S == 90061.01.

test('relaxed 1') :-
    parse_reltime("1d", S),
    S == 86400.

test('relaxed 2') :-
    parse_reltime("1h", S),
    S == 3600.

test('relaxed 3') :-
    parse_reltime("1m", S),
    S == 60.

test('relaxed 4') :-
    parse_reltime("1s", S),
    S == 1.

test('relaxed 5') :-
    parse_reltime("1h 1s", S),
    S == 3601.

test('relaxed 6') :-
    parse_reltime("1+1m", S),
    S == 86460.

test('fail 1', [fail]) :-
    parse_reltime("", _).


:- end_tests(reltime_parser).
