:- module(classad_reltime_parser,
          [parse_reltime/2,  % parse_reltime(ReltimeExpression, Seconds)
           unparse_reltime/2
          ]).

:- expects_dialect(swi).


unparse_reltime(RTS, RTE) :- 
    number(RTS),
    ((RTS < 0) -> (SG = "-", SS is -RTS) ; (SG = "", SS is RTS)),
    WS is floor(SS), FS is SS-WS,
    D is WS // 86400, DR is WS mod 86400,
    H is DR // 3600, HR is DR mod 3600,
    M is HR // 60, MR is HR mod 60,
    S is MR + FS,
    ((D > 0) -> (
        with_output_to(atom(A1), format("~s~d+", [SG, D])),
        with_output_to(atom(A2), format("~0t~d~2+:", [H])),
        with_output_to(atom(A3), format("~0t~d~2+:", [M])),
        with_output_to(atom(A4), format("~0t~3f~6+", [S]))
    ) ; ( (H > 0) -> (
        A1 = '',
        with_output_to(atom(A2), format("~s~d:", [SG, H])),
        with_output_to(atom(A3), format("~0t~d~2+:", [M])),
        with_output_to(atom(A4), format("~0t~3f~6+", [S]))
    ) ; ( (M > 0) -> (
        A1 = '', A2 = '',
        with_output_to(atom(A3), format("~s~d:", [SG,M])),
        with_output_to(atom(A4), format("~0t~3f~6+", [S]))
    ) ; (
        A1 = '', A2 = '', A3 = '', 
        with_output_to(atom(A4), format("~s~3f", [SG,S]))
    )))),
    concat_atom([A1,A2,A3,A4], RTE).


% parse a string containing a reltime expression, and return the corresponding number of seconds.
parse_reltime(RTE, S) :- reltime(S, RTE, []), !.

% consume a reltime expression, and return the corresponding number of seconds
% a reltime expression consists of a leading sign, day, hour, min, sec fields.  All are optional, however
% at least *one* of day/hour/min/sec must be present to be valid.
reltime(S) --> sign(SF), days(D, DP), hms(HMS, HP), whitespace, { (DP+HP) > 0, S is SF * (86400*D + HMS) }.

sign(1) --> whitespace, "+".
sign(-1) --> whitespace, "-".
sign(1) --> "".

days(D, 1) --> whitespace, num(D), tdays.
days(0, 0) --> "".
tdays --> whitespace, "+".
tdays --> whitespace, "d".
tdays --> whitespace, "D".

hms(HMS, P) --> hmsfix(HMS, P).
hms(HMS, P) --> hmstag(HMS, P).

hmsfix(HMS, 3) --> whitespace, ge2num(H), ":", ge2num(M), ":", ge2num(S), fnum(F), { HMS is 3600*H + 60*M + S + F}.
hmstag(HMS, P) --> hours(H, HP), minutes(M, MP), seconds(S, SP), { P is HP+MP+SP, HMS is 3600*H + 60*M + S }.

hours(H, 1) --> whitespace, num(H), thours.
hours(0, 0) --> "".
thours --> whitespace, "h".
thours --> whitespace, "H".

minutes(M, 1) --> whitespace, num(M), tminutes.
minutes(0, 0) --> "".
tminutes --> whitespace, "m".
tminutes --> whitespace, "M".

seconds(S, 1) --> whitespace, num(TS), fnum(F), tseconds, { S is TS + F}.
seconds(0, 0) --> "".
tseconds --> whitespace, "s".
tseconds --> whitespace, "S".
tseconds --> "".

% consume a number, return with its value:
ge2num(N) --> dhead(0, T1), dhead(T1, T2), drest(T2, N).
num(N) --> dhead(0, T), drest(T, N).

% the decimal fraction of a number
fnum(F) --> ".", frest(0, 0.1, F).
fnum(0) --> "".
fhead(C, DF, F) --> [D], { char_type(D, digit), F is C + DF*(D-"0") }.
frest(C, DF, F) --> fhead(C, DF, T), { NDF is 0.1 * DF }, frest(T, NDF, F).
frest(C, _, C) --> "".

dhead(C, N) --> [D], { char_type(D, digit), N is (D-"0") + 10*C }.
drest(C, N) --> dhead(C, T), drest(T, N).
drest(N, N) --> "".

% consume whitespace
whitespace --> wschar, whitespace.
whitespace --> "".
wschar --> [C], { char_type(C, white) }.
