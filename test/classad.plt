:- add_to_path('../lib').
:- use_module(classad).

:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(date)).

:- begin_tests(classad).

test('new_classad-1') :-
    new_classad('[classad]'(M)),
    is_assoc(M),
    assoc_to_list(M, []).

test('is_classad-1') :-
    new_classad(CA),
    is_classad(CA).

test('is_classad-2') :-
    \+is_classad(a),
    \+is_classad('[classad]'(foo)).

test('classad_assign_native-1') :-
    new_classad(CA0),
    classad_assign_native(vV, "4+2", CA0, '[classad]'(M)),
    assoc_to_list(M, [vv-'+'(4,2)]).

test('classad_assign/4-1') :-
    new_classad(CA0),
    classad_assign(xX, 0, CA0, '[classad]'(M)),
    assoc_to_list(M, [xx-0]).

test('classad_assign/4-context') :-
    new_classad(CA0),
    classad_assign(xX, 0, [CA0], ['[classad]'(M)]),
    assoc_to_list(M, [xx-0]).

test('classad_assign/3-1') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([x1 = error,
                    x2 = undefined,
                    x3 = 42,
                    x4 = 3.14,
                    x5 = 'A String',
                    x6 = ['a', ['list']],
                    x7 = true,
                    x8 = CAZ
                    ],
                   CA0, '[classad]'(M)),
    assoc_to_list(M,
                  [x1-error, 
                   x2-undefined,
                   x3-42,
                   x4-3.14,
                   x5-'[str]'('A String'),
                   x6-['[str]'(a),['[str]'(list)]],
                   x7-true,
                   x8-'[classad]'(MZ)]),
    assoc_to_list(MZ, [z-1]).

test('classad_assign/3-time') :-
    new_classad(CA0),
    classad_assign(['AT1' = date(2012,2,24,6,12,31.0,25200,'MST',false),
                    'AT2' = abstime(1330089151.0, 25200),
                    'AT3' = reltime(60)
                    ],
                   CA0, '[classad]'(M)),
    assoc_to_list(M,
                  [at1-'[abstime]'(1330089151.0, 25200),
                   at2-'[abstime]'(1330089151.0, 25200),
                   at3-'[reltime]'(60)]).

test('classad_assign/3-mixed') :-
    new_classad(CA0),
    classad_assign([x1 = 42,
                    [x2, 3.14],
                    [x3, true, T3],
                    [x4, false, boolean],
                    [x5, "String", as(string)]],
                    CA0, '[classad]'(M)),
    T3 == boolean,
    assoc_to_list(M,
                  [x1-42,
                   x2-3.14,
                   x3-true,
                   x4-false,
                   x5-'[str]'('String')]).

test('classad_assign/3-qtype') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([[x1, error, T1],
                    [x2, undefined, T2],
                    [x3, 42, T3],
                    [x4, 3.14, T4],
                    [x5, 'A String', T5],
                    [x6, ['a', ['list']], T6],
                    [x7, true, T7],
                    [x8, CAZ, T8],
                    [x9, abstime(0,0), T9],
                    [x10, reltime(0), T10]
                    ],
                   CA0, _CA),
    T1 == error,
    T2 == undefined,
    T3 == integer,
    T4 == real,
    T5 == string,
    T6 == list,
    T7 == boolean,
    T8 == classad,
    T9 == abstime,
    T10 == reltime.

test('classad_assign/3-ftype') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([[x1, error, error],
                    [x2, undefined, undefined],
                    [x3, 42, integer],
                    [x4, 3.14, real],
                    [x5, 'A String', string],
                    [x6, ['a', ['list']], list],
                    [x7, true, boolean],
                    [x8, CAZ, classad],
                    [x9, abstime(0,0), abstime],
                    [x10, reltime(0), reltime]
                    ],
                   CA0, _CA).

test('classad_assign/3-ftype-fail') :-
    new_classad(CA0),
    \+classad_assign([[x1, 42, list]], CA0, _CA).

test('classad_assign/3-promotions') :-
    classad_eval:local_tzo(Z),
    new_classad(CA0),
    classad_assign([[x1, "a string", as(string)],
                    [x2, 60, as(reltime)],
                    [x3, 1.5, as(reltime)],
                    [x4, 1330089151, as(abstime)],
                    [x5, 1330089151.5, as(abstime)],
                    [x6, 1, as(real)],
                    [x7, 1, as(number)],
                    [x8, 1, as(integer)],
                    [x9, 1.0, as(real)],
                    [x91, 1.0, as(integer)],
                    [x92, 1.0, as(number)]
                   ], CA0, '[classad]'(M)),
    assoc_to_list(M, [x1-'[str]'('a string'),
                      x2-'[reltime]'(60),
                      x3-'[reltime]'(1.5),
                      x4-'[abstime]'(1330089151, Z),
                      x5-'[abstime]'(1330089151.5, Z),
                      x6-1.0,
                      x7-1,
                      x8-1,
                      x9-1.0,
                      x91-1,
                      x92-1.0
                    ]).

test('classad_lookup/3-1') :-
    new_classad(CA0),
    classad_assign(x, 'fred', CA0, CA),
    classad_lookup(x, CA, 'fred').

test('classad_lookup/4-qtype') :-
    new_classad(CA0),
    classad_assign(v, false, CA0, CA),
    classad_lookup(v, CA, false, T),
    T == boolean.

test('classad_lookup/4-qtype-novar') :-
    new_classad(CA0),
    classad_assign(v, false, CA0, CA),
    classad_lookup(x, CA, undefined, T),
    T == novar.

test('classad_lookup/4-ftype') :-
    new_classad(CA0),
    classad_assign(v, false, CA0, CA),
    classad_lookup(v, CA, false, boolean),
    \+classad_lookup(v, CA, false, list).

test('classad_lookup/3-context') :-
    classad_eval_native("[pi=3.14]", [], C),
    classad_eval_native("[r=2; a=pi*r*r]", [], CA),
    classad_lookup(a, [CA, C], 12.56).

test('classad_lookup/4-rescope') :-
    classad_eval_native("[x = 5]", [], O),
    classad_eval_native("[x = 4; b = x < other.x]", [], CA),
    classad_lookup(b, CA, [other=O], true).

test('classad_lookup/5-qtype') :-
    classad_eval_native("[x = 5]", [], O),
    classad_eval_native("[x = 4; b = x < other.x]", [], CA),
    classad_lookup(b, CA, [other=O], Val, Type),
    Val == true, Type == boolean.

test('classad_lookup/5-ftype') :-
    classad_eval_native("[x = 5]", [], O),
    classad_eval_native("[x = 4; b = x < other.x]", [], CA),
    classad_lookup(b, CA, [other=O], true, boolean),
    \+classad_lookup(b, CA, [other=O], true, string).

test('classad_lookup/2-1') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([[x1, error],
                    [x2, undefined],
                    [x3, 42],
                    [x4, 3.14],
                    [x5, 'A String'],
                    [x6, ['a', ['list']]],
                    [x7, true],
                    [x8, CAZ],
                    [x9, abstime(0,0)],
                    [x10, reltime(0)]
                    ],
                   CA0, CA),
    classad_lookup(CA,
                   [[x1, error],
                    [x2, undefined],
                    [x3, 42],
                    [x4, 3.14],
                    [x5, 'A String'],
                    [x6, ['a', ['list']]],
                    [x7, true],
                    [x8, CAZ],
                    [x9, abstime(0,0)],
                    [x10, reltime(0)]
                    ]).

test('classad_lookup/2-qtype') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([x1 = error,
                    x2 = undefined,
                    x3 = 42,
                    x4 = 3.14,
                    x5 = 'A String',
                    x6 = ['a', ['list']],
                    x7 = true,
                    x8 = CAZ,
                    x9 = abstime(0,0),
                    x10 = reltime(0)
                    ],
                   CA0, CA),
    classad_lookup(CA,
                   [[x1, error, T1],
                    [x2, undefined, T2],
                    [x3, 42, T3],
                    [x4, 3.14, T4],
                    [x5, 'A String', T5],
                    [x6, ['a', ['list']], T6],
                    [x7, true, T7],
                    [x8, CAZ, T8],
                    [x9, abstime(0,0), T9],
                    [x10, reltime(0), T10]
                    ]),
    T1 == error,
    T2 == undefined,
    T3 == integer,
    T4 == real,
    T5 == string,
    T6 == list,
    T7 == boolean,
    T8 == classad,
    T9 == abstime,
    T10 == reltime.

test('classad_lookup/2-ftype') :-
    classad_eval_native("[z=1]", [], CAZ),
    new_classad(CA0),
    classad_assign([x1 = error,
                    x2 = undefined,
                    x3 = 42,
                    x4 = 3.14,
                    x5 = 'A String',
                    x6 = ['a', ['list']],
                    x7 = true,
                    x8 = CAZ,
                    x9 = abstime(0,0),
                    x10 = reltime(0)
                    ],
                   CA0, CA),
    classad_lookup(CA,
                   [[x1, error, error],
                    [x2, undefined, undefined],
                    [x3, 42, integer],
                    [x4, 3.14, real],
                    [x5, 'A String', string],
                    [x6, ['a', ['list']], list],
                    [x7, true, boolean],
                    [x8, CAZ, classad],
                    [x9, abstime(0,0), abstime],
                    [x10, reltime(0), reltime]
                    ]).

test('classad_lookup/2-mixed') :-
    new_classad(CA0),
    classad_assign([x3 = 42,
                    x4 = 3.14],
                   CA0, CA),
    classad_lookup(CA,
                   [x3 = V3,
                    [x3, 42],
                    [x3, V3b],
                    [x3, 42, integer],
                    [x3, V3a, number],
                    [x4, 3.14, T4],
                    [x4, V4, T4b],
                    [x4, 3.14, number],
                    x5 = V5,
                    x5 = undefined,
                    [x5, undefined, T5],
                    [x5, undefined, novar]
                    ]),
    V3 == 42,
    V3a == 42,
    V3b == 42,
    T4 == real,
    V4 == 3.14,
    T4b == real,
    V5 == undefined,
    T5 == novar.

test('classad_lookup/2-context') :-
    new_classad(CA0),
    classad_assign([x3 = 42,
                    x4 = 3.14],
                   CA0, CA),
    classad_lookup([CA],
                   [x3 = V3,
                    [x3, 42],
                    [x3, V3b],
                    [x3, 42, integer],
                    [x3, V3a, number],
                    [x4, 3.14, T4],
                    [x4, V4, T4b],
                    [x4, 3.14, number],
                    x5 = V5,
                    x5 = undefined,
                    [x5, undefined, T5],
                    [x5, undefined, novar]
                    ]),
    V3 == 42,
    V3a == 42,
    V3b == 42,
    T4 == real,
    V4 == 3.14,
    T4b == real,
    V5 == undefined,
    T5 == novar.

test('classad_lookup/2-promotions') :-
    classad_eval:local_tzo(Z),
    new_classad(CA0),
    classad_assign([x1 = 42,
                    x2 = 3.14,
                    xri = 60,
                    xrr = 60.0,
                    xai = 1330089151,
                    xar = 1330089151.0,
                    at = abstime(1330089151, 25200),
                    rt = reltime(3600),
                    bt = true,
                    bf = false,
                    s = 'pebbles'],
                    CA0, CA),
    classad_lookup(CA,
                   [[x1, V1, as(real)], 
                    [x1, V1b, as(number)],
                    [x2, V2, as(integer)],
                    [x2, V2b, as(number)],
                    [xri, VRI, as(reltime)],
                    [xrr, VRR, as(reltime)],
                    [xai, VAI, as(abstime)],
                    [xar, VAR, as(abstime)],
                    [xai, VDI, as(date)],
                    [xar, VDR, as(date)],
                    [at, VDA, as(date)],
                    [at, VIA, as(integer)],
                    [at, VRA, as(real)],
                    [at, VNA, as(number)],
                    [rt, VIRb, as(integer)],
                    [rt, VRRb, as(real)],
                    [rt, VNRb, as(number)],
                    [bt, VBTI, as(integer)],
                    [bt, VBTR, as(real)],
                    [bt, VBTN, as(number)],
                    [bf, VBFI, as(integer)],
                    [bf, VBFR, as(real)],
                    [bf, VBFN, as(number)],
                    [s, VSC, as(codelist)]
                   ]),
    V1 == 42.0,
    V1b == 42,
    V2 == 3,
    V2b == 3.14,
    VRI == reltime(60),
    VRR == reltime(60.0),
    VAI == abstime(1330089151, Z),
    VAR == abstime(1330089151.0, Z),
    VDI == date(2012,2,24,6,12,31.0,Z,-,-),
    VDR == date(2012,2,24,6,12,31.0,Z,-,-),
    VDA == date(2012,2,24,6,12,31.0,25200,-,-),
    VIA == 1330089151,
    VRA == 1330089151.0,
    VNA == 1330089151,
    VIRb == 3600,
    VRRb == 3600.0,
    VNRb == 3600,
    VBTI == 1,
    VBTR == 1.0,
    VBTN == 1,
    VBFI == 0,
    VBFR == 0.0,
    VBFN == 0,
    VSC == "pebbles".

test('classad_lookup/3-rescope') :-
    classad_eval_native("[pi=3.14; e=2.72]", [], Math),
    classad_eval_native("[r = 2;  area = math.pi*r*r;  base = math.e]", [], CA),
    classad_lookup(CA, [math=Math],
                   [area = Area, 
                    base = Base,
                    [base, 2.72],
                    [area, 12.56, real],
                    [base, 2, as(integer)]
                   ]),
    Area == 12.56,
    Base = 2.72.


:- end_tests(classad).
