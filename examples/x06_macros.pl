% This example demonstrates the parse-time macro evaluation enhancement to
% the classad language spec.

% use the classad module for this example:
:- add_to_path('../lib').
:- use_module(classad).


% a utility for reporting on classad values:
report(Var, Classad) :-
    classad_lookup(Var, Classad, Value, Type),
    format("~a= ~q    type= ~q\n", [Var, Value, Type]).


:-
% A simple classad that makes use of macro evaluation:
classad_eval_native("
[
    x = 1+2;
    # assign y to the current macro-eval of x, which is 3:
    y = $x;
    # use macro expansion of an expression to increment x:
    x = $(x+1);
    # z will get the most-recent value of x, which is 4:
    z = $x;
]
", [], CA1),

% lookup and output the values from CA1 to verify behavior:
format("\nfirst example (CA1):\n"),
report(x, CA1),
report(y, CA1),
report(z, CA1),


% a classad example inspired by condor config files
classad_eval_native("
[
    condor_host = 'big_cheese.pool.com';

    # in real life this would be obtained from a registered function:
    hostname = 'worker_bee.pool.com';

    log = '/var/log/condor';

    # this evaluates log at parse time:
    schedd_log = $log + '/ScheddLog';

    # this defers evaluation of log:
    master_log = log + '/MasterLog';

    # this evaluates entire concatenation at parse time:
    start_log = $(log + '/StartLog');

    # classads support first-class lists:
    daemon_list = { 'MASTER' };

    # macro eval is backed up by full classad eval() semantics:
    local_daemons = $((hostname =?= condor_host) ? { 'COLLECTOR', 'SCHEDD', 'NEGOTIATOR' } : { 'STARTD' });

    # append to daemon list:
    daemon_list = $(daemon_list + local_daemons);
]
", [], CA2),

format("\nsecond example (CA2):\n"),
report(master_log, CA2),
report(schedd_log, CA2),
report(start_log, CA2),
report(daemon_list, CA2),

% value of master_log can change if classad is updated, but
% the other two will not change as their values were fixed at parse time:
classad_assign(log, '/home/condor/log', CA2, CA3),

format("\nthird example (CA3):\n"),
report(master_log, CA3),
report(schedd_log, CA3),
report(start_log, CA3),

% end example
true.
