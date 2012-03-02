% This example demonstrates the classad_match predicate for matching two
% classads or contexts.

% use the classad module for this example:
:- add_to_path('../lib').
:- use_module(classad).

:-
% A classad representing a job, with requirements:
classad_eval_native("[reqs = (mem <= resource.mem) && (disk <= resource.disk);
    mem = 1024;
    disk = 2048;
]", [], JobCA),

% A classad representing a resource, with requirements:
classad_eval_native("[reqs = (mem >= job.mem) && (disk >= job.disk);
    mem = 2048;
    disk = 4096;
]", [], ResourceCA),

% Try to match a job classad against a resource classad.
% Do the match using the variable 'reqs' for both classads: declare rescoping
% variable 'resource' for the job ad, and 'job' for the resource ad.
(classad_match(JobCA, ResourceCA, reqs, resource=job)
    -> format("Ads Matched!\n") 
    ;  format("Ads did not match!\n")),


% Here is a classad that will be used for additional context:
classad_eval_native("[KB = 1024; MB = 1024*KB; GB = 1024*MB]", [], UnitCA),

% A classad representing a job, with requirements:
classad_eval_native("[reqs = (mem <= target.mem) && (disk <= target.disk);
    mem = 1024 * MB;
    disk = 2048 * GB;
]", [], JobCA2),

% A classad representing a resource, with requirements:
classad_eval_native("[requirements = (mem >= target.mem) && (disk >= target.disk);
    mem = 2048 * MB;
    disk = 4096 * MB;
]", [], ResourceCA2),

% Match two classad Contexts.  Here, we specify that 'reqs' is to be matched against
% 'requirements', and that both ads use the 'target' rescoping variable:
(classad_match([JobCA2, UnitCA], [ResourceCA2, UnitCA], reqs=requirements, target)
    -> format("Ads Matched!\n") 
    ;  format("Ads did not match!\n")),

% end example
true.
