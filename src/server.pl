:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- ensure_loaded(request).
:- ensure_loaded(schedule).

% Paths
%:- http_handler('/finishby', handle_finish_by_request, []).

:- http_handler('/complete', handle_complete_request, []).
:- http_handler('/valid', handle_valid_schedule_request, []).
:- http_handler('/', handle_liveness, []).

handle_liveness(_) :-
  reply_json_dict(_{ alive: true }).

handle_valid_schedule_request(Request) :-
  http_read_json_dict(Request, Query),
  is_valid_schedule(Query, Response),
  reply_json_dict(Response).

handle_complete_request(Request) :-
  http_read_json_dict(Request, Query),
  complete_schedule(Query, Response),
  reply_json_dict(Response).

complete_schedule(_{term: Term, year: Year, current_course_ids: UIDs, min_credits: MinCredits, max_credits: MaxCredits, max_classes: MaxClasses}, _{schedule: Schedule}) :-
  termFromString(Term, SemesterAtom),
  get_courses_state(CourseData),
  length(UIDs, CurrentClassCount),
  !,
  between(MinCredits, MaxCredits, Credits),
  between(CurrentClassCount, MaxClasses, ClassCount),
  length(Schedule, ClassCount),
  append(UIDs, _, Schedule),
  valid_avaliable_schedule(SemesterAtom, Year, CourseData, Schedule, Credits).

is_valid_schedule(_{term: Term, year: _, course_ids: _}, _{error: "Invalid Term"}) :-
  \+ termFromString(Term, _).
is_valid_schedule(_{term: _, year: Year, course_ids: _}, _{error: "Invalid Year"}) :-
  \+ number(Year).
is_valid_schedule(_{term: Term, year: Year, course_ids: _}, _{error: "Invalid Semester"}) :-
  termFromString(Term, SemesterAtom),
  \+ semester(SemesterAtom, Year).
is_valid_schedule(_{term: Term, year: Year, course_ids: UIDs}, _{valid: true}) :-
  termFromString(Term, SemesterAtom),
  get_courses_state(CourseData),
  valid_avaliable_schedule(SemesterAtom, Year, CourseData, UIDs, _).
is_valid_schedule(_{term: _, year: _, course_ids: _}, _{valid: false}).
is_valid_schedule(_, _{error: "Invalid Request"}).

% Starts Server
server(Port) :-
  http_server(http_dispatch, [port(Port)]).
