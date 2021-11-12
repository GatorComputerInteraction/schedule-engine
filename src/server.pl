:- use_module(library(http/http_cors)).
:- set_setting(http:cors, [*]).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_json)).

:- ensure_loaded(request).
:- ensure_loaded(schedule).

% Paths
%:- http_handler('/finishby', handle_finish_by_request, []).

:- http_handler('/schedule/complete', handle_complete_request, []).
:- http_handler('/schedule/valid', handle_valid_schedule_request, []).
:- http_handler('/schedule/predict_grad', handle_predict_graduation, []).
:- http_handler('/schedule', handle_liveness, []).

handle_liveness(_) :-
  option(method(get), Request), !,
  cors_enable,
  reply_json_dict(_{ alive: true }).

handle_liveness(Request) :-
  option(method(options), Request), !,
  cors_enable(Request,
    [ methods([get])
    ]),
  format('~n').

handle_valid_schedule_request(Request) :-
  option(method(post), Request), !,
  cors_enable,
  http_read_json_dict(Request, Query),
  is_valid_schedule(Query, Response),
  reply_json_dict(Response).

handle_valid_schedule_request(Request) :-
  option(method(options), Request), !,
  cors_enable(Request,
    [ methods([post])
    ]),
  format('~n').

handle_complete_request(Request) :-
  option(method(post), Request), !,
  cors_enable,
  http_read_json_dict(Request, Query),
  complete_schedule(Query, Response),
  reply_json_dict(Response).

handle_complete_request(Request) :-
  option(method(options), Request), !,
  cors_enable(Request,
    [ methods([post])
    ]),
  format('~n').

handle_predict_graduation(Request) :-
  option(method(post), Request), !,
  cors_enable,
  http_read_json_dict(Request, Query),
  predict_graduation(Query, Response),
  reply_json_dict(Response).

handle_predict_graduation(Request) :-
  option(method(options), Request), !,
  cors_enable(Request,
    [ methods([post])
    ]),
  format('~n').

predict_graduation(_{current_term: CurrentTerm, current_year: CurrentYear, required_course_ids: RequiredCourses, completed_course_ids: CompletedCourses, max_fall_spring_credits: MaxFallSpringCredits, max_total_summer_credits: MaxSummerCredits }, _{ expected_graduation_year: PredictedYear, expected_graduation_semester: PredictedTerm }) :-
  termFromString(CurrentTerm, SemesterAtom),
  get_courses_state(CourseData),
  predict_graduation(SemesterAtom, CurrentYear, CourseData, CompletedCourses, RequiredCourses, MaxFallSpringCredits, MaxSummerCredits, PredictedTerm, PredictedYear).

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

:- initialization http_daemon.
