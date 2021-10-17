:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- ensure_loaded(request).

% Paths
%:- http_handler('/finishby', handle_finish_by_request, []).
%:- http_handler('/complete', handle_complete_request, []).
:- http_handler('/valid', handle_valid_schedule_request, []).

handle_valid_schedule_request(Request) :-
  http_read_json_dict(Request, Query),
  solve(Query, Solution),
  reply_json_dict(Solution).

is_valid_schedule(_{course_ids: UIDs}, _{valid:Result}) :-
  get_courses_state(Courses),
  valid_avaliable_schedule

% Calculates a + b.
solve(_{a:X, b:Y}, _{answer:N}) :-
  number(X),
  number(Y),
  N is X + Y.

% Starts Server
server(Port) :-
  http_server(http_dispatch, [port(Port)]).
