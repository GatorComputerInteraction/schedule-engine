:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Paths
:- http_handler('/', handle_request, []).

% Handles requests to /
handle_request(Request) :-
  http_read_json_dict(Request, Query),
  solve(Query, Solution),
  reply_json_dict(Solution).

% Calculates a + b.
solve(_{a:X, b:Y}, _{answer:N}) :-
  number(X),
  number(Y),
  N is X + Y.

% Starts Server
server(Port) :-
  http_server(http_dispatch, [port(Port)]).
