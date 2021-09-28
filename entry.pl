:- ensure_loaded(src/server).

% Service Entrypoint

:- initialization(server(8000), program).
