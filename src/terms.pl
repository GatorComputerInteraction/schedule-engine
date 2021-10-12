
% Valid Terms for Courses
term(fall).
term(spring).
term(summerA).
term(summerB).
term(summerC).

% Semester data structure
semester(Term, Year) :-
  number(Year),
  term(Term).
