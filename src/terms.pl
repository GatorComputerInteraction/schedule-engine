
% Valid Terms for Courses
term(fall).
term(spring).
term(summerA).
term(summerB).
term(summerC).

termFromString("Fall", fall).
termFromString("Spring", spring).
termFromString("Summer A", summerA).
termFromString("Summer B", summerB).
termFromString("Summer C", summerC).

% Semester data structure
semester(Term, Year) :-
  number(Year),
  term(Term).
