
% Class Data Structure
class(ClassTuple) :-
  ClassTuple = (Name, CourseCredits, ClassMeetings),
  class(Name, CourseCredits, ClassMeetings).
class(Name, CourseCredits, ClassMeetings) :-
  string(Name),
  number(CourseCredits),
  class_meetings(ClassMeetings).

% Class Meeting time
class_meeting(Day, PeriodRange) :-
  day(Day),
  period_range(PeriodRange).

% Series of Class Meeting times. as seed for a course schedule
class_meetings([]).
class_meetings([(Day, PeriodRange)|B]) :-
  is_list(B),
  class_meeting(Day, PeriodRange),
  class_meetings(B).

% Days classes can occur on
day(monday).
day(tuesday).
day(wednesday).
day(thursday).
day(friday).
day(saturday).

% List of valid periods
period_range([]).
period_range([F|B]) :-
  is_list(B),
  period(F),
  period_range(B).

% Class Periods used by ONE UF
period(one).
period(two).
period(three).
period(four).
period(five).
period(six).
period(seven).
period(eight).
period(nine).
period(ten).
period(eleven).
period(e1).
period(e2).
period(e3).
period(online).
