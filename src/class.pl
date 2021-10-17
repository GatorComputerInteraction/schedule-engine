
% Class Data Structure
class(ClassTuple) :-
  ClassTuple = (ID, Name, CourseCredits, ClassMeetings),
  class(ID, Name, CourseCredits, ClassMeetings).
class(ID, Name, CourseCredits, ClassMeetings) :-
  number(ID),
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

dayByString("M", monday).
dayByString("T", tuesday).
dayByString("W", wednesday).
dayByString("R", thursday).
dayByString("F", friday).
dayByString("S", saturday).

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

periodByNumber(1, one).
periodByNumber(2, two).
periodByNumber(3, three).
periodByNumber(4, four).
periodByNumber(5, five).
periodByNumber(6, six).
periodByNumber(7, seven).
periodByNumber(8, eight).
periodByNumber(9, nine).
periodByNumber(10, ten).
periodByNumber(11, eleven).
periodByNumber(12, e1).
periodByNumber(13, e2).
periodByNumber(14, e3).
periodByNumber(15, online).
