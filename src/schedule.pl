:- ensure_loaded(class).
:- ensure_loaded(terms).
:- ensure_loaded(request).

% Returns Day-Period pairs
% Used for turning a list of periods in a day into the pairs, which can be checked
% for schedule uniqueness.
occupied_periods(_, [], _).
occupied_periods(Day, [Period|B], ReturnPeriodList) :-
  day(Day),
  period(Period),
  occupied_periods(Day, B, ExpandedL),
  !,
  append(ExpandedL, [(Day, Period)], ReturnPeriodList).

% Returns a class meeting list into a list of day-period pairs.
occupied_periods_days([], _).
occupied_periods_days([ClassMeeting|B], ReturnList) :-
  occupied_periods_days(B, OtherDaysL),
  ClassMeeting = (Day, Meeting),
  day(Day),
  period_range(Meeting),
  occupied_periods(Day, Meeting, ExpandedL),
  !,
  append(OtherDaysL, ExpandedL, ReturnList).

% Schedule data structure, list of classes
schedule([]).
schedule([(ID, Name, CourseCredits, ClassMeetings)|B]) :-
  class(ID, Name, CourseCredits, ClassMeetings),
  schedule(B).

% Returns all periods that are occupied by a schedule.
% Can be used to then determine if there are any period conflicts.
schedule_occupied_periods([], _).
schedule_occupied_periods([(ID, Name, CourseCredits, ClassMeetings)|B], ReturnList) :-
  class(ID, Name, CourseCredits, ClassMeetings),
  schedule_occupied_periods(B, OtherCoursesL),
  occupied_periods_days(ClassMeetings, ExpandedL),
  !,
  append(OtherCoursesL, ExpandedL, ReturnList).

% True if all members of list are unique
unique([]).
unique([H|T]) :-
  not(member(H,T)),
  !,
  unique(T).

% True if schedule is free of period conflicts, classes are unique and under 18 credits.
valid_schedule(Schedule) :-
  schedule_occupied_periods(Schedule, OccupiedPeriods),
  schedule_credits(Schedule, Credits),
  !,
  Credits =< 18,
  unique(Schedule),
  unique(OccupiedPeriods).

% Calculates the number of credits in a schedule
schedule_credits([], 0).
schedule_credits([(ID, Name, Credit, Schedule)|B], ReturnCredits) :-
  schedule_credits(B, OtherCourseCredits),
  class(ID, Name, Credit, Schedule),
  !,
  plus(Credit, OtherCourseCredits, ReturnCredits).

% True if all courses are avaliable in a semester.
classes_avaliable(_, []).
classes_avaliable(Courses, [Course|B]) :-
  member(Course,Courses),
  classes_avaliable(Courses, B).

% TODO Ensure the courselist has separate class items for each "class instance"
% True if schedule is valid for a given semester
valid_avaliable_schedule(Term, Year, CourseList, Schedule) :-
  semester(Term, Year),
  !,
  findall((ID, Name, Credit, ClassSchedule), avaliable_course(CourseList, ID, Term, Year, Name, Credit, ClassSchedule), Courses),
  !,
  maplist(avaliable_course(CourseList), Schedule),
  maplist(avaliable_course(CourseList), Schedule, ScheduleTuple),
  valid_schedule(ScheduleTuple),
  classes_avaliable(Courses, ScheduleTuple).