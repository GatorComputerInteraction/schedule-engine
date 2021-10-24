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
  !,
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
valid_schedule(Schedule, Credits) :-
  schedule_occupied_periods(Schedule, OccupiedPeriods),
  !,
  schedule_credits(Schedule, Credits),
  unique(Schedule),
  unique(OccupiedPeriods).

% Calculates the number of credits in a schedule
schedule_credits([], 0).
schedule_credits([(_, _, Credit, _)|B], ReturnCredits) :-
  schedule_credits(B, OtherCourseCredits),
  plus(Credit, OtherCourseCredits, ReturnCredits).

% True if all courses are avaliable in a semester.
classes_avaliable(_, []).
classes_avaliable(Courses, [Course|B]) :-
  member(Course,Courses),
  classes_avaliable(Courses, B).

% TODO Ensure the courselist has separate class items for each "class instance"
% True if schedule is valid for a given semester
valid_avaliable_schedule(Term, Year, CourseList, Schedule, Credits) :-
  semester(Term, Year),
  findall((ID, Name, Credit, ClassSchedule), avaliable_course(CourseList, ID, Term, Year, Name, Credit, ClassSchedule), Courses),
  maplist(avaliable_course(CourseList), Schedule, ScheduleTuple),
  Credits =< 18,
  classes_avaliable(Courses, ScheduleTuple),
  valid_schedule(ScheduleTuple, Credits).

list_difference(ListA, ListB, Difference):- findall(X, (member(X, ListA), not(member(X, ListB))), Difference).


total_course_credit_list_by_id(_, [], 0).
total_course_credit_list_by_id(CourseList, [CourseId|B], Credits) :-
  total_course_credit_list_by_id(CourseList, B, TailedCredits),
  avaliable_course(CourseList, CourseId, _, _, _, FoundCredits, _),
  plus(FoundCredits, TailedCredits, Credits).

roll_forward_remaining_credits(FinalTerm, RemainingCredits, _, _, FinalTerm) :-
  RemainingCredits =< 0.
roll_forward_remaining_credits(summerC, RemainingCredits, MaxFallSpringCredits, MaxSummerCredits, FinalTerm) :-
  roll_forward_remaining_credits(fall, RemainingCredits - MaxSummerCredits, MaxFallSpringCredits, MaxSummerCredits, FinalTerm).
roll_forward_remaining_credits(spring, RemainingCredits, MaxFallSpringCredits, MaxSummerCredits, FinalTerm) :-
  roll_forward_remaining_credits(summerC, RemainingCredits - MaxFallSpringCredits, MaxFallSpringCredits, MaxSummerCredits, FinalTerm).
roll_forward_remaining_credits(fall, RemainingCredits, MaxFallSpringCredits, MaxSummerCredits, FinalTerm) :-
  roll_forward_remaining_credits(spring, RemainingCredits - MaxFallSpringCredits, MaxFallSpringCredits, MaxSummerCredits, FinalTerm).

get_credits_remaining_in_year(fall, _, _, 0).
get_credits_remaining_in_year(spring, MaxFallSpringCredits, MaxSummerCredits, MaxSummerCredits + MaxFallSpringCredits).
get_credits_remaining_in_year(summerA, MaxFallSpringCredits, _, MaxFallSpringCredits).
get_credits_remaining_in_year(summerB, MaxFallSpringCredits, _, MaxFallSpringCredits).
get_credits_remaining_in_year(summerC, MaxFallSpringCredits, _, MaxFallSpringCredits).

predict_graduation_year(CreditsRemaining, MaxFallSpringCredits, MaxSummerCredits, CurrentTerm, CurrentYear, CurrentYear) :-
  get_credits_remaining_in_year(CurrentTerm, MaxFallSpringCredits, MaxSummerCredits, RemainingCreditsInYear),
  RemainingCreditsInYear >= CreditsRemaining.
predict_graduation_year(CreditsRemaining, MaxFallSpringCredits, MaxSummerCredits, _, CurrentYear, FinalYear) :-
  MaxCreditsPerYear is (MaxFallSpringCredits * 2) + MaxSummerCredits,
  FinalYear is ceil(CreditsRemaining / MaxCreditsPerYear) + CurrentYear.

predict_graduation(CurrentTerm, CurrentYear, CourseList, CompletedCourses, RequiredCourses, MaxFallSpringCredits, MaxSummerCredits, PredictedTerm, PredictedYear) :-
  list_difference(RequiredCourses, CompletedCourses, RemainingCourses),
  total_course_credit_list_by_id(CourseList, RemainingCourses, TotalCreditsToTake),
  predict_graduation_year(TotalCreditsToTake, MaxFallSpringCredits, MaxSummerCredits, CurrentTerm, CurrentYear, PredictedYear),
  MaxCreditsPerYear is (MaxFallSpringCredits * 2) + MaxSummerCredits,
  roll_forward_remaining_credits(CurrentTerm, TotalCreditsToTake mod MaxCreditsPerYear, MaxFallSpringCredits, MaxSummerCredits, PredictedTerm).
  
