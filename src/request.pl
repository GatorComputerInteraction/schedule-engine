:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- ensure_loaded(class).
:- ensure_loaded(terms).

% Converts String in the form of "M,W,F" to a list of day atoms.
day_string_to_atom(String, ResultList) :-
  string(String),
  split_string(String, ",", "", StringList),
  maplist(dayByString, StringList, ResultList).

% Makes http GET request for json.
get_json_endpoint(Path, Result) :-
  getenv('BASE_URL', BaseUrl),
  string_concat(BaseUrl, Path, URL),
  http_open(URL, In, []),
  json_read_dict(In, Result),
  close(In).

% Ensures course id and instance match.
find_instance_by_course_id(CourseID, _{instanceId: _, semester: _, year: _, courseId: CourseID, slotId: _, courseModel: _, timeslotModel: _}).

find_degree_course_by_id(DegreeId, _{ degreeId: DegreeId, courseId: _, degreeModel: _, courseModel: _ }).

select_course_id(_{ degreeId: _, courseId: Id, degreeModel: _, courseModel: _ }, Id).

% Extracts slot id from instance id json
slot_id_from_instance(_{courseId:_,courseModel:_,instanceId:_,semester:_,slotId: ID,timeslotModel:_,year:_}, ID).

% Builds period day pairs
% TODO: Remove single item list for period, needs to be modified in schedule.pl
build_period_day_pairs(Period, Day, (Day, [Period])).

% Takes a list of days and a period number and produces valid class meeting pairs.
build_day_period_list(_, [], []).
build_day_period_list(_, [PossiblyNull|_], []) :- \+ periodByNumber(PossiblyNull, _).
build_day_period_list(Days, [PeriodNumber|B], List) :-
  build_day_period_list(Days, B, ListTail),
  !,
  periodByNumber(PeriodNumber, Period),
  maplist(build_period_day_pairs(Period), Days, Pairs),
  append(Pairs, ListTail, List).

% Joins timeslot with the instance id, converting json values into usable atoms and data structures.
timeslot_from_instance_id(SlotIds, _{slotId: SlotId, day: _, periodId1: _, periodId2: _, periodId3: _, periodMode1l: _, periodModel2: _, periodModel3: _}, []) :-
  \+ member(SlotId, SlotIds).
timeslot_from_instance_id(SlotIds, _{slotId: SlotId, day: DayString, periodId1: PeriodOne, periodId2: PeriodTwo, periodId3: PeriodThree, periodMode1l: _, periodModel2: _, periodModel3: _}, MeetingList) :-
  member(SlotId, SlotIds),
  day_string_to_atom(DayString, Days),
  build_day_period_list(Days, [PeriodOne, PeriodTwo, PeriodThree], MeetingList).

% Combines a course with it's instances and time slots.
join_course_data(CourseInstances, TimeSlots, _{courseId: ID, courseName: Name, credits: Credits}, (ID, Semester, Year, Name, Credits, MeetingList)) :-
  include(find_instance_by_course_id(ID), CourseInstances, InstancesForID),
  InstancesForID = [_{courseId:_,courseModel:_,instanceId:_,semester: SemesterString ,slotId: _,timeslotModel:_,year: Year}|_],
  termFromString(SemesterString, Semester),
  maplist(slot_id_from_instance, InstancesForID, SlotIds),
  maplist(timeslot_from_instance_id(SlotIds), TimeSlots, MeetingListNested),
  !,
  foldl(append, MeetingListNested, [], MeetingList).

% True if course has no instances.
course_empty(CourseInstances, _{courseId: ID, courseName: _, credits: _}) :-
  include(find_instance_by_course_id(ID), CourseInstances, []).

% Returns needed JSON objects from the endpoint.
get_courses_endpoints(Courses, CourseInstances, TimeSlots) :-
  get_json_endpoint('/course', Courses),
  get_json_endpoint('/courseinstance', CourseInstances),
  get_json_endpoint('/timeslot', TimeSlots).

get_required_courses(DegreeId, CourseIdList) :-
  get_json_endpoint('/DegreeCourse', DegreeCourseList),
  include(find_degree_course_by_id(DegreeId), DegreeCourseList, FilteredDegrees),
  maplist(select_course_id, FilteredDegrees, CourseIdList).

% Returns a joined list of all classes from the endpoint.
get_courses_state(CourseData) :-
  get_courses_endpoints(Courses, CourseInstances, TimeSlots),
  exclude(course_empty(CourseInstances), Courses, ValidCourses),
  maplist(join_course_data(CourseInstances, TimeSlots), ValidCourses, CourseData).

% Searches the course list of a given class, given parameters.
avaliable_course(CourseData, ID) :-
  avaliable_course(CourseData, ID, _, _, _, _, _).
avaliable_course(CourseData, ID, (ID, Name, Credits, Schedule)) :-
  member((ID, _, _, Name, Credits, Schedule), CourseData).
avaliable_course(CourseData, ID, Term, Year, Name, Credits, Schedule) :-
  member((ID, Term, Year, Name, Credits, Schedule), CourseData).
