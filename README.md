# Scheduler Engine
*Gator Computer Interaction UF HCI Fall 2021*

## Prerequisites

* [SWI Prolog](https://www.swi-prolog.org/Download.html)

## Getting Started

```
# Start Webserver
swipl entry.pl
```

To halt the webserver once running, use the key combination `Ctrl+D` to halt the `swipl` terminal, or type `halt.` and press enter.

# Routes

## POST `/schedule/valid`

Returns whether or not the given schedule is valid.

Example:

```
POST
{
  "term": "Fall",
  "year": 2021,
  "course_ids": [1, 2, 4]
}

Returns
{
  "valid": false
}
```

## POST `/schedule/complete`

Returns a valid schedule given the beginnings of a schedule.

Example:

```
POST
{
  "term": "Fall",
  "year": 2021,
  "current_course_ids": [1],
  "min_credits": 12,
  "max_credits": 18,
  "max_classes": 4
}

Returns
{
  "schedule": [
    1,
    2,
    5
  ]
}
```

## POST `/schedule/predict_grad`

Roughly predicts the graduation year and semester given current completed classes and total required classes.

Example:

```
POST
{
  "current_term": "Fall",
  "current_year": 2021,
  "required_course_ids": [1, 2, 3, 5, 6],
  "completed_course_ids": [1],
  "max_fall_spring_credits": 4,
  "max_total_summer_credits": 4
}

Returns
{
  "expected_graduation_semester": "spring",
  "expected_graduation_year": 2023
}
```
