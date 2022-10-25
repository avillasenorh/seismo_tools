# Miscellaneous utility scripts:

## getdate

Providing the year and day of year (also referred to as Julian day), returns
date in ISO 8601 format: YYYY-MM-DD.

Usage:

    $ getdate
    usage: getdate year day_of_year

Example:

    $ getdate 1990 151
    1990-05-31

## getjul

Providing a date in ISO 8601 format (or as YYYY MM DD), returns the day
of year (also referred to as Julian day).

Usage:

    $ getjul
    usage: getjul year month day | year-month-day

Examples:

    $ getjul 1990 5 31
    151
    $ getjul 1990-05-31
    151


