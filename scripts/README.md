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

## ign2nor.sh

Converts a IMS file (IGN version) to SEISAN's Nordic format 1.
Requires `ign2nor` (this version converts Lg to Sg for events before 2016
and also reads arrival info and sets channel and component).
Some events in January-February 2016 are still with old format.

Usage:

    $ ign2nor.sh ims_file

Output is a file with same base name as the IMS file with `nor` extension.


## depth_ttimes.sh

GMT6 script to plot travel-time vs distance for a 1D layered model.
Uses SEISAN's `ttlayer` executable. Reads 1D model from STATION0.HYP (local file or at
the global location `$SEISAN_TOP/DAT`).


## depth_ttimes_reduced.sh

Same as `depth_ttimes.sh` but using a reduction velocity.


