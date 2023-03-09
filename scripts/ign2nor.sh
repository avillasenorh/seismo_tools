#!/usr/bin/env bash

# converts IMS file to Nordic format
# If IMS file is named prefix.ims, output nordic file is named prefix.nor
# adds current date to STATUS? line

set -euo pipefail

progname=${0##*/}
[[ $# -lt 1 ]] && { echo "usage: $progname IMS_file"; exit 1; }

[[ ! -s $1 ]] && { echo "ERROR: IMS file does not exist: $1"; exit 1; }

imsfile="$1"
basename=${imsfile##*/}
prefix=${basename%.*}
norfile=$prefix.nor

command -v ign2nor > /dev/null 2>&1 || { echo "ERROR: ign2nor executable does not exist"; exit 1; }

/bin/rm -f tmp.nor $norfile

ign2nor << END > /dev/null
$imsfile
tmp.nor
END

status=$?
[[ $status -ne 0 || ! -s tmp.nor ]] && { echo "ERROR: conversion of $imsfile to Nordic failed"; exit 1; }

# New version of ign2nor adds date to nordic file
#date_string=$( $DATE "+%y-%m-%d %H:%M" )
#sed "s/ ACTION:NEW                OP/ ACTION:NEW $date_string OP/" tmp.nor > $norfile

/bin/mv -f tmp.nor $norfile
