#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# Copyright (C) Simon Wright <simon@pushface.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

# $Id: split-csv.tcl,v 873ac776fc89 2004/02/10 09:44:27 simon $

# Splits a CSV file containing a header line and data lines into
# multiple CSV files, depending on the value in a particular column.
#
# Invocation:
#
#  split-csv.tcl column input-file
#
# To split foo.csv on column 1, say
#
#  split-csv.tcl 1 foo.csv
#
# If foo.csv contains
#
#  side,length
#  left,0.1
#  RIGHT,0.4
#  left,1.7
#
# then there will be two output files:
#
# foo.left.csv
#  side,length
#  left,0.1
#  left,1.7
#
# foo.RIGHT.csv
#  side,length
#  RIGHT,0.4
#
# You can name columns 1-n or a-z (anything over z, use the number).

proc process {from baseName col} {

    # convert lettered columns (a-z only) to numbers
    set col [string tolower $col]
    if {[regexp {^[a-z]$} $col]} {
        scan $col %c c
        scan "a" %c a
        set col [expr $c - $a + 1]
    } elseif {![regexp {[0-9]+} $col]} {
        puts stderr "column $col not recognised"
        exit 1
    }

    # make a regex which extracts the required column
    set rgx "^"
    for {set c 1} {$c < $col} {incr c} {
        set rgx [format "%s\[^,\]*," $rgx]
    }
    set rgx [format "%s(\[^,\]+)" $rgx]

    # we need the header line to copy into every output file
    gets $from firstLine

    while {![eof $from]} {

        # read the next line
        gets $from l

        # if there was something to read ..
        if {![eof $from]} {

            # extract the required column
            if {![regexp $rgx $l wh field]} {

                puts stderr "failed to find column $col in line $l"

            } else {

                # remove white space
                set field [string trim $field]

                # check for sensible content (we don't want hundreds
                # of files to be created if the user names a numeric
                # field by mistake!)
                if {![regexp {^[a-z]} [string tolower $field]]} {
                    puts stderr "split column contains \"$field\", \
                                 not splitting"
                    exit 1
                }

                # make the file name
                set file "$baseName.$field.csv"

                # create it if necessary
                if [expr ![info exists files($file)]] {

                    # report the new filename
                    puts stderr "opening file $file"

                    # open the file
                    set files($file) [open $file w]

                    # output the header line
                    puts $files($file) $firstLine
                }

                # output the line just read
                puts $files($file) $l

            }
        }
    }
    # should close the files ..
}

switch $argc {
    0 {
        puts stderr "must specify column"
        exit 1
    }
    1 {
        process stdin "stdin" [lindex $argv 0]
    }
    2 {
        set fileName [lindex $argv 1]
        set file [open $fileName r]
        set fileName [exec basename $fileName .csv]
        process $file $fileName [lindex $argv 0]
        close $file
    }
    default {
        puts stderr "too many arguments"
        exit 1
    }
}
