#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# Converts an XML document containing mixed serialization output from
# ColdFrame to comma-separated-variable files, one file per record
# name.
#
# The input document is read from standard input.

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

while {![eof stdin]} {
    gets stdin l
    if {[regexp {^<record name=\"([a-zA-Z0-9_.]+)\">$} $l wh name]} {
        set file $name.csv
        if [expr ![info exists files($file)]] {
            set firstLine 1
            set files($file) [open $file w]
        } else {
            set firstLine 0
        }
        set currentFile $files($file)
        set itemNo 1
        set done 0
        while {!$done} {
            gets stdin l
            if [eof stdin] {
                set done 1
            } elseif [regexp {^</record>$} $l wh] {
                set done 1
                if $firstLine {
                    for {set i 1} {$i < $itemNo} {incr i} {
                        puts -nonewline $currentFile "$names($i),"
                    }
                    puts $currentFile ""
                }
                for {set i 1} {$i < $itemNo} {incr i} {
                    puts -nonewline $currentFile "$items($i),"
                }
                puts $currentFile ""
            } elseif \
		{[regexp \
		      {^<field name=\"([a-zA-Z0-9_.]+)\">([^<]*)</field>$} \
		      $l wh n v]} {
			  set names($itemNo) $n
			  set items($itemNo) $v
			  incr itemNo
		      }
        }
    }
}
