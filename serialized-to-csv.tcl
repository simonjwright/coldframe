#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# $Id: serialized-to-csv.tcl,v 243967d6b136 2004/01/12 12:52:11 simon $

# Converts a document containing mixed serialization output from
# ColdFrame to comma-separated-variable files, one file per record
# name.

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

package require xml

#######################
# XML parse interface #
#######################

proc startTag {tag attrs} {
    global files currentFile firstLine itemNo item names
    switch $tag {
        record  {
            array set attr $attrs
            set file $attr(name).csv
            if [expr ![info exists files($file)]] {
                set firstLine 1
                set files($file) [open $file w]
            } else {
                set firstLine 0
            }
            set currentFile $files($file)
            set itemNo 1
        }
        field {
            array set attr $attrs
            set names($itemNo) $attr(name)
            set item ""
        }
        default {}
    }
}

proc textInTag {str} {
    global item
    set item "$item$str"
}

proc endTag {tag} {
    global currentFile firstLine item itemNo items names
    switch $tag {
        field {
            set items($itemNo) $item
            incr itemNo
        }
        record {
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
        }
        default {}
    }
}

###########
# Globals #
###########

set firstLine 0
set item ""
set stackDump 1

################
# Main program #
################

set parser [xml::parser]
$parser configure \
        -elementstartcommand startTag \
        -elementendcommand endTag \
        -characterdatacommand textInTag

set stackDump 1
if $stackDump {
    $parser parse [read stdin]
} else {
    if [catch {$parser parse [read stdin]} msg] {
        puts stderr "CF: internal error: $msg"
        exit 1
    }
}
