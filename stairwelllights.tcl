#!/bin/sh
# $Id: stairwelllights.tcl,v 6ffd7145311b 2002/12/18 10:11:02 simon $
# HCI for ColdFrame stairwell lights demo
# the next line restarts using ./stairwell_demo \
exec ./stairwell_demo "$0" "$@"

set title "Stairwell demo"

wm title . $title

frame .panel -width 10c -height 5c
pack .panel -side top -fill x

frame .panel.l
frame .panel.r
grid .panel.l -row 0 -column 0 -sticky ew
grid .panel.r -row 0 -column 1 -sticky ew

foreach f {0 1 2 3} {
    button .panel.l.$f -text $f -command "pushButton $f"
    pack .panel.l.$f -side bottom -anchor w
}

foreach l {a b c} {
    label .panel.r.$l -text $l -background black -foreground yellow
    pack .panel.r.$l -side bottom -anchor w -padx 2m -pady 2m
}
after 100 checkLampProc


proc checkLampProc {} {
    foreach l {a b c} {
	set s [getLampState $l]
	if {$s} {
	    .panel.r.$l configure -background yellow -foreground black
	} else {
	    .panel.r.$l configure -background black -foreground yellow
	}
    }
    after 100 checkLampProc
}

# for emacs:
# Local Variables:
# tcl-default-application: "stairwell_demo"
# End:
