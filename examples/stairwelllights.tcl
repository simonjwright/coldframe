#!/bin/sh
# $Id$
# HCI for ColdFrame stairwell lights demo
# the next line restarts using ./stairwell_demo \
exec ./stairwell_demo "$0" "$@"

set title "Stairwell demo"

wm title . $title

# The image used is Lansing's Castle
# (http://www.sos.state.mi.us/history/archive/exhibits/barnes.html).
image create photo pic -file house-2.gif

canvas .c -width 772 -height 691
.c create image [expr 772 / 2] [expr 691 / 2] -image pic
pack .c -side top -fill x

# lamps; top floor downwards
.c create oval 316 280 324 288 -width 1 -outline black -fill gray -tags a
.c create oval 316 386 324 394 -width 1 -outline black -fill gray -tags b
.c create oval 316 487 324 495 -width 1 -outline black -fill gray -tags c
.c create oval 330 588 338 596 -width 1 -outline black -fill gray -tags d

# buttons; top floor downwards.
# pushButton is exported from Ada, the argument is the button number.
.c bind \
    [.c create rectangle 339 322 347 330 -width 1 -outline black -fill green] \
    <Button-1> {pushButton 0}
.c bind \
    [.c create rectangle 338 418 346 426 -width 1 -outline black -fill green] \
    <Button-1> {pushButton 1}
.c bind \
    [.c create rectangle 338 525 346 533 -width 1 -outline black -fill green] \
    <Button-1> {pushButton 2}
.c bind \
    [.c create rectangle 341 600 349 608 -width 1 -outline black -fill green] \
    <Button-1> {pushButton 3}

after 100 checkLampProc

proc checkLampProc {} {
    foreach l {a b c d} {
	# getLampState is exported from Ada, the argument is the lamp letter.
	set s [getLampState $l]
	if {$s} {
	    .c itemconfigure $l -fill yellow
	} else {
	    .c itemconfigure $l -fill gray
	}
    }
    after 100 checkLampProc
}

# for emacs:
# Local Variables:
# tcl-default-application: "stairwell_demo"
# End:
