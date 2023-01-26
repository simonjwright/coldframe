#!/bin/sh
# HCI for ColdFrame stairwell lights demo
# the next line restarts using ./stairwell_demo \
exec ./stairwell_demo "$0" "$@"

package require Img

set title "Stairwell demo"

wm title . $title

# The image used is Lansing's Castle
# (http://www.sos.state.mi.us/history/archive/exhibits/barnes.html).
image create photo pic -file ../doc/house-2.jpg

canvas .c -width 772 -height 691
.c create image [expr 772 / 2] [expr 691 / 2] -image pic
pack .c -side top -fill x

# lamps; top floor downwards
.c create oval 316 280 324 288 -width 1 -outline black -fill gray -tags a
.c create oval 316 386 324 394 -width 1 -outline black -fill gray -tags b
.c create oval 316 487 324 495 -width 1 -outline black -fill gray -tags c
.c create oval 330 588 338 596 -width 1 -outline black -fill gray -tags d

# buttons; top floor downwards.

# pushButton is exported from Ada, the argument is the button number
# and the second (optional) argument is the button state.
.c create rectangle 339 322 347 330 -width 1 -outline black -fill green -tags b0
.c bind b0 <Button-1> {pushButton 0 1}
.c bind b0 <ButtonRelease-1> {pushButton 0 0}
.c create rectangle 338 418 346 426 -width 1 -outline black -fill green -tags b1
.c bind b1 <Button-1> {pushButton 1 1}
.c bind b1 <ButtonRelease-1> {pushButton 1 0}
.c create rectangle 338 525 346 533 -width 1 -outline black -fill green -tags b2
.c bind b2 <Button-1> {pushButton 2 1}
.c bind b2 <ButtonRelease-1> {pushButton 2 0}
.c create rectangle 341 600 349 608 -width 1 -outline black -fill green -tags b3
.c bind b3 <Button-1> {pushButton 3 1}
.c bind b3 <ButtonRelease-1> {pushButton 3 0}

.c create rectangle 329 322 337 330 -width 1 -outline black -fill blue -tags b4
.c bind b4 <Button-1> {toggle b4 4 1}
.c bind b4 <ButtonRelease-1> {toggle b4 4 0}
.c create rectangle 328 418 336 426 -width 1 -outline black -fill blue -tags b5
.c bind b5 <Button-1> {toggle b5 5 1}
.c bind b5 <ButtonRelease-1> {toggle b5 5 0}
.c create rectangle 328 525 336 533 -width 1 -outline black -fill blue -tags b6
.c bind b6 <Button-1> {toggle b6 6 1}
.c bind b6 <ButtonRelease-1> {toggle b6 6 0}
.c create rectangle 331 600 339 608 -width 1 -outline black -fill blue -tags b7
.c bind b7 <Button-1> {toggle b7 7 1}
.c bind b7 <ButtonRelease-1> {toggle b7 7 0}

array set toggleButtonState {b4 0 b5 0 b6 0 b7 0}

# toggle and show the toggle button's state; pass all changes to
# the application
proc toggle {key button state} {
    if {$state} {
        upvar toggleButtonState($key) tb
        set tb [expr {$tb ? 0 : 1} ]
        if {$tb} {
           .c itemconfigure $key -fill lightblue
        } else {
           .c itemconfigure $key -fill blue
        }
    }
    pushButton $button $state
}

# all lamps off to start with
array set lampState {a 0 b 0 c 0 d 0}

# trace assignments to lampState; key should be a, b, c or d
proc traceLampState {varName key op} {
    global lampState
    if {$varName != "lampState"} {return}
    if {$lampState($key)} {
        .c itemconfigure $key -fill yellow
    } else {
        .c itemconfigure $key -fill gray
    }
}

# start the trace
trace variable lampState w traceLampState

# for emacs:
# Local Variables:
# tcl-default-application: "stairwell_demo"
# End:
