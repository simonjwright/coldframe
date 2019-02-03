# Examples #

There are several examples of ColdFrame usage here; the `.gpr` files enable build. Note, not all the examples necessarily work at any given release: `make` should build them all (see the macro `EXECUTABLES` in the Makefile).

As released, the build expects to find the following additional packages:

>**[tcladashell](https://sourceforge.net/projects/tcladashell/)** installed at `~/tcladashell` <br/>
>**[scripted_testing](https://sourceforge.net/projects/scriptedtesting.coldframe.p/)** at `~/scripted_testing`

## The examples ##

### `House_Management_Scripting` ###

This demonstrates advanced use of stub generation and scripted testing.

`House_Management` is part of the Stairwell demonstration (see below).

Run the test with

    ./house_management-scripting h.tcl

### `Interrupt_Handling` ###

This demonstrates desktop use of `ColdFrame.Interrupts` to communicate
with a task via a Signal.

Run the demo with

    ./interrupt_handling-harness

and try Ctrl-C.

### `Library` ###

Shows AUnit use (with a child package of the parent `Library`).

Run the test with

    ./library_test_harness

### `Performance` ###

Measures various aspects of ColdFrame performance.

Run with

    ./performance-harness

### `Problem_Reporting` ###

This demo is about super/subtypes and subtype migration.

Run with

    ./problem_reporting_harness

### `Simple_Buttons` ###

This demo is about a simple state machine (in a Button) which decides
for how long to light an LED. The main motivation is that the other
candidate, `House_Management`, is quite impractical for demonstrating
the use of ColdFrame in a Ravenscar-limited environment with typical
MCUs; no one is going to want to breadboard an application with 8
buttons and 4 LEDs! See the MCU subdirectories `arduino/` (Arduino
Due), `microbit/` (the BBC micro:bit) and `stm32f4/` (the STM32F407
Disco board).

A short press on the Button lights the LED for 5 seconds. A further
short press restarts the timer.

A long press on the Button lights the LED until a further press.

Here, there's a test using `scripted_testing` to check the Button's
state machine. Run the test with

    ./simple_buttons-scripting s.tcl

### `stairwell_demo` ###

This is actually the executable which provides an embedded Tcl/Tk
interpreter, and runs a graphical interface showing a cross-section of
a 19th-century house in Lansing, Michigan, with lamps on each landing
(and the basement) and two switches each: the blue one is a toggle,
the green one is timed (5 seconds). Each button controls multiple
lamps, so that if - for instance - you click a top-floor button, the
lamps on that floor and the one below light to help you make your way
down the stairs.

The application domain here is `House_Management` (see above).

Run with

    ./stairwelllights.tcl

### `States` ###

This demonstrates a complex state machine for monitoring a Device
which is supposed to report a regular Heartbeat: is it Working,
Recovering, or Failed?

Run with

    ./states-t

### `Van_Fleet` ###

Another demonstration of super/subtypes and subtype migration.

Run with

    ./van_fleet-demo
