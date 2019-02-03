# Simple Buttons for BBC micro:bit #

`Simple_Buttons` is a demo of ColdFrame aimed at MCUs. It's intended
only as an example of using ColdFrame's state machines in a restricted
(Ravenscar) environment, which requires no or minimal additional
breadboarding (i.e., uses on-board resources as far as possible).

A Button controls an LED. A short push on the button leaves the LED
lit for 5 seconds. Another short push while the LED is lit restarts
the 'lit' period.

A long push leaves the LED lit indefinitely, until the button is
pushed again.

For this MCU, the Button is button A, and the LED is the one at row 3,
column 2. The LED at row 3, column 4 provides a heartbeat (rapid
flashing for the first second, thereafter a flash every second).

Note, because of the limited space on this MCU, this demo needs to be
built with optimisation (`BUILD=Production`, resulting in nearly
halving the size). It's OK to have the runtime and the ColdFrame
library built for debug (`BUILD=Debug`, the default).
