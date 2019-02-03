# Simple Buttons for Arduino Due #

`Simple_Buttons` is a demo of ColdFrame aimed at MCUs. It's intended
only as an example of using ColdFrame's state machines in a restricted
(Ravenscar) environment, which requires no or minimal additional
breadboarding (i.e., uses on-board resources as far as possible).

A Button controls an LED. A short push on the button leaves the LED
lit for 5 seconds. Another short push while the LED is lit restarts
the 'lit' period.

A long push leaves the LED lit indefinitely, until the button is
pushed again.

For this MCU, the LED is the amber user LED. There's no heartbeat
provided (there are no other LEDs).

I had hoped to subvert the ERASE button to act as the button (on PC0)
for this demo. Unfortunately, although I was able to stop the button
erasing the FLASH, I wasn't able to make it actually act as an input,
so fell back to PC1; an external pushbutton is needed, between GND and
digital pin 33.
