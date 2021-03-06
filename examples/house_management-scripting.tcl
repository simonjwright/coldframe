# Copyright (C) Simon Wright <simon@pushface.org>

# This unit is free software; you can redistribute it and/or modify it
# as you wish. This unit is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This is a demonstration of Tcl-based scripted testing, to be run by
# house_management-scripting(.exe).

# Define the window round the timer expiry
set margin_ms 2
set margin_s [expr $margin_ms / 1000.0]

puts "window for checks after delay is $margin_ms ms"

# Check the settings of all 4 output signals. This is probably more
# verbose than you'd need.
proc check-outputs {s0 s1 s2 s3} {
    proc check-output {switch state} {
        echo "checking output $switch is in state $state"
        check-boolean-for-digital_io.output_signal \
            digital_io.set \
            o $switch \
            to_state $state
    }
    check-output 0 $s0
    check-output 1 $s1
    check-output 2 $s2
    check-output 3 $s3
}

puts "script compiling"

echo "script executing"

# setup required
set-boolean digital_io.get return false

#setup done
initialize
start_dispatcher
wait_until_idle

echo "checking that all lamps are off"
check-outputs false false false false
save_number_of_calls digital_io.set

echo "pushing button 0"
mark a
callback-digital_io.input_signal_state {0 true}
wait_until_idle

echo "lamps 0 and 1 should be set"
check_number_of_new_calls digital_io.set 2
check-outputs true true false false
save_number_of_calls digital_io.set

echo "waiting until $margin_ms ms before the timeout to check that the lamps
  are still set"
wait_from_mark a [expr 5.000 - $margin_s]
echo "checking the lamps are still set; then, wait until $margin_ms ms after
   the timeout to check that they're clear"
check_number_of_new_calls digital_io.set 0
check-outputs true true false false
save_number_of_calls digital_io.set

# echo "waiting until $margin_ms ms after the timeout"
wait_from_mark a [expr 5.000 + $margin_s]

echo "checking the lamps are now clear"
check_number_of_new_calls digital_io.set 2
check-outputs false false false false

echo "done."

go
