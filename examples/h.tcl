# Copyright (C) Simon Wright <simon@pushface.org>

# This unit is free software; you can redistribute it and/or modify it
# as you wish. This unit is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This is a demonstration of Tcl-based scripted testing, to be run by
# house_management-scripting(.exe).

# Check the settings of all 4 output signals.
proc check-outputs {s0 s1 s2 s3} {
    check-boolean-for-digital_io.output_signal digital_io.set o 0 to_state $s0
    check-boolean-for-digital_io.output_signal digital_io.set o 1 to_state $s1
    check-boolean-for-digital_io.output_signal digital_io.set o 2 to_state $s2
    check-boolean-for-digital_io.output_signal digital_io.set o 3 to_state $s3
}

puts "script starting."

# no setup required
start_dispatcher
wait_until_idle

echo "checking that all lamps are off"
check-outputs false false false false
save_number_of_calls digital_io.set

echo "pushing button 0"
mark a
digital_io.input_signal_state_callback 0 true
wait_until_idle

echo "lamps 0 and 1 should be set"
check_number_of_new_calls digital_io.set 2
check-outputs true true false false

echo "waiting until just before the timeout"
wait_from_mark a 4.990
echo "checking the lamps are still set"
check_number_of_new_calls digital_io.set 2
# this is tautological if there've been no calls, but ..
check-outputs true true false false

echo "waiting until just after the timeout"
wait_from_mark a 5.010

echo "checking the lamps are now clear"
check_number_of_new_calls digital_io.set 4
check-outputs false false false false

echo "done."

go
