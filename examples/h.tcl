# Copyright (C) Simon Wright <simon@pushface.org>

# This unit is free software; you can redistribute it and/or modify it
# as you wish. This unit is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This is a demonstration of Tcl-based scripted testing, to be run by
# house_management-scripting(.exe).

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

puts "script starting"

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

echo "waiting until just before the timeout"
wait_from_mark a 4.995
echo "checking the lamps are still set"
check_number_of_new_calls digital_io.set 0
# this is tautological if there've been no calls, but ..
check-outputs true true false false
save_number_of_calls digital_io.set

echo "waiting until just after the timeout"
wait_from_mark a 5.005

echo "checking the lamps are now clear"
check_number_of_new_calls digital_io.set 2
check-outputs false false false false

echo "done."

go
