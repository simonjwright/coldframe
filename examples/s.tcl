# Copyright (C) Simon Wright <simon@pushface.org>

# This unit is free software; you can redistribute it and/or modify it
# as you wish. This unit is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This is a demonstration of Tcl-based scripted testing, to be run by
# simple_buttons-scripting(.exe).

# NB: each test must leave the LED clear!

# Check the settings of the output signal. This is probably more
# verbose than you'd need.
proc check-output {state} {
    # echo "checking output 0 is in state $state"
    check-boolean-for-digital_io.output_signal \
        digital_io.set \
        o 0 \
        to_state $state
}

puts "script starting"

# setup required
set-boolean digital_io.get return false

set SHORT_PUSH_LIMIT 0.25
set LIT_PERIOD 5.0

#setup done
initialize
start_dispatcher
wait_until_idle

echo "starting: checking that all LEDs are off"
check-output false
save_number_of_calls digital_io.set

proc short_push {} {
    echo "\n\n*** SHORT PUSH ***\n"

    global SHORT_PUSH_LIMIT LIT_PERIOD

    echo "pushing button 0"
    mark short_push
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "LED should be set"
    check_number_of_new_calls digital_io.set 1
    check-output true

    echo "waiting until just before the pushed timeout"
    wait_from_mark short_push [expr $SHORT_PUSH_LIMIT - 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just before the lit timeout"
    wait_from_mark short_push [expr $LIT_PERIOD - 0.005]

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the lit timeout"
    wait_from_mark short_push [expr $LIT_PERIOD + 0.005]

    echo "checking the LED is now clear"
    check_number_of_new_calls digital_io.set 2
    check-output false
}

proc repeated_short_push {} {
    echo "\n\n*** REPEATED SHORT PUSH ***\n"

    global SHORT_PUSH_LIMIT LIT_PERIOD

    save_number_of_calls digital_io.set
    mark repeated_short_push

    echo "pushing button 0"
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "LED should be set"
    check_number_of_new_calls digital_io.set 1
    check-output true

    echo "waiting until just before the pushed timeout"
    wait_from_mark repeated_short_push [expr $SHORT_PUSH_LIMIT - 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the pushed timeout"
    wait_from_mark repeated_short_push [expr $SHORT_PUSH_LIMIT + 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "pushing button 0"
    mark second_repeated_short_push
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "checking the LED is still set"
    # note, there's another Changed() call on re-entry to the Pushed state
    check_number_of_new_calls digital_io.set 2
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 2
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just before the lit timeout"
    wait_from_mark second_repeated_short_push [expr $LIT_PERIOD - 0.005]

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 2
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the lit timeout"
    wait_from_mark second_repeated_short_push [expr $LIT_PERIOD + 0.005]

    echo "checking the LED is now clear"
    check_number_of_new_calls digital_io.set 3
    check-output false
}

proc long_push {} {
    echo "\n\n*** LONG PUSH ***\n"

    global SHORT_PUSH_LIMIT LIT_PERIOD

    save_number_of_calls digital_io.set
    mark long_push

    echo "pushing button 0"
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "LED should be set"
    check_number_of_new_calls digital_io.set 1
    check-output true

    echo "waiting until just before the pushed timeout"
    wait_from_mark long_push [expr $SHORT_PUSH_LIMIT - 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the pushed timeout"
    wait_from_mark long_push [expr $SHORT_PUSH_LIMIT + 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until after the lit timeout"
    wait_from_mark long_push [expr $LIT_PERIOD + 1.0]

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    check-output true

    echo "pushing button 0"
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "checking the LED is clear"
    check_number_of_new_calls digital_io.set 2
    check-output false

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still clear"
    check_number_of_new_calls digital_io.set 2
    check-output false
}

proc short_then_long_push {} {
    echo "\n\n*** SHORT THEN LONG PUSH ***\n"

    global SHORT_PUSH_LIMIT LIT_PERIOD

    save_number_of_calls digital_io.set
    mark short_then_long_push

    echo "pushing button 0"
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "LED should be set"
    check_number_of_new_calls digital_io.set 1
    check-output true

    echo "waiting until just before the pushed timeout"
    wait_from_mark short_then_long_push [expr $SHORT_PUSH_LIMIT - 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the pushed timeout"
    wait_from_mark short_then_long_push [expr $SHORT_PUSH_LIMIT + 0.005]
    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 1
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "pushing button 0"
    mark second_short_then_long_push
    callback-digital_io.input_signal_state {0 true}
    wait_until_idle

    echo "checking the LED is still set"
    # note, there's another Changed() call on re-entry to the Pushed state
    check_number_of_new_calls digital_io.set 2
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the pushed timeout"
    wait_from_mark second_short_then_long_push [expr $SHORT_PUSH_LIMIT + 0.005]

    echo "releasing button 0"
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 2
    # this is tautological if there've been no calls, but ..
    check-output true

    echo "waiting until just after the lit timeout"
    wait_from_mark second_short_then_long_push [expr $LIT_PERIOD + 1.0]

    echo "checking the LED is still set"
    check_number_of_new_calls digital_io.set 2
    check-output true

    # there may be another test in due course
    echo "resetting"
    callback-digital_io.input_signal_state {0 true}
    callback-digital_io.input_signal_state {0 false}
    wait_until_idle

    echo "checking the LED is now clear"
    check_number_of_new_calls digital_io.set 3
    check-output false
}

short_push
repeated_short_push
long_push
short_then_long_push

echo "done."

go
