leds
====

This project provides basic Erlang support for controlling LEDs through the Linux /sys/class interface. LEDs
are referenced by the name that they appear as in the /sys/class/leds directory. Here's an example of how
to use the interface:

    MyLed = 'beaglebone:green:usr0'.
    led:open(MyLed).
    led:disable_triggers(MyLed).

    % Turn the LED on
    led:set_brightness(MyLed, 1).

    % Turn the LED off
    led:set_brightness(MyLed, 0).

    % Blink the LED (500 ms on, 500 ms off)
    led:blink(MyLed, 500, 500).

    % When done.
    led:close(MyLed).

If the LEDs on the system are not known ahead of time, led:list/0 can be used to return the available LEDs.
