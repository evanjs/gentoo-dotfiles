#!/usr/bin/env sh

# Terminate already running bar instances
#killall -q slack 

# Wait until the processes have been shut down
#while pgrep -x slack >/dev/null; do sleep 1; done

# Launch onedrived
# TODO: run `onedrived stop` and wait until dead, then run `onedrived start`?
if pgrep -x onedrived > /dev/null; then
    kill onedrived
    # Don't take any action if onedrived is running
else
    # restart, just to be sure
    onedrived start
fi
