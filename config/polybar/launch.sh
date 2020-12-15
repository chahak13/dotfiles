#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar on main screen
polybar laptop -c ~/.config/polybar/default_config &

external_monitor=$(xrandr --query | grep 'HDMI-2' | cut -d' ' -f2)
if [[ $external_monitor == "connected" ]]; then
    polybar monitor -c ~/.config/polybar/default_config &
fi
