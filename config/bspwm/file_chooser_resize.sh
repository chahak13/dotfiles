#!/bin/sh
wid=$1
class=$2
case "$class" in
        *)
                case "$(xprop -id "$wid" | awk '/WM_WINDOW_ROLE/ {print $3}')" in
                        '"GtkFileChooserDialog"' )
                                echo rectangle=800x600+0+0
                                ;;
                esac
                ;;
esac
