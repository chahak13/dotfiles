_get_battery () {
    local batc=$(</sys/class/power_supply/BAT1/capacity)
    local bats=$(</sys/class/power_supply/BAT1/status)

    local label="+"
    case "${bats}" in
	Discharging)
	    label="-"
	    if [[ ${batc} -le 90 ]]; then
		notify-send --hint string:bgcolor:#ff0000 "Battery Low" "Please plug in the charger"
	    fi
	    ;;
	Full)
	    label="%"
	    ;;
	*)
	    label="+"
	    ;;
    esac

    echo -e "B: ${batc}${label}"
}

_battery () {
    while true; do
	echo "B" "$(_get_battery)"
	sleep 1s
    done
}

_battery
