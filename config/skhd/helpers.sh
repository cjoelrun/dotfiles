#!/bin/bash

# Focus a space and move or open an app
focus_space_with_app() {
    local space=$1
    local app_name=$2
    local app_path=$3

    yabai -m space --focus "$space"

    if pgrep -x "$app_name" > /dev/null; then
        # App is running, move it to the space
        yabai -m window $(yabai -m query --windows | jq --arg app "$app_name" '.[] | select(.app==$app) | .id') --space "$space" 2>/dev/null
    else
        # App is not running, open it
        open -a "$app_path"
    fi
}

# Focus a space and move or open multiple apps
focus_space_with_apps() {
    local space=$1
    shift

    yabai -m space --focus "$space"

    for app_spec in "$@"; do
        local app_name="${app_spec%%:*}"
        local app_path="${app_spec#*:}"

        if pgrep -x "$app_name" > /dev/null; then
            yabai -m window $(yabai -m query --windows | jq --arg app "$app_name" '.[] | select(.app==$app) | .id') --space "$space" 2>/dev/null
        else
            open -a "$app_path"
        fi
    done
}
