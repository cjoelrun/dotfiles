#!/bin/bash

# Focus a space and move or open an app
focus_space_with_app() {
    local space=$1
    local app_name=$2
    local app_path=$3

    # Focus the space
    yabai -m space --focus "$space"

    # Check if app has any windows
    local window_id=$(yabai -m query --windows | jq --arg app "$app_name" '.[] | select(.app==$app) | .id' | head -1)
    
    if [[ -n "$window_id" ]]; then
        # App has windows, move them to the space
        yabai -m window "$window_id" --space "$space" 2>/dev/null
    else
        # No windows or app not running, open it
        open -a "$app_path"
    fi
}

# Focus a space and move or open multiple apps
focus_space_with_apps() {
    local space=$1
    shift

    # Focus the space
    yabai -m space --focus "$space"

    for app_spec in "$@"; do
        local app_name="${app_spec%%:*}"
        local app_path="${app_spec#*:}"

        # Check if app has any windows
        local window_id=$(yabai -m query --windows | jq --arg app "$app_name" '.[] | select(.app==$app) | .id' | head -1)
        
        if [[ -n "$window_id" ]]; then
            # App has windows, move them to the space
            yabai -m window "$window_id" --space "$space" 2>/dev/null
        else
            # No windows or app not running, open it
            open -a "$app_path"
        fi
    done
}
