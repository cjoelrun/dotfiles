#!/bin/bash

# Focus a space and move or open an app
focus_space_with_app() {
    local space=$1
    local app_name=$2
    local app_path=$3

    # Focus the space
    yabai -m space --focus "$space" 2>/dev/null || return 1

    # Get all windows as JSON array first, then process
    local windows_json=$(yabai -m query --windows 2>/dev/null)
    
    # Check if we got valid JSON
    if [[ -z "$windows_json" ]] || ! echo "$windows_json" | jq empty 2>/dev/null; then
        # No valid JSON, just open the app
        open -a "$app_path" 2>/dev/null
        return 0
    fi
    
    # Check if app has any windows
    local window_info=$(echo "$windows_json" | jq --arg app "$app_name" '[.[] | select(.app==$app)] | first' 2>/dev/null)
    
    if [[ -n "$window_info" ]] && [[ "$window_info" != "null" ]]; then
        local window_id=$(echo "$window_info" | jq -r '.id // empty' 2>/dev/null)
        local window_space=$(echo "$window_info" | jq -r '.space // empty' 2>/dev/null)
        local is_minimized=$(echo "$window_info" | jq -r '."is-minimized" // false' 2>/dev/null)
        local is_visible=$(echo "$window_info" | jq -r '."is-visible" // true' 2>/dev/null)
        local is_hidden=$(echo "$window_info" | jq -r '."is-hidden" // false' 2>/dev/null)
        
        # If window exists but is minimized/hidden or not visible, open the app to bring it forward
        if [[ "$is_minimized" == "true" ]] || [[ "$is_visible" == "false" ]] || [[ "$is_hidden" == "true" ]]; then
            open -a "$app_path" 2>/dev/null
        elif [[ -n "$window_id" ]] && [[ -n "$window_space" ]] && [[ "$window_space" != "$space" ]]; then
            # Window is visible but on different space, move it
            yabai -m window "$window_id" --space "$space" 2>/dev/null
        fi
    else
        # No windows or app not running, open it
        open -a "$app_path" 2>/dev/null
    fi
}

# Focus a space and move or open multiple apps
focus_space_with_apps() {
    local space=$1
    shift

    # Focus the space
    yabai -m space --focus "$space" 2>/dev/null || return 1

    # Get all windows as JSON array first, then process
    local windows_json=$(yabai -m query --windows 2>/dev/null)
    
    # Check if we got valid JSON
    if [[ -z "$windows_json" ]] || ! echo "$windows_json" | jq empty 2>/dev/null; then
        # No valid JSON, just open the apps
        for app_spec in "$@"; do
            local app_path="${app_spec#*:}"
            open -a "$app_path" 2>/dev/null
        done
        return 0
    fi

    for app_spec in "$@"; do
        local app_name="${app_spec%%:*}"
        local app_path="${app_spec#*:}"

        # Check if app has any windows
        local window_info=$(echo "$windows_json" | jq --arg app "$app_name" '[.[] | select(.app==$app)] | first' 2>/dev/null)
        
        if [[ -n "$window_info" ]] && [[ "$window_info" != "null" ]]; then
            local window_id=$(echo "$window_info" | jq -r '.id // empty' 2>/dev/null)
            local window_space=$(echo "$window_info" | jq -r '.space // empty' 2>/dev/null)
            local is_minimized=$(echo "$window_info" | jq -r '."is-minimized" // false' 2>/dev/null)
            local is_visible=$(echo "$window_info" | jq -r '."is-visible" // true' 2>/dev/null)
            local is_hidden=$(echo "$window_info" | jq -r '."is-hidden" // false' 2>/dev/null)
            
            # If window exists but is minimized/hidden or not visible, open the app to bring it forward
            if [[ "$is_minimized" == "true" ]] || [[ "$is_visible" == "false" ]] || [[ "$is_hidden" == "true" ]]; then
                open -a "$app_path" 2>/dev/null
            elif [[ -n "$window_id" ]] && [[ -n "$window_space" ]] && [[ "$window_space" != "$space" ]]; then
                # Window is visible but on different space, move it
                yabai -m window "$window_id" --space "$space" 2>/dev/null
            fi
        else
            # No windows or app not running, open it
            open -a "$app_path" 2>/dev/null
        fi
    done
}