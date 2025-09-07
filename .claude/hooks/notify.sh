#!/bin/bash

# Claude notification hook for macOS
# Sends system notifications when Claude needs attention

# Read JSON input from stdin
INPUT=$(cat)

# Extract hook event and message
HOOK_EVENT=$(echo "$INPUT" | jq -r '.hook_event_name // empty')
MESSAGE=$(echo "$INPUT" | jq -r '.message // empty')

# For Stop events, notify that Claude is done
if [[ "$HOOK_EVENT" == "Stop" ]]; then
    osascript -e 'display notification "Task completed" with title "Claude Code" sound name "Glass"'
    exit 0
fi

# For Notification events, check the message content
if [[ "$HOOK_EVENT" == "Notification" ]]; then
    if [[ "$MESSAGE" == *"needs your permission"* ]]; then
        osascript -e 'display notification "Permission needed for tool use" with title "Claude Code" sound name "Ping"'
    elif [[ "$MESSAGE" == *"waiting for your input"* ]]; then
        osascript -e 'display notification "Waiting for your input" with title "Claude Code" sound name "Pop"'
    else
        osascript -e "display notification \"$MESSAGE\" with title \"Claude Code\""
    fi
fi

exit 0