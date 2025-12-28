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
    terminal-notifier -title "Claude Code" -message "Task completed" -sound Glass
    exit 0
fi

# For Notification events, check the message content
if [[ "$HOOK_EVENT" == "Notification" ]]; then
    if [[ "$MESSAGE" == *"needs your permission"* ]]; then
        terminal-notifier -title "Claude Code" -message "Permission needed for tool use" -sound Ping
    elif [[ "$MESSAGE" == *"waiting for your input"* ]]; then
        terminal-notifier -title "Claude Code" -message "Waiting for your input" -sound Pop
    else
        terminal-notifier -title "Claude Code" -message "$MESSAGE"
    fi
fi

exit 0