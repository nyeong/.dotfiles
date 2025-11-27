#!/usr/bin/env bash
# Attach to remote zellij session or create new one if it doesn't exist
# Usage: attach-remote.sh <hostname> <session-name>

set -e

HOST="$1"
SESSION_NAME="${2:-default}"

if [ -z "$HOST" ]; then
  echo "Usage: $0 <hostname> [session-name]"
  exit 1
fi

# SSH to remote and attach to zellij session
ssh -t "$HOST" "zellij attach -c '$SESSION_NAME' || zellij -s '$SESSION_NAME'"
