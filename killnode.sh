#!/usr/bin/env bash

# Kill the Erlang process corresponding to a given node name
port=`epmd -names | awk -v name=musiklistan '$2==name {print $5}'`
if [ -z "$port" ]; then
  echo "ERROR: Node name not found: musiklistan"
  exit 1
else
  pid=`lsof -i TCP:$port -s TCP:LISTEN | tail -n +2 | awk '{print $2}'`
  kill $pid
  exit 0
fi
