#!/usr/bin/env bash

set -m

ID="${1}"
STATE="${2}"
WAIT_TIME="${3}"

cd "/tmp/SmartTrie/"
while true; do
    echo "Start replica with ID: ${ID} STATE: ${STATE}"
    java -cp dist/SmartTrie.jar -XX:+UnlockExperimentalVMOptions -XX:InitialHeapSize=2G -XX:MaxHeapSize=16G -Xlog:gc=debug:file=logs/gc.log:time smarttrie.app.server.Server "${ID}" "${STATE}"

    echo "Sleep to avoid bind address errors"
    sleep "${WAIT_TIME}"
done