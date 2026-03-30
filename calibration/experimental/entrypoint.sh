#!/bin/bash
set -e

CONFIG_PATH="/config.json"

if [ -n "$S3_CONFIG" ]; then
  echo "Downloading config from $S3_CONFIG"
  aws s3 cp "$S3_CONFIG" "$CONFIG_PATH"
elif [ ! -f "$CONFIG_PATH" ]; then
  echo "ERROR: No config found. Set S3_CONFIG or mount a file to /config.json"
  exit 1
fi

exec Rscript calibration/run-calibration-remote.R
