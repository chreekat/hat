#!/usr/bin/env bash

set -Eeuo pipefail

num=${1:-4000}

for i in $(seq 1 $num); do echo "."; done
