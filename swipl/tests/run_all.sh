#!/bin/sh
set -eu
DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$DIR/.." && pwd)
swipl -q "$DIR/unit_tests.pl"
"$DIR/integration_openai.sh"
