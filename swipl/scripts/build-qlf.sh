#!/bin/sh
# Compile the plain runtime to QLF. Run this in the target image so the QLF
# matches the installed SWI version and architecture.
set -e
DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$DIR/.." && pwd)
cd "$ROOT/prolog"
for f in dc_json.pl dc_config.pl dc_events.pl dc_sse.pl dc_llm.pl \
         dc_tools.pl dc_agent.pl dc_frame.pl dc_session.pl dc_repl.pl \
         providers/dc_llm_openai.pl providers/dc_llm_anthropic.pl providers/dc_llm_google.pl; do
  swipl -q -g "qcompile('$f'),halt"
done
echo "QLF written under $ROOT/prolog"
