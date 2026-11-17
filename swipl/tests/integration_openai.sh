#!/bin/sh
# End-to-end plain-runtime test against a local OpenAI-compatible SSE server.
set -eu
DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$DIR/.." && pwd)
PORT=8769
WS=$(mktemp -d)
PID=

cleanup() {
  if [ -n "$PID" ]; then kill "$PID" 2>/dev/null || true; fi
  rm -rf "$WS"
}
trap cleanup EXIT INT TERM

mkdir -p "$WS/.deepclause"
cat > "$WS/.deepclause/config.json" <<JSON
{"providers":{"openai_compatible":{"base_url":"http://127.0.0.1:$PORT/v1"}}}
JSON
cat > "$WS/task.dml" <<'DML'
agent_main :- task("Say hello", string(Out)), answer(Out).
DML

python3 "$DIR/mock_openai.py" "$PORT" &
PID=$!
sleep 0.5

OUT=$(OPENAI_COMPATIBLE_API_KEY=test DC_WORKSPACE="$WS" timeout 30 sh -c "
  printf '/provider openai_compatible\n/model mock\n/run $WS/task.dml\n/quit\n' |
  '$ROOT/bin/dc'
" 2>&1) || {
  echo "$OUT"
  echo "integration test: runtime exited non-zero" >&2
  exit 1
}

echo "$OUT"
printf '%s\n' "$OUT" | grep -q "hello from mock" || {
  echo "integration test: missing model/tool result" >&2
  exit 1
}
echo "integration test passed"
