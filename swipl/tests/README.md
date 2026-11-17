# Plain runtime tests

Unit tests cover JSON/schema conversion, the SSE parser, OpenAI/Anthropic/Google
event parsing and request shapes:

```bash
swipl -q swipl/tests/unit_tests.pl
# or both suites:
swipl/tests/run_all.sh
```

The integration test starts a local OpenAI-compatible SSE server and runs a real
`task/2` through the REPL, including `set_result`/`finish` and the final answer:

```bash
swipl/tests/integration_openai.sh
```

`integration_openai.sh` needs Python 3. It does not use a real API key.

Manual provider checks can use any OpenAI-compatible server, for example:

```bash
OPENAI_COMPATIBLE_API_KEY=test swipl/bin/dc \
  --provider openai_compatible --model local-model
```
