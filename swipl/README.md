# Plain SWI-Prolog DeepClause runtime

Status: **working core**. The plain runtime runs on stock SWI-Prolog, with no
Node, npm, or WASM in the execution path.

See [`../docs/SWIPL_RUNTIME.md`](../docs/SWIPL_RUNTIME.md) for the design,
provider layer, AgentVM deployment, and the optional C/C++ host analysis.

## Run

```bash
swipl/scripts/build-qlf.sh          # optional, but recommended
swipl/bin/dc                        # starts the REPL
swipl/bin/dc --provider anthropic --model claude-3-5-haiku-latest
swipl/bin/dc --workspace ./my-project
```

Or open the REPL with a skill loaded:

```bash
swipl/bin/dc /path/to/skill.dml
```

The wrapper uses `prolog/dc_repl.qlf` when present, otherwise
`prolog/dc_repl.pl`.

## REPL commands

```text
/help                     this help
/load <file.dml>          load a DML harness
/goal <dml>               run a goal
/run <file> [args...]     load and run agent_main/N
/listing [filter]         inspect the harness
/save <file.dml>          export the harness
/reset                    empty the harness
/provider [name]          show/set the LLM provider
/model [id]               show/set the model
/config                   show effective config JSON
/quit                     exit
```

Plain text ending in `.` is treated as a DML goal. Natural-language authoring
is not part of the first plain runtime; use `/goal`, `/load`, and `/run`.

## Configuration

The runtime reads, in order:

1. CLI flags (`--provider`, `--model`, `--workspace`, `--config`)
2. environment variables (`DC_PROVIDER`, `DC_MODEL`, provider API-key env vars)
3. `<workspace>/.deepclause/config.json`
4. `~/.config/deepclause/config.json`
5. built-in defaults

Example:

```json
{
  "llm": {
    "provider": "openai",
    "model": "gpt-4o-mini",
    "temperature": 0.2,
    "max_tokens": 4096
  },
  "providers": {
    "openai":       { "api_key_env": "OPENAI_API_KEY" },
    "anthropic":    { "api_key_env": "ANTHROPIC_API_KEY" },
    "google":       { "api_key_env": "GEMINI_API_KEY" },
    "openrouter":   { "api_key_env": "OPENROUTER_API_KEY" },
    "openai_compatible": { "base_url": "http://127.0.0.1:11434/v1" },
    "azure": {
      "base_url": "https://YOUR-RESOURCE.openai.azure.com/openai/deployments/YOUR-DEPLOYMENT",
      "api_version": "2024-10-21",
      "api_key_env": "AZURE_OPENAI_API_KEY"
    }
  }
}
```

For Azure, the adapter appends `/chat/completions?api-version=...` unless the
configured base URL already contains `/chat/completions`.

`providers.<name>.api_key` is also accepted, but environment variables are
preferred. Local OpenAI-compatible servers do not require a key.

## Providers

| Provider | Adapter |
|---|---|
| OpenAI | OpenAI Chat Completions (`openai_chat`) |
| Anthropic | Messages API |
| Google Gemini | `generateContent` streaming |
| OpenRouter, Groq, Mistral, DeepSeek, xAI, Together, Fireworks, Cerebras, NVIDIA, Azure OpenAI | OpenAI-compatible Chat Completions |
| Ollama, LM Studio, vLLM | OpenAI-compatible Chat Completions |

OpenAI Responses, Bedrock, and Vertex are not in the first core. The provider
seam in `dc_llm.pl` is designed so they can be added without changing DML or the
agent loop.

## Runtime layout

```text
swipl/
├── bin/dc                    # entry point
├── prolog/
│   ├── dc_repl.pl            # REPL and commands
│   ├── dc_frame.pl           # frame driver
│   ├── dc_session.pl         # sessions, load/save
│   ├── dc_agent.pl           # task/prompt agent loop
│   ├── dc_tools.pl           # bash/exec, input
│   ├── dc_llm.pl             # normalized LLM transport
│   ├── dc_sse.pl             # SSE parser
│   ├── dc_config.pl          # config and provider registry
│   ├── dc_json.pl            # JSON/schema helpers
│   ├── dc_events.pl          # UI event bus
│   └── providers/
├── scripts/build-qlf.sh
├── tests/
└── agentvm/                  # Alpine/AgentVM image files
```

## Tests

```bash
# Unit tests (JSON, SSE, provider event parsing, request shapes)
swipl -q swipl/tests/unit_tests.pl

# End-to-end with a local OpenAI-compatible SSE server
swipl/tests/integration_openai.sh
```

The integration test starts a local mock server, runs a `task/2` that asks the
model to set a typed result, and verifies the final answer. It needs Python 3.

## AgentVM

Build and run the Alpine image locally:

```bash
docker build -f swipl/agentvm/Dockerfile -t deepclause-swi .
docker run --rm -it deepclause-swi
```

See [`agentvm/README.md`](./agentvm/README.md). The guest needs Alpine
`swi-prolog` (edge/testing) and `ca-certificates`; compile QLF inside the image.
Alpine does not package SWI development headers, so a C/C++ embedding host needs
vendored headers or a source build.
