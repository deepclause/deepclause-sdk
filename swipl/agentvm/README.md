# Running the plain runtime inside AgentVM

The plain runtime is designed to run inside AgentVM's Alpine guest, with bash
and the Prolog engine in the same sandbox.

## Image requirements

- Alpine `swi-prolog` (the `edge/testing/riscv64` package is 10.0.2 at the time
  of writing) and `ca-certificates`.
- The runtime under `/opt/deepclause`, with `src/prolog-src` available at
  `/opt/src/prolog-src` (the MI module is loaded relatively).
- QLF compiled **inside the image**; QLF is architecture/version-specific.

Build from the repository root:

```bash
docker build -f swipl/agentvm/Dockerfile -t deepclause-swi .
```

For the AgentVM image pipeline, run the same commands in the Alpine build stage
and copy `/opt/deepclause` into the guest image.

## Interactive console

Start AgentVM with `interactive: true`, then run:

```text
/opt/deepclause/bin/dc
```

Pipe input with `vm.writeToStdin` and route `onStdout`/`onStderr`. The launcher
is the only Node/JS component; the DeepClause runtime is Prolog.

## Network and keys

- Enable AgentVM networking only when a provider is configured.
- Install `ca-certificates`; verify `https://api.openai.com/` or the chosen
  provider before blaming the runtime.
- Use AgentVM's firewall to allow only the provider domains you configured.
- Pass credentials through the environment or a 0600 config file in
  `/workspace`; do not bake them into the image.

## Compiling a C/C++ host inside AgentVM

Alpine packages `swi-prolog`, but not `swi-prolog-dev`. A C/C++ embedding host
therefore needs the matching SWI headers vendored or SWI built from source, plus
a riscv64 cross toolchain. This is intentionally not on the default path; see
`docs/SWIPL_RUNTIME.md` §9.
