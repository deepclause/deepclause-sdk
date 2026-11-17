# Feasibility probes

These are small, dependency-free checks that the plain SWI design is sound.
They are not the runtime; they exist so the design can be verified on a stock
SWI installation.

```bash
# Drive the meta-interpreter frame loop directly (no Node).
swipl -q swipl/experiments/frame_probe.pl

# Prove request_llm can be answered from Prolog (fake provider, no network).
swipl -q swipl/experiments/llm_probe.pl
```

Observed results:

- `frame_probe.pl` prints the three `member/2` solutions followed by `finished`.
- `llm_probe.pl` prints `request_llm`, the fake reply as the `R` binding, and
  `finished`.

The real `dc_llm` layer replaces the fake `post_llm_result/3` with an HTTPS/SSE
provider call.
