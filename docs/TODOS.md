- run/compile path logic (.deepclause always)
- Judgment layer:
    - the judge predicates use unprefixed generic names (`verify/3`, `choose/4`,
      `rate/4`, `probability/3`, `holds/2,3`, ...) and shadow user predicates of
      the same name/arity; see `docs/JUDGE_NAME_COLLISIONS.md`.
- More examples and testing
- config system
- Module system?
    - consult works
    - std lib?

- TUI / EyeCandy
- MCP support, general tool settings
- Meta:
    - References to MCP so that config can be updated
- Check if all examples in documentation work and everything is correct

- dml skills
    - add files and folders that are loaded automatically into thwe VM
        compile --> add assets --> contains e.g. python code etc.
            (needs to be part of the prompt)
                .deepclause/assets

- compilation:
    can use a coding agent to produce sidecar code (assets?)
    sidecar = bunch of python/js scripts etc.
    gets mounted into the vm automatically

- allow vm == local machine option?

- vm 
    - cache compiled python code?
    - install dependencies from outside in?
    - how to make it faster?
    
- orchestrate coding agents or other agents
    - would be great to have them running directly in the VM