import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import * as fs from 'fs/promises'
import * as os from 'os'
import * as path from 'path'
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js'
import type { ToolDefinition } from '../src/types.js'

let workspace: string
let repl: HarnessRepl | undefined

const PYTHON: ToolDefinition = {
  description: 'python stub',
  parameters: { type: 'object', properties: {}, required: [] },
  execute: async () => ({ exitCode: 0, stdout: '{"ok":true}', stderr: '' }),
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-collision-'))
  repl = await createHarnessRepl({
    workspacePath: workspace,
    model: 'test-model',
    provider: 'openai',
    tools: new Map([['python', PYTHON]]),
  })
})

afterEach(async () => {
  await repl?.dispose()
  repl = undefined
  await fs.rm(workspace, { recursive: true, force: true })
})

const withName = (name: string): string => `
script_file(${name}, "scripts/run_tests.py").
run_tests(Dir, Summary) :- run_script(${name}, [Dir], R), Summary = R.
test_dir(Dir, Summary) :- run_tests(Dir, Summary).
`

describe('script link names and predicate names', () => {
  it('rejects the same name with an actionable collision error', async () => {
    const result = await repl!.session.loadCode(withName('run_tests'))
    expect(result.error).toMatch(/script_predicate_collision/)
    expect(result.error).toMatch(/run_tests_py|rename/i)
  })

  it('accepts a distinct script link name', async () => {
    const result = await repl!.session.loadCode(withName('run_tests_py'))
    expect(result.error).toBeUndefined()
  })

  it('rejects a link added after a predicate with the same name', async () => {
    await repl!.session.loadCode('run_tests(_, _).')
    const result = await repl!.session.loadCode('script_file(run_tests, "scripts/run_tests.py").')
    expect(result.error).toMatch(/script_predicate_collision/)
  })
})
