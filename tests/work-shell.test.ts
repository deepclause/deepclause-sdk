/**
 * `shell/1` is the Work-mode presentation contract. The web Work view reads it
 * through `DMLSession.shell()`; a harness without one stays on the classic view.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import * as fs from 'fs/promises'
import * as os from 'os'
import * as path from 'path'

import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js'

let workspace: string
let repl: HarnessRepl | undefined

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-shell-'))
})

afterEach(async () => {
  await repl?.dispose()
  repl = undefined
  await fs.rm(workspace, { recursive: true, force: true })
})

async function load(code: string): Promise<ReturnType<NonNullable<typeof repl>['session']['shell']>> {
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' })
  const result = await repl.session.loadCode(code)
  expect(result.error).toBeUndefined()
  return repl.session.shell()
}

describe('Work shell', () => {
  it('normalizes the agent/workflows/welcome form', async () => {
    const shell = await load(`
      shell(agent([system("You are a mail assistant."), host_tools(show_image, execute)]),
            workflows([name("Check emails"), predicate([goal(check_mail), args([(string:"Account")])])]),
            welcome("## Mail")).
      check_mail(_).
    `)
    expect(shell).toMatchObject({
      kind: 'agent',
      system: 'You are a mail assistant.',
      hostTools: ['show_image', 'execute'],
      welcome: '## Mail',
      workflows: [{ name: 'Check emails', goal: 'check_mail', args: [{ name: 'Account', type: 'string' }] }],
    })
  })

  it('reads a single workflow shell', async () => {
    const shell = await load(`
      shell(workflow([goal(entry_point), args([(string:"Param 1"), (integer:"Count")])])).
      entry_point(_, _).
    `)
    expect(shell).toMatchObject({
      kind: 'workflow',
      workflows: [{ goal: 'entry_point', args: [{ name: 'Param 1', type: 'string' }, { name: 'Count', type: 'integer' }] }],
    })
  })

  it('reads the dict form and defaults host tools', async () => {
    const shell = await load(`
      shell(_{ kind: agent, system: "Hi", workflows: [_{name:"A", goal:"a"}], welcome: "W" }).
      a.
    `)
    expect(shell).toMatchObject({
      kind: 'agent',
      system: 'Hi',
      hostTools: ['show_image', 'execute'],
      workflows: [{ name: 'A', goal: 'a', args: [] }],
    })
  })

  it('returns null when the harness has no shell', async () => {
    expect(await load('a(1).')).toBeNull()
  })
})
