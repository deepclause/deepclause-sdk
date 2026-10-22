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
      workflows: [{ name: 'Check emails', goal: 'check_mail', inputs: [{ name: 'Account', type: 'string' }], outputs: [] }],
    })
  })

  it('reads a single workflow shell', async () => {
    const shell = await load(`
      shell(workflow([goal(entry_point), args([(string:"Param 1"), (integer:"Count")])])).
      entry_point(_, _).
    `)
    expect(shell).toMatchObject({
      kind: 'workflow',
      workflows: [{ goal: 'entry_point', inputs: [{ name: 'Param 1', type: 'string' }, { name: 'Count', type: 'integer' }], outputs: [] }],
    })
  })

  it('reads file workflow arguments', async () => {
    const shell = await load(`
      shell(workflow([goal(read_document), args([(file:"Document"), (string:"Question")])])).
      read_document(_, _, _).
    `)
    expect(shell).toMatchObject({
      kind: 'workflow',
      workflows: [
        { goal: 'read_document', inputs: [{ name: 'Document', type: 'file' }, { name: 'Question', type: 'string' }], outputs: [] },
      ],
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
      workflows: [{ name: 'A', goal: 'a', inputs: [], outputs: [] }],
    })
  })


  it('reads explicit tool inputs and outputs', async () => {
    const shell = await load(`
      shell(_{
        kind: agent,
        tools: [_{
          name: "Lookup",
          goal: "lookup",
          description: "Find a thing.",
          inputs: [_{name: "Id", type: string}],
          outputs: [_{name: "Thing", type: object}]
        }],
        workflows: [_{
          name: "Review",
          goal: "review",
          inputs: [_{name: "Text", type: string}],
          outputs: [_{name: "Summary", type: string}, _{name: "Score", type: integer}]
        }]
      }).
      lookup(_, _).
      review(_, _, _).
    `)
    expect(shell).toMatchObject({
      tools: [{ name: 'Lookup', goal: 'lookup', description: 'Find a thing.', inputs: [{ name: 'Id', type: 'string' }], outputs: [{ name: 'Thing', type: 'object' }] }],
      workflows: [{ name: 'Review', inputs: [{ name: 'Text', type: 'string' }], outputs: [{ name: 'Summary', type: 'string' }, { name: 'Score', type: 'integer' }] }],
    })
  })


  it('reads inputs and outputs from the workflow form', async () => {
    const shell = await load(`
      shell(workflow([
        goal(demo),
        inputs([(string:"Text")]),
        outputs([(string:"Result")])
      ])).
      demo(Text, Result) :- Result = Text.
    `)
    expect(shell).toMatchObject({
      kind: 'workflow',
      workflows: [{ goal: 'demo', inputs: [{ name: 'Text', type: 'string' }], outputs: [{ name: 'Result', type: 'string' }] }],
    })
  })

  it('returns null when the harness has no shell', async () => {
    expect(await load('a(1).')).toBeNull()
  })
})
