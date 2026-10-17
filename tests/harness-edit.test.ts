import { afterEach, describe, expect, it } from 'vitest'
import { createHarnessRepl } from '../src/repl/index.js'
import type { HarnessRepl } from '../src/repl/index.js'

let repl: HarnessRepl | undefined

afterEach(async () => {
  await repl?.dispose()
  repl = undefined
})

const backend = { complete: async () => ({ text: '' }) }

describe('harnessSource / replaceHarness', () => {
  it('returns loadable source and replaces the whole module on commit', async () => {
    repl = await createHarnessRepl({
      workspacePath: '/tmp/dc-harness-edit',
      harness: 'edit',
      model: 'test',
      provider: 'openai',
      llmBackend: backend as never,
    })
    const session = repl.session
    const loaded = await session.loadCode(
      ['greet(Name, Out) :- format(atom(Out), "hi ~w", [Name]).', 'old(1).'].join('\n'),
    )
    expect(loaded.error).toBeUndefined()

    const before = await session.harnessSource()
    expect(before).toContain('greet')
    expect(before).toContain('old(1)')

    const result = await session.replaceHarness(
      ['greet(Name, Out) :- format(atom(Out), "hello ~w", [Name]).'].join('\n'),
    )
    expect(result.committed).toBe(true)
    expect(result.error).toBeUndefined()
    expect(result.changed).toContain('-old/1')

    const view = session.harnessView()
    const indicators = view.predicates.map((predicate) => predicate.indicator)
    expect(indicators).toContain('greet/2')
    expect(indicators).not.toContain('old/1')

    const after = await session.harnessSource()
    expect(after).toContain('hello')
    expect(after).not.toContain('old(1)')
  })

  it('rejects a source that does not parse and leaves the module intact', async () => {
    repl = await createHarnessRepl({
      workspacePath: '/tmp/dc-harness-edit-2',
      harness: 'edit2',
      model: 'test',
      provider: 'openai',
      llmBackend: backend as never,
    })
    const session = repl.session
    await session.loadCode('keep(1).')

    const result = await session.replaceHarness('broken( :- .')
    expect(result.committed).toBe(false)
    expect(result.error).toBeTruthy()
    expect(session.harnessView().predicates.map((predicate) => predicate.indicator)).toContain('keep/1')
  })
})
