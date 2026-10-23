import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import * as fs from 'fs/promises'
import * as os from 'os'
import * as path from 'path'
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js'
import type { DMLEvent } from '../src/types.js'

let workspace: string
let repl: HarnessRepl | undefined

async function solutions(goal: string): Promise<number> {
  const events: DMLEvent[] = []
  for await (const event of repl!.session.query(goal)) events.push(event)
  return events.filter((event) => event.type === 'solution').length
}

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-annotation-delete-'))
  repl = await createHarnessRepl({ workspacePath: workspace, model: 'test-model', provider: 'openai' })
  await repl.session.loadCode(`
    doc(foo/2, "foo docs").
    doc(bar/2, "bar docs").
    schema(order, "order schema").
    script_file(run_tests_py, "scripts/run_tests.py").
  `)
})

afterEach(async () => {
  await repl?.dispose()
  repl = undefined
  await fs.rm(workspace, { recursive: true, force: true })
})

describe('keyed annotation deletion', () => {
  it('removes one doc/2 key and leaves the others', async () => {
    expect(await repl!.session.beginTurn()).toMatchObject({ ok: true })
    expect(await repl!.session.deletePredicate('doc:foo/2')).toMatchObject({ ok: true })
    expect(await solutions('doc(foo/2, _)')).toBe(0)
    expect(await solutions('doc(bar/2, _)')).toBe(1)
  })

  it('removes one schema/2 key', async () => {
    expect(await repl!.session.beginTurn()).toMatchObject({ ok: true })
    expect(await repl!.session.deletePredicate('schema:order')).toMatchObject({ ok: true })
    expect(await solutions('schema(order, _)')).toBe(0)
  })

  it('removes one script_file/2 key', async () => {
    expect(await repl!.session.beginTurn()).toMatchObject({ ok: true })
    expect(await repl!.session.deletePredicate('script_file:run_tests_py')).toMatchObject({ ok: true })
    expect(await solutions('script_file(run_tests_py, _)')).toBe(0)
  })

  it('reports a missing key instead of silently succeeding', async () => {
    expect(await repl!.session.beginTurn()).toMatchObject({ ok: true })
    const result = await repl!.session.deletePredicate('doc:missing/1')
    expect(result.ok).toBe(false)
    expect(result.error).toMatch(/no such keyed annotation|existence_error/)
  })
})
