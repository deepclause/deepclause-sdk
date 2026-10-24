import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import * as fs from 'fs/promises'
import * as os from 'os'
import * as path from 'path'
import { createHarnessRepl, type HarnessRepl } from '../src/repl/index.js'
import type { FormEventPayload } from '../src/types.js'

let workspace: string
let repl: HarnessRepl | undefined

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-workflow-form-'))
})

afterEach(async () => {
  await repl?.dispose()
  repl = undefined
  await fs.rm(workspace, { recursive: true, force: true })
})

describe('foreground workflow goals', () => {
  it('keeps forms available and returns the output binding', async () => {
    let submitted = false
    repl = await createHarnessRepl({
      workspacePath: workspace,
      model: 'test-model',
      provider: 'openai',
      onForm: (form: FormEventPayload) => {
        if (form.op !== 'open') return
        const id = (form as FormEventPayload & { formId?: string }).formId
        setTimeout(() => {
          if (!id) return
          submitted = repl!.session.submitForm(id, {
            action: 'submit',
            values: { name: 'Ada' },
          })
        }, 20)
      },
    })
    const loaded = await repl.session.loadCode(`
      intake(Result) :-
          form(_{title: "Intake", fields: [_{name: name, type: string, required: true}]}, F),
          await_form(F, Change),
          close_form(F),
          ( get_dict(values, Change, V), get_dict(name, V, Name)
          -> Result = Name
          ; Result = "cancelled"
          ).
    `)
    expect(loaded.error).toBeUndefined()

    const result = await repl.session.executeForeground('intake(R)', 1)
    expect(result.error).toBeUndefined()
    expect(result.solutions[0]).toEqual([{ name: 'R', value: '"Ada"' }])
    expect(submitted).toBe(true)
  })
})
