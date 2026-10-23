import { describe, expect, it } from 'vitest'
import { createWorkAgentTools, type HarnessToolController } from '../src/repl/harness-tools.js'
import type { ShellSpec } from '../src/types.js'

const shell: ShellSpec = {
  kind: 'agent',
  system: 'You are a test assistant.',
  hostTools: ['show_image'],
  tools: [
    {
      name: 'lookup_order',
      goal: 'lookup_order',
      description: 'Find an order.',
      inputs: [{ name: 'OrderId', type: 'string' }],
      outputs: [{ name: 'Order', type: 'string' }],
    },
  ],
  workflows: [
    {
      name: 'Review',
      goal: 'review',
      inputs: [{ name: 'Text', type: 'string' }],
      outputs: [{ name: 'Summary', type: 'string' }],
    },
  ],
  welcome: '',
}

function controller(): HarnessToolController {
  return {
    shell: () => shell,
    isFrozen: () => false,
    execute: async () => ({
      bindings: [{ name: 'Out1', value: 'result' }],
      solutions: [[{ name: 'Out1', value: 'result' }]],
      output: '',
      elapsedMs: 1,
    }),
    executeForeground: async () => ({
      bindings: [{ name: 'Out1', value: 'result' }],
      solutions: [[{ name: 'Out1', value: 'result' }]],
      output: '',
      elapsedMs: 1,
    }),
    showImage: async () => ({ message: 'image' }),
  } as unknown as HarnessToolController
}

describe('Work agent tools', () => {
  it('exposes declared tools and run_workflow, never authoring/DML tools', () => {
    const tools = createWorkAgentTools(controller())
    expect([...tools.keys()].sort()).toEqual(['lookup_order', 'run_workflow', 'show_image'])
    expect(tools.has('execute')).toBe(false)
    expect(tools.has('reference')).toBe(false)
    expect(tools.has('define')).toBe(false)
  })

  it('calls a declared tool with typed inputs and returns named outputs', async () => {
    const tools = createWorkAgentTools(controller())
    const result = await tools.get('lookup_order')!.execute({ OrderId: 'A-1' })
    expect(result).toMatchObject({ ok: true, outputs: { Order: 'result' } })
  })

  it('runs a workflow with input values', async () => {
    const tools = createWorkAgentTools(controller())
    const result = await tools.get('run_workflow')!.execute({ name: 'Review', inputs: { Text: 'hi' } })
    expect(result).toMatchObject({ ok: true, outputs: { Summary: 'result' } })
  })
})
