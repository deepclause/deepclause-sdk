// Deterministic contract-test backend, NOT an LLM quality evaluation.
export function canonicalBackend(id) {
  const state = { modelCalls: 0, sawMedia: false, usedCapability: false, sawPriorTask: false, freshPrompt: false, formCalls: [] };
  const values = { Claim: 'A supported claim.', Report: 'A grounded summary.',
    Reply: '3 characters', Person: { name: 'Ada', age: 36 }, Text: 'An observed image.',
    Summary: 'Ada likes blue.', Label: 'A title.' };
  return {
    state,
    async complete(request) {
      state.modelCalls++;
      if (id === 'judgment-routing') return { text: '{"answers":[{"value":"billing"}]}' };
      const names = (request.tools ?? []).map((tool) => tool.name);
      const toolMessages = request.messages.filter((message) => message.role === 'tool');
      if (id === 'custom-intake') {
        const lastUser = [...request.messages].reverse().find((message) => message.role === 'user');
        if (lastUser?.content?.includes('What did you just read')) {
          const remembered = request.messages.some((message) => message.role === 'assistant' && message.content?.includes('Tool result'));
          return { text: remembered ? 'I remember the earlier turn.' : 'ERROR: lost chat memory' };
        }
        if (names.includes('read_text') && !toolMessages.some((message) => message.toolName === 'read_text')) {
          return { text: '', toolCalls: [{ id: 'read', name: 'read_text', arguments: { arg1: 'canonical/input.txt' } }] };
        }
        state.usedCapability = true;
        return { text: 'Tool result: hello' };
      }
      if (id === 'task-tools' && !toolMessages.some((message) => message.toolName === 'length_of')) {
        if (!names.includes('length_of')) throw new Error('Expected scoped length_of tool');
        state.usedCapability = true;
        return { text: '', toolCalls: [{ id: 'length', name: 'length_of', arguments: { arg1: 'Ada' } }] };
      }
      if (id === 'dynamic-form') {
        const steps = [
          ['render_form', { arg1: { title: 'Intake', fields: [{ name: 'name', required: true }, { name: 'color', required: true }] } }],
          ['wait_for_form_change', { arg1: 'form_1' }],
          ['close_form', { arg1: 'form_1' }],
        ];
        const step = steps[state.formCalls.length];
        if (step) {
          state.formCalls.push(step[0]);
          return { text: '', toolCalls: [{ id: step[0], name: step[0], arguments: step[1] }] };
        }
      }
      if (id === 'vision') {
        state.sawMedia = request.messages.some((message) => message.media?.length > 0);
        if (!state.sawMedia) throw new Error('Vision example lost the attached image');
      }
      const resultTool = request.tools?.find((tool) => tool.name === 'set_result');
      const variable = resultTool?.parameters?.properties?.variable?.enum?.[0];
      let value = values[variable] ?? (id === 'schema-output' ? values.Person : 'A grounded summary.');
      if (id === 'task-memory') {
        const prior = request.messages.some((message) => message.role === 'assistant' && message.content?.includes('kept-alpha'));
        if (variable === 'First') value = 'kept-alpha';
        else if (variable === 'Second') {
          state.sawPriorTask = prior;
          if (!prior) throw new Error('Second task did not receive the first task memory');
          value = 'Prior task was visible.';
        } else {
          state.freshPrompt = !prior;
          if (prior) throw new Error('Fresh prompt inherited task memory');
          value = 'Memory scopes checked.';
        }
      }
      if (variable) return { text: id === 'task-memory' ? String(value) : '', toolCalls: [
        { id: 'result', name: 'set_result', arguments: { variable, value } },
        { id: 'finish', name: 'finish', arguments: { success: true } },
      ] };
      return { text: typeof value === 'string' ? value : JSON.stringify(value) };
    },
  };
}
