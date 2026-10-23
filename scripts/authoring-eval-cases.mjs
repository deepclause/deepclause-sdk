// Contract rubrics for OPT-IN model authoring evaluations. No network at import.
export const AUTHORING_EVAL_CASES = [
  {
    id: 'shell-default',
    prompt: 'Approved to implement without another design question: build a normal conversational writing assistant. Give it a character-count tool and an uppercase workflow, with explicit input/output declarations. No custom conversation protocol is needed. Do not add unrelated features.',
    artifact: true,
  },
  {
    id: 'pure-skill',
    prompt: 'Approved: implement triple(+N,-R) for exact numeric N*3 with correct doc and a test on 4. No model call is needed. Commit it.',
    artifact: true, probe: 'triple(4, R)', expectedBinding: '12',
  },
  {
    id: 'annotation-cleanup',
    initial: 'doc(stale/1, "Old incorrect key").\ndoc(keep/1, "Keep this").\nkeep(_).',
    prompt: 'Approved: remove ONLY the stale doc(stale/1, ...) entry, keep all other documentation and predicates. Do not create alias predicates to silence warnings. Commit the cleanup.',
    artifact: true,
  },
  {
    id: 'scoped-task',
    prompt: 'Approved: implement inspect(+Text,-Reply), a bounded model task that may call only a character-count capability length_of/2. Declare a real delegate predicate and nested tool annotation with one last output. Include actual Text in the task prompt. Do not call the model while authoring; commit the module.',
    artifact: true, requiredReference: 'tasks',
  },
  {
    id: 'typed-fresh-extraction',
    prompt: 'Approved: implement extract_person(+Text,-Person) using an isolated fresh model transformation without tools. Declare a person schema with required string name and integer age default 0. Include actual Text. Do not call a model while authoring; commit.',
    artifact: true, requiredReference: 'tasks',
  },
  {
    id: 'form-workflow',
    prompt: 'Approved: implement a workflow-only intake shell with a zero-input workflow whose predicate returns Values. It should show known fields name and amount using a fixed form, close it, and return an explicit cancellation value without saving a guessed record. Do not test the interactive form inside execute; tell me where to run it.',
    artifact: true, requiredReference: 'forms',
  },
  {
    id: 'process-result',
    prompt: 'Design only; do not edit. I want to spawn worker(Input, Result) and have Result bind automatically in the parent. Read the process contract and explain the correct result-return and cleanup pattern instead.',
    requiredReference: 'processes',
  },
  {
    id: 'vision-boundaries',
    prompt: 'Design only; do not edit. Can I attach a workspace image, call prompt/N to analyse it, and then generate a new image using the selected text model? Check the media contract, correct the plan and explain the required capabilities.',
    requiredReference: 'media',
  },
  {
    id: 'python-input',
    prompt: 'Design only; do not edit or execute. Check the CURRENT Python tool schema and Python reference. How should a script receive text with spaces and quotes? Is stdin allowed? Is DML /tmp a safe shared staging path for Python?',
    requiredReference: 'python',
  },
  {
    id: 'reference-late-section',
    prompt: 'Read the processes lifecycle-and-cleanup section using reference, not a repository file, and explain how forms and process shutdown work in this browser embedding. Do not edit.',
    requiredReference: 'processes',
  },
];

export function scoreAuthoringCase(test, { source = '', shell, answer = '', events = [], probe = '' }) {
  const checks = [];
  const add = (name, passed) => checks.push({ name, passed: Boolean(passed) });
  const text = answer.toLowerCase();
  const refs = events.filter((e) => e.type === 'tool_call' && e.toolName === 'reference').map((e) => e.toolArgs?.topic);
  if (test.requiredReference) add('consulted focused contract', refs.includes(test.requiredReference));
  add('no removed edit tools', !events.some((e) => ['assert', 'assert_many', 'retract'].includes(e.toolName)));
  if (test.id === 'shell-default') {
    add('agent shell exists', shell?.kind === 'agent');
    add('typed tools and workflows', shell?.tools?.length > 0 && shell?.workflows?.length > 0
      && shell.tools.every((t) => Array.isArray(t.inputs) && Array.isArray(t.outputs))
      && shell.workflows.every((w) => Array.isArray(w.inputs) && Array.isArray(w.outputs)));
    add('no custom chat loop', !/\b(?:chat|input)\s*\(/.test(source));
  }
  if (test.id === 'pure-skill') {
    add('numeric result is exact', probe === test.expectedBinding);
    add('no LLM arithmetic', !/\b(?:task|prompt|llm)\s*\(/.test(source));
  }
  if (test.id === 'annotation-cleanup') {
    add('removed only stale doc key', !/doc\(stale\s*\/\s*1/.test(source) && /doc\(keep\s*\/\s*1/.test(source));
    add('no bogus alias', !/^stale\s*\(/m.test(source));
  }
  if (test.id === 'scoped-task') {
    add('scoped task uses explicit data', /with_tools\s*\(\s*\[\s*length_of\s*\]/.test(source) && /task\(/.test(source) && /\{Text\}/.test(source));
    add('real tool delegate', /tool\(length_of\(/.test(source) && /^length_of\(/m.test(source));
  }
  if (test.id === 'typed-fresh-extraction') {
    add('fresh typed extraction', /prompt\(/.test(source) && /schema\(person/.test(source) && /\{Text\}/.test(source));
    add('no tools required', !/\btask\(/.test(source));
  }
  if (test.id === 'form-workflow') {
    add('workflow-only surface', shell?.kind === 'workflow' && shell?.workflows?.some((w) => w.inputs.length === 0 && w.outputs.length === 1));
    add('complete form lifecycle', /form\(/.test(source) && /await_form\(/.test(source) && /close_form\(/.test(source));
    add('no interactive scratch test', !events.some((e) => e.toolName === 'execute' && /\b(?:form|intake|collect)\s*\(/.test(e.toolArgs?.goal ?? '')));
    add('foreground explained', /foreground|work sidebar|workflow/.test(text));
  }
  if (test.id === 'process-result') {
    add('message not shared output', /send|message/.test(text) && /copied|copies|not.*bind|cannot.*bind|no.*shared|isolat/.test(text));
    add('cleanup', /wait|reap|stop|exit/.test(text));
  }
  if (test.id === 'vision-boundaries') {
    add('task sees attachment', /attach_media/.test(text) && /task/.test(text));
    add('generation is capability-gated', /backend|configured|image.generator/.test(text));
  }
  if (test.id === 'python-input') {
    add('argv and no stdin', /argv/.test(text) && /no.{0,30}stdin|stdin.{0,30}(unsupported|not|no)|no standard.input/.test(text));
    add('shared workspace staging', /workspace/.test(text) && /tmp/.test(text));
  }
  if (test.id === 'reference-late-section') add('worker form limitation', /foreground/.test(text) && /worker/.test(text));
  return { passed: checks.every((check) => check.passed), checks };
}
