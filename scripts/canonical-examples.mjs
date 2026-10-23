// Single source for documentation, SDK contract tests and browser engine examples.
// Fixtures are created by the TEST/AUTHOR, never silently overwritten by the skill.
export const CANONICAL_EXAMPLES = [
  {
    id: 'work-agent', title: 'Default Work agent with a tool and workflow',
    note: 'The default conversation is supplied by shell/1. A custom persona does not need a DML chat loop. Inputs precede outputs; tools are not directly run from the sidebar.',
    requires: 'Work host; title workflow requires a text model. The character-count test is pure DML.',
    goal: 'length_of("Ada", N), write(N)', expected: '3',
    failure: 'Do not replace output binding with answer/1 or add a custom conversation loop.',
    code: String.raw`length_of(Text, N) :- string_length(Text, N).
title(Text, Label) :- prompt("Give a short title for: {Text}", string(Label)).
shell(_{
    kind: agent, system: "You are a concise writing assistant.", host_tools: [],
    tools: [_{name: "text_length", goal: "length_of", description: "Count characters.",
              inputs: [_{name: "Text", type: string}], outputs: [_{name: "N", type: integer}]}],
    workflows: [_{name: "Create title", goal: "title",
                  inputs: [_{name: "Text", type: string}], outputs: [_{name: "Label", type: string}]}],
    welcome: "Ask about text, or create a title."
}).`,
  },
  {
    id: 'basic-relation', title: 'Arguments, arithmetic and a deterministic branch',
    note: 'N is an input; Label is an output. is/2 evaluates arithmetic, = binds a returned value. A failed input guard is not an exception.',
    requires: 'No host tools or model.', goal: 'size_label(2, Label), write(Label)', expected: 'small',
    failure: 'size_label(-1, _) has zero solutions; catch/3 does not catch a false guard.',
    code: String.raw`size_label(N, Label) :-
    integer(N), N >= 0,
    Twice is N * 2,
    ( Twice > 10 -> Label = large ; Label = small ).`,
  },
  {
    id: 'typed-pipeline', title: 'Fresh typed transformation and explicit data flow',
    note: 'Each prompt includes its input. No tool or previous-memory dependency. Reject empty intermediate data before the next semantic step.',
    requires: 'Text model; tests supply deterministic typed completions.',
    goal: 'brief("A source document", Report), write(Report)', expected: 'A grounded summary.',
    failure: 'An empty Claim fails before generating a report.',
    code: String.raw`brief(Text, Report) :-
    prompt("Extract the central claim from: {Text}", string(Claim)),
    Claim \== "",
    prompt("Explain this claim using only the source {Text}: {Claim}", string(Report)).`,
  },
  {
    id: 'task-tools', title: 'A scoped task calls one declared capability',
    note: 'Host tools, nested tool/N annotations and shell tools are different surfaces. This task may only call length_of; the real predicate remains callable directly.',
    requires: 'Text model with tool calling; test checks the capability really executes.',
    goal: 'inspect("Ada", Reply), write(Reply)', expected: '3 characters',
    failure: 'Naming length_of in prose without registering its annotation would not expose it.',
    code: String.raw`length_of(Text, N) :- string_length(Text, N).
tool(length_of(Text, N), "Count characters.", [string]) :- length_of(Text, N).
inspect(Text, Reply) :-
    with_tools([length_of], task("Call length_of for {Text}; report the observed count.", string(Reply))).`,
  },
  {
    id: 'task-memory', title: 'Task memory accumulates; a fresh prompt does not inherit it',
    note: 'Task memory is DML harness memory, not the authoring chat. The tasks share a fresh group, then a separate prompt receives only explicitly supplied data.',
    requires: 'Text model; tests inspect actual request memory, not only returned strings.',
    goal: 'memory_demo(Out), write(Out)', expected: 'Memory scopes checked.',
    failure: 'Do not assume a prompt sees a prior task or a nested tool inherits the Work transcript.',
    code: String.raw`memory_demo(Out) :-
    with_fresh_memory(with_tools([], (
        task("Return marker alpha.", string(First)),
        task("Report the earlier marker. Explicit value: {First}", string(Second))
    ))),
    prompt("Report only this explicit result: {Second}", string(Out)).`,
  },
  {
    id: 'judgment-routing', title: 'A constrained semantic choice with explicit evidence',
    note: 'Choose returns one option atom; deterministic branching follows it. A semantic judgment is not proof that an external action was performed.',
    requires: 'Configured judgment backend (the test uses the session model with a deterministic judge response).',
    goal: 'route("I was charged twice", Team), write(Team)', expected: 'billing',
    failure: 'Unknown evidence is not a reason to claim that a real refund happened.',
    code: String.raw`route(Message, Team) :-
    choose(Message, "Which team handles this?", [billing, orders, account], Choice),
    ( Choice == billing -> Team = billing
    ; Choice == orders -> Team = orders
    ; Team = account
    ).`,
  },
  {
    id: 'schema-output', title: 'A declared schema rather than guessed dict keys',
    note: 'The schema is an output contract, not merely a suggested JSON shape. Missing required fields and invalid types fail validation.',
    requires: 'Text model; deterministic schema-aware test output.',
    goal: 'extract_person("Ada, age 36", Person), get_dict(name, Person, Name), write(Name)', expected: 'Ada',
    failure: 'Missing name must not be silently converted to an empty string.',
    code: String.raw`schema(person, [field(name, string, required), field(age, integer, default(0))]).
extract_person(Text, Person) :-
    prompt("Extract a person from: {Text}", schema(person, Person)).`,
  },
  {
    id: 'python-files', title: 'Python, files and argv',
    note: 'Write/link the helper during authoring, not on each run. Script link normalise_py is distinct from the normalise/2 predicate. run_script checks exit status and parses JSON.',
    requires: 'Python host capability (or a compatible script execution embedding). Files below are fixtures to create before calling the skill.',
    files: { 'canonical/input.txt': 'hello', 'scripts/normalise.py': "import json, sys\nfrom pathlib import Path\nprint(json.dumps({'text': Path(sys.argv[1]).read_text().upper()}))\n" },
    goal: 'normalise("canonical/input.txt", Text), write(Text)', expected: 'HELLO',
    failure: 'A missing input file produces a script_failed error; no model fills the missing text.',
    code: String.raw`script_file(normalise_py, "scripts/normalise.py").
normalise(Path, Text) :-
    run_script(normalise_py, [Path], Data),
    get_dict(text, Data, Text).`,
  },
  {
    id: 'swarm', title: 'Bounded parallel mapping with visible failures',
    note: 'Results preserve input order. Every item has its own process; failed items remain visible. Use pmap only if any failure should fail the whole batch.',
    requires: 'Process-capable session; no model.',
    goal: 'parallel_values([1,2,3], Results), write(Results)', expected: '[ok(2),ok(4),ok(6)]',
    failure: 'Inspect failed items rather than assuming all results succeeded.',
    code: String.raw`double(N, R) :- R is N * 2.
parallel_values(Items, Results) :- swarm(double, Items, Results, 2).`,
  },
  {
    id: 'spawn-python', title: 'Spawn a worker and return by message',
    note: 'Create required files before spawn. Child output variables do not bind the parent; send a result, receive it with a bound, then reap the worker.',
    requires: 'Python and process host. Test fixture canonical/input.txt exists before spawn.',
    files: { 'canonical/input.txt': 'hello' },
    goal: 'read_in_worker("canonical/input.txt", Out), write(Out)', expected: 'HELLO',
    failure: 'A failed worker may never send; the receive is bounded. Inspect the worker and stop/reap it after a timeout.',
    code: String.raw`read_in_worker(Path, Out) :-
    self(Me), spawn(file_worker(Path, Me), Pid),
    receive(done(Out), 30000), wait(Pid, _).
file_worker(Path, Parent) :-
    exec(python(code: "from pathlib import Path\nimport sys\nprint(Path(sys.argv[1]).read_text().upper())", argv: [Path]), R),
    get_dict(exitCode, R, 0), get_dict(stdout, R, Out),
    send(Parent, done(Out)).`,
  },
  {
    id: 'service', title: 'A stateful service with correlated replies',
    note: 'A service is for message-driven state, not a single model call. Registered names survive across turns while the process is alive. Stop/reap test services.',
    requires: 'Process-capable session.',
    goal: 'service_demo(Text), write(Text)', expected: '1 2 2',
    failure: 'Duplicate service names are rejected; callers use bounded service_call timeouts.',
    code: String.raw`service_demo(Text) :-
    serve(counter, counter_loop(0), Pid),
    service_call(counter, bump, First, 5000),
    service_call(counter, bump, Second, 5000),
    service_call(counter, read, Final, 5000),
    service_cast(counter, stop), wait(Pid, _),
    format(string(Text), "~w ~w ~w", [First, Second, Final]).
counter_loop(Count) :-
    receive(Msg),
    ( Msg = request(From, Ref, read)
    -> send(From, reply(Ref, Count)), counter_loop(Count)
    ; Msg = request(From, Ref, bump)
    -> Next is Count + 1, send(From, reply(Ref, Next)), counter_loop(Next)
    ; Msg = stop -> true
    ; counter_loop(Count)
    ).`,
  },
  {
    id: 'vision', title: 'Vision input is not display or generation',
    note: 'Attach a workspace reference, then use a vision task. A fresh prompt does not read the attached image. Display is a separate output operation.',
    requires: 'Vision-capable model and supported image file; tests provide a tiny PNG and assert media is actually in the model request.',
    binaryFiles: { 'canonical/pixel.png': 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jRZkAAAAASUVORK5CYII=' },
    goal: 'describe_image("canonical/pixel.png", Text), write(Text)', expected: 'An observed image.',
    failure: 'A text-only model cannot analyse pixels. Image generation additionally needs a configured image backend.',
    code: String.raw`describe_image(Path, Text) :-
    read_media(Path, Image), attach_media(Image),
    with_tools([], task("Describe only the attached image.", string(Text))).
show_image_file(Path) :- output(media(Path)).`,
  },
  {
    id: 'fixed-form', title: 'Fixed foreground form with a cancellation branch',
    note: 'Known fields need no model. The host enforces hard validation; a refused submit is not delivered. Never save on cancel or unavailable.',
    requires: 'Foreground form host. Browser disposable execute/process workers do not render forms.',
    goal: 'claim_form(V), get_dict(name, V, Name), format(string(S), "Hello ~w", [Name]), write(S)', expected: 'Hello Ada',
    failure: 'Cancel/unavailable returns an empty dict; a caller must not create a record from it.',
    code: String.raw`claim_form(Values) :-
    form(_{title: "Quick check-in", fields: [_{name: name, type: string, required: true}]}, F),
    await_form(F, Change), close_form(F),
    ( Change.action == submit -> Change.values = Values ; Values = _{} ).`,
  },
  {
    id: 'dynamic-form', title: 'Context-dependent fields designed by a scoped task',
    note: 'The nested model designs the form, waits for actual values and closes it. Result/finish machinery is internal, not a user tool annotation.',
    requires: 'Foreground form host and text model with tools.',
    goal: 'collect_details("name and favorite color", Summary), write(Summary)', expected: 'Ada likes blue.',
    failure: 'A form rendered without a real submission is not evidence that intake completed.',
    code: String.raw`collect_details(Context, Summary) :-
    with_tools([render_form, wait_for_form_change, close_form],
        task("Collect {Context} using a form, wait for the user action, close the form, and report only what they entered.", string(Summary))).`,
  },
  {
    id: 'custom-intake', title: 'Custom staged conversation (advanced, not the default)',
    note: 'Only use a custom loop for a required protocol. This example validates project intake BEFORE allowing conversation. A normal assistant with a persona/tools uses shell/1 instead.',
    requires: 'Foreground conversation/input host and text model; file fixture exists. First input is the required project name, followed by chat messages.',
    files: { 'canonical/input.txt': 'hello' },
    goal: 'agent_main', expected: 'I remember the earlier turn.',
    failure: 'An empty project fails before chat starts; no custom loop should be added for ordinary conversation.',
    code: String.raw`agent_main :-
    input("Project name?", Project), Project \== "",
    system("You are a concise assistant for project {Project}."),
    conversation.
conversation :- input(Message), chat(Message), conversation.
read_text(Path, Text) :- read_file(Path, Text).
tool(read_text(Path, Text), "Read a workspace text file.", [string]) :- read_text(Path, Text).`,
  },
];

export function examplesMarkdown() {
  return '# Runnable canonical examples\n\nGenerated from scripts/canonical-examples.mjs; do not hand-copy into the system prompt.\nPick ONE matching section. Each skill binds its outputs; agent_main is only used\nfor the explicitly custom conversation. Host-dependent examples list prerequisites.\n\n'
    + CANONICAL_EXAMPLES.map((example) => `## ${example.id}\n\n**${example.title}**\n\n${example.note}\n\nRequires: ${example.requires}\n\n\`\`\`prolog\n${example.code}\n\`\`\`\n\nTest goal: \`${example.goal}\`. Expected: \`${example.expected}\`.\n\nFailure/cancel check: ${example.failure}\n`
      + Object.entries(example.files ?? {}).map(([name, text]) => `\nFixture \`${name}\` (create before run):\n\n\`\`\`${name.endsWith('.py') ? 'python' : 'text'}\n${text.trimEnd()}\n\`\`\`\n`).join('')
      + (example.binaryFiles ? '\nBinary fixture: provide a valid supported image before running (test suite includes one).\n' : '')).join('\n');
}
