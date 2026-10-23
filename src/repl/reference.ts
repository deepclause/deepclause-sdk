/** Focused, resumable documentation pages. Never rely on the agent's result truncation. */
import type { WorkspaceDocAssetName } from '../system/assets/index.js';

export interface ReferenceOptions {
  section?: string;
  query?: string;
  /** Block offset returned by the previous page (not a character offset). */
  offset?: number;
}

export const REFERENCE_PAGE_LIMIT = 7000;
export const REFERENCE_TOPICS: ReadonlyArray<{
  topic: string; asset: WorkspaceDocAssetName; title: string; when: string;
}> = [
  { topic: 'basics', asset: 'dml-basics', title: 'DML syntax and execution', when: 'arguments, unification, arithmetic, failure, determinism' },
  { topic: 'language', asset: 'dml-reference', title: 'Language API', when: 'exact predicate signatures and advanced builtins' },
  { topic: 'tasks', asset: 'dml-tasks', title: 'Model calls and memory', when: 'task, prompt, llm, typed outputs, schemas and nested tools' },
  { topic: 'shell', asset: 'dml-shell', title: 'Work shell contract', when: 'normal agents, tools, workflows and explicit input/output modes' },
  { topic: 'python', asset: 'dml-python', title: 'Python host calls', when: 'files, scripts, argv, packages and host errors' },
  { topic: 'processes', asset: 'dml-processes', title: 'Processes and concurrency', when: 'swarm, spawn, services, mailboxes and cleanup' },
  { topic: 'media', asset: 'dml-media', title: 'Images and media', when: 'vision input, display, generation capability and HTML' },
  { topic: 'forms', asset: 'dml-forms', title: 'Structured human input', when: 'fixed/dynamic forms, validation, cancellation, foreground execution' },
  { topic: 'authoring', asset: 'dml-authoring', title: 'Authoring workflow', when: 'define, delete, transactions and conservative changes' },
  { topic: 'debugging', asset: 'dml-debugging', title: 'Debugging and tests', when: 'failure, wrong bindings, tool errors or execution contexts' },
  { topic: 'patterns', asset: 'harness-patterns', title: 'Design recipes', when: 'choose the smallest orchestration pattern' },
  { topic: 'examples', asset: 'harness-examples', title: 'Runnable canonical examples', when: 'copy a complete tested shape with prerequisites and expected results' },
  { topic: 'snippets', asset: 'prolog-snippets', title: 'Prolog glue', when: 'look up one deterministic library operation' },
];
const ALIASES: Record<string, string> = {
  dml: 'language', 'harness-patterns': 'patterns', 'dml-forms': 'forms',
  'host-tools': 'tools', 'dml-tools': 'tasks',
};
export function referenceTopic(value: string): string {
  const key = value.trim().toLowerCase();
  return ALIASES[key] ?? (key || 'index');
}
export function sectionId(title: string): string {
  return title.toLowerCase().replace(/[`'"()]/g, '').replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
}
interface Section { id: string; title: string; level: number; start: number; end: number }
export function referenceSections(markdown: string): Section[] {
  const result: Section[] = [];
  const counts = new Map<string, number>();
  let position = 0;
  let fenced = false;
  for (const line of markdown.split('\n')) {
    if (/^\s*(```|~~~)/.test(line)) fenced = !fenced;
    const heading = !fenced ? /^(#{2,4})\s+(.+)$/.exec(line) : null;
    if (heading) {
      const slug = sectionId(heading[2]);
      const count = (counts.get(slug) ?? 0) + 1;
      counts.set(slug, count);
      result.push({ id: count === 1 ? slug : `${slug}-${count}`, title: heading[2], level: heading[1].length, start: position, end: markdown.length });
    }
    position += line.length + 1;
  }
  result.forEach((section, index) => {
    section.end = result.slice(index + 1).find((next) => next.level <= section.level)?.start ?? markdown.length;
  });
  return result;
}
/** Paragraphs and fenced code blocks are indivisible, so a page is copyable. */
export function referenceBlocks(text: string): string[] {
  const blocks: string[] = [];
  let lines: string[] = [];
  let fenced = false;
  const flush = (): void => { if (lines.length) blocks.push(lines.join('\n')); lines = []; };
  for (const line of text.trim().split('\n')) {
    if (/^\s*(```|~~~)/.test(line)) {
      if (!fenced) flush();
      fenced = !fenced;
      lines.push(line);
      if (!fenced) flush();
    } else if (!line.trim() && !fenced) flush();
    else lines.push(line);
  }
  flush();
  return blocks;
}
function revision(text: string): string {
  let value = 2166136261;
  for (let i = 0; i < text.length; i++) value = Math.imul(value ^ text.charCodeAt(i), 16777619);
  return (value >>> 0).toString(16);
}
function fits(text: string): boolean {
  // The actual reference tool returns {text}; allow for JSON escaping AND the agent cap.
  return JSON.stringify({ text }).length <= REFERENCE_PAGE_LIMIT;
}
function page(topic: string, blocks: string[], options: ReferenceOptions, hash: string): string {
  const offset = options.offset ?? 0;
  if (!Number.isInteger(offset) || offset < 0 || offset >= Math.max(1, blocks.length)) {
    throw new Error(`Invalid reference offset ${offset}; request the index or use the returned next offset.`);
  }
  const header = `Reference: ${topic}${options.section ? ` / ${options.section}` : options.query ? ` / search ${JSON.stringify(options.query)}` : ' / index'}\nRevision: ${hash}\n`;
  if (blocks.some((block) => !fits(`${header}\n${block}\n${' '.repeat(700)}`))) {
    throw new Error(`Reference block exceeds the safe response budget in ${topic}; split the documentation block instead of truncating it.`);
  }
  let body = '';
  let next = offset;
  // Reserve room for the continuation and section names, including escaped JSON.
  while (next < blocks.length && fits(`${header}\n${body}${blocks[next]}\n\n${' '.repeat(700)}`)) {
    body += `${blocks[next]}\n\n`;
    next++;
  }
  if (next === offset && blocks.length > 0) {
    throw new Error(`Reference block exceeds the safe response budget in ${topic}; the documentation must split this block. Nothing was silently truncated.`);
  }
  const more = next < blocks.length;
  const follow = more
    ? `More: yes. Continue with reference(${JSON.stringify({ topic, ...options, offset: next })}).`
    : 'More: no.';
  const result = `${header}\n${body.trimEnd()}\n\n${follow}`;
  if (!fits(result)) throw new Error('Reference page metadata exceeds its response budget.');
  return result;
}
export function referenceIndex(): string {
  return ['# Reference index', 'Read one relevant contract before using an unfamiliar feature; follow its section ids, not repository paths.',
    ...REFERENCE_TOPICS.map((entry) => `- ${entry.topic}: ${entry.title} — ${entry.when}`),
    '- tools (alias host-tools): the CURRENT host catalog and parameter schemas.',
    'reference({topic}) returns an index. Use section (an id from that index), query (search), and offset (returned continuation).',
    'Do not reread the whole library each turn. After a contract error, consult that feature before retrying.',
  ].join('\n');
}
export function renderReference(topic: string, markdown: string, options: ReferenceOptions = {}): string {
  const sections = referenceSections(markdown);
  if (options.section) {
    const key = sectionId(options.section);
    const exact = sections.find((section) => section.id === options.section || section.id === key);
    if (!exact) throw new Error(`Unknown section '${options.section}' in ${topic}. Read reference({topic: ${JSON.stringify(topic)}}) for valid ids.`);
    return page(topic, referenceBlocks(markdown.slice(exact.start, exact.end)), { ...options, section: exact.id }, revision(markdown));
  }
  if (options.query) {
    const words = options.query.toLowerCase().split(/\s+/).filter(Boolean);
    const matches = sections.filter((section) => words.every((word) =>
      markdown.slice(section.start, section.end).toLowerCase().includes(word)));
    const blocks = matches.length
      ? matches.map((section) => `- ${section.id}: ${section.title}\n  Read with section=${JSON.stringify(section.id)}.`)
      : ['No matching sections. Try fewer terms or read the topic index.'];
    return page(topic, blocks, options, revision(markdown));
  }
  const introduction = markdown.slice(0, sections[0]?.start ?? markdown.length).trim();
  const blocks = [introduction,
    'Choose a section id below. Each response is bounded; use its continuation if More: yes.',
    ...sections.map((section) => `- ${section.id}: ${section.title}`)].filter(Boolean);
  return page(topic, blocks, options, revision(markdown));
}
