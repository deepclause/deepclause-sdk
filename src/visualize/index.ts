/**
 * `deepclause visualize` — turn a harness (its `listing`, including `doc/2` and
 * `module_doc/1`) into a Mermaid diagram.
 *
 * Two granularities, mirroring sop2harness:
 *   - `presentation`: a general-audience overview (inputs, steps, answer).
 *   - `spec`: engineering detail (decisions, thresholds, task/exec calls,
 *     branches, fallbacks).
 *
 * The Mermaid source is always the deliverable; PNG/SVG rendering is best
 * effort via `@mermaid-js/mermaid-cli` (`mmdc`) when it is available.
 */

import { spawnSync } from 'child_process';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

export type DiagramMode = 'presentation' | 'spec';

const DIAGRAM_TYPE = /^\s*(flowchart|graph|sequenceDiagram|classDiagram|stateDiagram|erDiagram|journey|gantt|pie)\b/i;

export interface DiagramPromptInput {
  /** Entry predicate, e.g. `router/2`. */
  entry: string;
  mode: DiagramMode;
  /** The harness `listing` text (source + `doc/2` annotations + manifest). */
  listing: string;
}

/**
 * Build the diagram-generation prompt. The model only sees the harness listing
 * (source + docs) and the entry point, and must return Mermaid only.
 */
export function buildDiagramPrompt(input: DiagramPromptInput): string {
  const kind = input.mode === 'presentation'
    ? [
      'KIND: presentation — a general-audience overview.',
      '- Show the skill name, its inputs, the main steps in order, and the final answer.',
      '- Read like a process a stakeholder could follow. Avoid Prolog syntax and predicate names in labels.',
    ].join('\n')
    : [
      'KIND: specification — engineering detail.',
      '- Show every decision (choose/verify/rate/probability, holds), threshold, and rule fact.',
      '- Show task/N and prompt/N model calls (with their typed outputs), exec/2 tool calls, branches,',
      '  backtracking, and explicit fallback paths.',
    ].join('\n');

  return [
    'You are a technical writer who turns DeepClause (Prolog/DML) agent harnesses into Mermaid diagrams.',
    '',
    `Produce ONE Mermaid diagram for the skill \`${input.entry}\` in the harness below.`,
    '',
    'OUTPUT RULES',
    '- Return ONLY Mermaid source. No prose, no explanation, no ``` code fences.',
    '- The first line must be `flowchart TD` or `flowchart LR`.',
    '- Quote any node label containing spaces or punctuation: A["text"], B["x = 1"].',
    '- Plain flowchart syntax only (nodes, edges, subgraphs). No `classDef`/`style`/`click` directives.',
    '- Keep it readable: at most about 20 nodes.',
    '',
    'HARNESS SEMANTICS',
    '- task/N, prompt/N: a model call (prompt/N has fresh context).',
    '- exec(Tool(...)): a host tool call (usually bash).',
    '- choose/verify/rate/probability, holds: a semantic judgment (a decision).',
    '- Multiple clauses / `;` / `->`: alternative paths; show the "else"/fallback.',
    '',
    kind,
    '',
    `HARNESS (focus: ${input.entry}; the listing includes each predicate and its doc/2 annotation)`,
    '```',
    input.listing.trim() || '(empty harness)',
    '```',
  ].join('\n');
}

/** Extract Mermaid source from a model reply (strips fences and leading prose). */
export function extractMermaid(text: string): string {
  let out = (text ?? '').trim();
  const fence = out.match(/```(?:mermaid)?\s*\n?([\s\S]*?)```/i);
  if (fence) {
    out = fence[1].trim();
  }
  const lines = out.split('\n');
  const start = lines.findIndex((line) => DIAGRAM_TYPE.test(line));
  if (start > 0) {
    out = lines.slice(start).join('\n').trim();
  }
  return out;
}

/** Light structural validation (same spirit as sop2harness). */
export function validateMermaid(source: string): string[] {
  const issues: string[] = [];
  const lines = source.split('\n').map((line) => line.trim()).filter(Boolean);
  if (lines.length === 0) {
    return ['the diagram is empty'];
  }
  const first = lines.find((line) => !line.startsWith('%%'));
  if (!first) {
    issues.push('no diagram body (only comments)');
  } else if (!DIAGRAM_TYPE.test(first)) {
    issues.push(`unrecognized diagram type: ${first.split(/\s+/)[0]}`);
  }
  for (const [open, close] of [['{', '}'], ['[', ']'], ['(', ')']] as const) {
    const opens = (source.match(new RegExp(`\\${open}`, 'g')) ?? []).length;
    const closes = (source.match(new RegExp(`\\${close}`, 'g')) ?? []).length;
    if (opens !== closes) {
      issues.push(`unbalanced ${open}${close} (${opens}/${closes})`);
    }
  }
  return issues;
}

/** A mermaid renderer command, if one is available. */
export function findMermaidRenderer(): { command: string; args: string[] } | undefined {
  const which = process.platform === 'win32' ? 'where' : 'which';
  for (const candidate of ['mmdc', 'mmdc.cmd']) {
    if (spawnSync(which, [candidate], { stdio: 'ignore' }).status === 0) {
      return { command: candidate, args: [] };
    }
  }
  return undefined;
}

/** Absolute path to a system Chromium/Chrome, for puppeteer to launch. */
function findChrome(): string | undefined {
  const which = process.platform === 'win32' ? 'where' : 'which';
  for (const candidate of ['google-chrome', 'chromium', 'chromium-browser', 'chrome']) {
    const result = spawnSync(which, [candidate], { encoding: 'utf8' });
    if (result.status === 0) {
      const first = (result.stdout ?? '').trim().split('\n')[0].trim();
      if (first) {
        return first;
      }
    }
  }
  return undefined;
}

/**
 * Render a `.mmd` file to an image. Returns the renderer used, or throws with a
 * helpful message when no renderer is available.
 */
export async function renderMermaidFile(mmdPath: string, outputPath: string): Promise<string> {
  const renderer = findMermaidRenderer();
  const command = renderer ? renderer.command : 'npx';
  const args = renderer ? [...renderer.args] : ['--yes', '@mermaid-js/mermaid-cli'];
  args.push('-i', mmdPath, '-o', outputPath, '-b', 'white');

  const env = { ...process.env };
  if (!env.PUPPETEER_EXECUTABLE_PATH) {
    const chrome = findChrome();
    if (chrome) {
      env.PUPPETEER_EXECUTABLE_PATH = chrome;
    }
  }
  // Reuse the system browser instead of downloading Chromium for puppeteer.
  if (env.PUPPETEER_EXECUTABLE_PATH && env.PUPPETEER_SKIP_DOWNLOAD == null) {
    env.PUPPETEER_SKIP_DOWNLOAD = 'true';
  }

  // Chrome sandbox flags (mirrors sop2harness's `mmdc -p <config>` usage).
  const configPath = path.join(os.tmpdir(), `deepclause-mermaid-${process.pid}.json`);
  await fs.writeFile(configPath, JSON.stringify({
    args: ['--no-sandbox', '--disable-setuid-sandbox', '--disable-dev-shm-usage'],
  }));
  args.push('-p', configPath);

  try {
    const result = spawnSync(command, args, { env, encoding: 'utf8' });
    if (result.status !== 0) {
      const reason = (result.stderr || result.stdout || '').trim().split('\n').slice(-3).join(' ');
      throw new Error(
        `Mermaid renderer failed${reason ? `: ${reason}` : ''}. `
        + 'Install it with `npm i -g @mermaid-js/mermaid-cli` (or use `--output <file>.mmd`).',
      );
    }
    return renderer ? renderer.command : 'npx @mermaid-js/mermaid-cli';
  } finally {
    await fs.rm(configPath, { force: true });
  }
}

/** Split `name/arity` (or `name`) into a predicate name and arity. */
export function parseEntry(entry: string): { name: string; arity?: number } {
  const match = entry.trim().match(/^([A-Za-z_][A-Za-z0-9_]*)\s*\/\s*(\d+)$/);
  if (match) {
    return { name: match[1], arity: Number(match[2]) };
  }
  const bare = entry.trim().match(/^([A-Za-z_][A-Za-z0-9_]*)$/);
  if (bare) {
    return { name: bare[1] };
  }
  throw new Error(`invalid entry '${entry}': expected Name/Arity (e.g. router/2)`);
}

/** Resolve the `.mmd` path and the optional image path from `--output`. */
export function resolveOutputPaths(output: string | undefined, entry: string): { mmdPath: string; imagePath?: string } {
  const fallback = `${entry.replace(/[^A-Za-z0-9_.-]/g, '_') || 'diagram'}.mmd`;
  const target = output ?? fallback;
  if (target.toLowerCase().endsWith('.mmd')) {
    return { mmdPath: target };
  }
  const ext = path.extname(target);
  const mmdPath = ext ? `${target.slice(0, -ext.length)}.mmd` : `${target}.mmd`;
  return { mmdPath, imagePath: target };
}

/** Write the Mermaid source (creating directories as needed). */
export async function writeMermaid(mmdPath: string, source: string): Promise<void> {
  await fs.mkdir(path.dirname(path.resolve(mmdPath)), { recursive: true });
  await fs.writeFile(mmdPath, `${source.trim()}\n`, 'utf8');
}

// Terminal rendering: Mermaid → Unicode tree, Mermaid → SVG → PNG (no browser),
// and phone into pi-tui's Kitty/iTerm2 inline-image encoder.
export * from './ascii.js';
export * from './svg.js';
export * from './raster.js';
export * from './graphics.js';
export * from './display.js';
export * from './image.js';
export * from './transcode.js';
