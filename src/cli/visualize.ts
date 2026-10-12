/**
 * `deepclause visualize <file> -g <pred/arity> [--presentation|--spec] -o out.png`
 *
 * Loads a harness, takes its `listing` (source + doc annotations), asks the
 * active model for a Mermaid diagram of one entry predicate, and writes the
 * Mermaid source (and, best effort, a rendered image).
 */

import type { Command } from 'commander';
import * as fs from 'fs/promises';
import * as path from 'path';

import { loadConfig } from './config.js';
import { DMLSession } from '../repl/session.js';
import { resolveLlmBackend, type BackendKind } from '../repl/backend.js';
import { generateLlmReply } from '../prolog/bridge.js';
import type { LLMBackend } from '../types.js';
import type { Provider } from '../system/config/model-slots.js';
import {
  buildDiagramPrompt,
  extractMermaid,
  parseEntry,
  parseMermaidFlowchart,
  rasterizeSvg,
  renderFlowchartSvg,
  renderMermaidAscii,
  resolveOutputPaths,
  validateMermaid,
  writeMermaid,
  type DiagramMode,
} from '../visualize/index.js';

export interface VisualizeOptions {
  /** `presentation` (default) or `spec`. */
  mode: DiagramMode;
  /** Output path; `.mmd` writes Mermaid, `.png`/`.svg` renders it. */
  output?: string;
  workspace?: string;
  backend?: BackendKind;
  model?: string;
  provider?: string;
  /** Injected backend (used by tests and hosts that already hold one). */
  llmBackend?: LLMBackend;
  signal?: AbortSignal;
  /** Print a terminal-friendly ASCII diagram to stdout. */
  ascii?: boolean;
}

export interface VisualizeResult {
  entry: string;
  mode: DiagramMode;
  mermaid: string;
  ascii: string;
  svg: string;
  mmdPath: string;
  imagePath?: string;
  renderer?: string;
  warnings: string[];
}

/** Generate (and, when possible, render) a diagram for a harness entry. */
export async function visualizeHarness(
  file: string,
  entry: string,
  options: VisualizeOptions,
): Promise<VisualizeResult> {
  const workspace = path.resolve(options.workspace ?? './');
  const config = await loadConfig(workspace);

  // Backend: reuse the host's injected backend when given (tests, pi embedding);
  // otherwise resolve pi/env from the config.
  let llmBackend = options.llmBackend;
  let provider = options.provider ?? 'openai';
  let model = options.model ?? 'gpt-4o';
  if (!llmBackend) {
    const resolved = await resolveLlmBackend({
      kind: options.backend ?? config.backend?.kind,
      model: options.model ?? config.backend?.model,
      provider: options.provider,
      agentDir: config.backend?.agentDir,
      modelsPath: config.backend?.modelsPath,
      authPath: config.backend?.authPath,
    });
    llmBackend = resolved.llmBackend;
    provider = resolved.provider;
    model = resolved.model;
  }

  const { name, arity } = parseEntry(entry);
  const session = await DMLSession.open({
    workspacePath: workspace,
    tools: new Map(),
    runnerOptions: {
      model,
      provider: provider as Provider,
      temperature: config.temperatures.run,
      maxTokens: 8192,
      llmBackend,
    },
  });

  try {
    const loaded = await session.load(file);
    if (loaded.error) {
      throw new Error(loaded.error);
    }

    const arities = session.predicateArities(name);
    if (arities.length === 0) {
      throw new Error(`the harness does not define ${name}/N`);
    }
    if (arity !== undefined && !arities.includes(arity)) {
      throw new Error(`the harness defines ${name}/${arities.join(`, ${name}/`)}, not ${name}/${arity}`);
    }
    const entryLabel = arity !== undefined ? `${name}/${arity}` : `${name}/${arities[0]}`;

    const listing = await session.listing('all');
    const prompt = buildDiagramPrompt({ entry: entryLabel, mode: options.mode, listing });
    const { text } = await generateLlmReply({
      messages: [{ role: 'user', content: prompt }],
      modelOptions: { provider, model, temperature: config.temperatures.run },
      llmBackend,
      signal: options.signal,
    });

    const mermaid = extractMermaid(text);
    const warnings = validateMermaid(mermaid);
    const parsed = parseMermaidFlowchart(mermaid);
    const svg = renderFlowchartSvg(parsed);
    const ascii = renderMermaidAscii(mermaid);

    const { mmdPath, imagePath } = resolveOutputPaths(options.output, entryLabel);
    await writeMermaid(mmdPath, mermaid);

    let renderer: string | undefined;
    if (imagePath) {
      if (imagePath.toLowerCase().endsWith('.svg')) {
        // SVG needs no rasterizer and no browser.
        await fs.mkdir(path.dirname(path.resolve(imagePath)), { recursive: true });
        await fs.writeFile(imagePath, `${svg.svg.trim()}\n`, 'utf8');
        renderer = 'svg';
      } else {
        const png = await rasterizeSvg(svg.svg, { width: svg.width, height: svg.height });
        if (!png) {
          throw new Error(
            'cannot render PNG without the optional @resvg/resvg-js rasterizer; '
            + 'use `-o out.svg`, `-o out.mmd`, or `--ascii`.',
          );
        }
        await fs.mkdir(path.dirname(path.resolve(imagePath)), { recursive: true });
        await fs.writeFile(imagePath, png);
        renderer = 'resvg (no browser)';
      }
    }

    return { entry: entryLabel, mode: options.mode, mermaid, ascii, svg: svg.svg, mmdPath, imagePath, renderer, warnings };
  } finally {
    await session.dispose();
  }
}

/** Register the `visualize` command on the CLI program. */
export function registerVisualizeCommand(program: Command): void {
  program
    .command('visualize <file>')
    .description('Draw a Mermaid diagram of a harness skill from its listing and docs')
    .requiredOption('-g, --goal <pred/arity>', 'entry predicate to diagram, e.g. router/2')
    .option('--presentation', 'general-audience overview (default)')
    .option('--spec', 'engineering detail (decisions, calls, branches, fallbacks)')
    .option('-o, --output <file>', 'output file: .mmd writes Mermaid, .svg/.png render it (no browser)')
    .option('--ascii', 'print a terminal-friendly ASCII diagram to stdout')
    .option('--workspace <path>', 'workspace root for file operations', './')
    .option('--backend <kind>', 'model backend: pi (default) or env')
    .option('--model <model>', 'override configured model')
    .option('--provider <provider>', 'override configured provider')
    .action(async (file: string, options: Record<string, unknown>) => {
      try {
        const result = await visualizeHarness(file, String(options.goal), {
          mode: options.spec ? 'spec' : 'presentation',
          output: typeof options.output === 'string' ? options.output : undefined,
          ascii: options.ascii === true,
          workspace: typeof options.workspace === 'string' ? options.workspace : undefined,
          backend: options.backend as BackendKind | undefined,
          model: typeof options.model === 'string' ? options.model : undefined,
          provider: typeof options.provider === 'string' ? options.provider : undefined,
        });
        if (options.ascii) {
          console.log(result.ascii);
          return;
        }
        const lines = [
          '',
          `📊 ${result.mode} diagram of ${result.entry}`,
          `   mermaid: ${path.resolve(result.mmdPath)}`,
        ];
        if (result.imagePath && result.renderer) {
          lines.push(`   image:   ${path.resolve(result.imagePath)}  (via ${result.renderer})`);
        }
        for (const warning of result.warnings) {
          lines.push(`   ⚠ ${warning}`);
        }
        console.log(lines.join('\n'));
      } catch (error) {
        console.error(`❌ ${(error as Error).message}`);
        process.exit(1);
      }
    });
}
