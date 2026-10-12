#!/usr/bin/env node
/**
 * DeepClause CLI.
 *
 * Commands: init, set-model, show-model, list-models, run, repl, login,
 * logout, set-backend.
 */

import { Command } from 'commander';
import * as path from 'path';
import * as fs from 'fs/promises';
import { deepClauseDirExists, initConfig, loadConfig, setModel, showModel } from './config.js';
import { run } from './run.js';
import type { ModelSlot } from '../system/config/model-slots.js';
import { registerReplCommand } from './repl.js';
import { registerVisualizeCommand } from './visualize.js';
import { registerSetBackendCommand, setBackend } from './set-backend.js';
import { registerLoginCommands } from './login.js';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

// Get version from package.json
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const packageJsonPath = join(__dirname, '../../package.json');
const packageJson = JSON.parse(readFileSync(packageJsonPath, 'utf-8'));
const version = packageJson.version;

const program = new Command();

program
  .name('deepclause')
  .description('Run and build DML skills with the REPL and one-shot runner')
  .version(version);

// =============================================================================
// Configuration Commands
// =============================================================================

program
  .command('init')
  .description('Initialize DeepClause configuration in current directory')
  .option('-f, --force', 'Overwrite existing configuration')
  .option('--model <model>', 'Set initial model (default: gpt-4o)')
  .action(async (options) => {
    try {
      await initConfig(process.cwd(), options);
      console.log('✅ DeepClause initialized successfully');
      console.log('   Configuration: .deepclause/config.json');
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

program
  .command('set-model <model>')
  .description('Set the default LLM model (format: provider:model, or just a model name to search)')
  .option('--slot <slot>', 'Apply only to a single slot: gateway, run, or compile')
  .option('--provider <provider>', 'Non-interactive: specify provider directly (openai, anthropic, google, openrouter, custom:name)')
  .option('--thinking <level>', 'thinking level for reasoning models (off|minimal|low|medium|high|xhigh|max)')
  .action(async (model, options) => {
    try {
      const slot = options.slot as ModelSlot | undefined;

      // With the pi backend the model comes from pi's catalog, so route the
      // change through set-backend (which validates auth and persists the
      // model + thinking level together). The env backend keeps the DB flow.
      const config = await loadConfig(process.cwd());
      if ((config.backend?.kind ?? 'pi') === 'pi') {
        const result = await setBackend(process.cwd(), 'pi', {
          model: model.includes(':') || model.includes('/') ? model.replace(':', '/') : model,
          provider: options.provider,
          thinking: options.thinking,
        });
        console.log(result.info);
        return;
      }

      // If the model already has a provider prefix (contains : or is custom:), use as-is
      const hasProviderPrefix = model.includes(':') || model.startsWith('custom:');

      let finalModel = model;

      if (!hasProviderPrefix) {
        // Bare model name — search the database
        const { searchModels } = await import('../system/config/model-database.js');
        const results = searchModels(model);

        if (results.length === 0) {
          console.error(`❌ No models found matching "${model}". Try "deepclause list-models" to see available models.`);
          process.exit(1);
        }

        if (options.provider) {
          // Non-interactive: use specified provider
          const match = results.find(r =>
            r.providers.some(p => p.provider === options.provider || p.label === options.provider),
          );
          if (!match) {
            console.error(`❌ Provider "${options.provider}" not available for model "${model}".`);
            console.error('Available providers for matching models:');
            for (const r of results.slice(0, 5)) {
              console.error(`  ${r.modelId}: ${r.providers.map(p => p.label).join(', ')}`);
            }
            process.exit(1);
          }
          const provider = match.providers.find(p => p.provider === options.provider || p.label === options.provider)!;
          finalModel = `${provider.provider}:${provider.modelId}`;
        } else if (results.length === 1 && results[0].providers.length === 1) {
          // Single match, single provider — use directly
          const r = results[0];
          const p = r.providers[0];
          finalModel = `${p.provider}:${p.modelId}`;
          console.log(`Found: ${r.entry.name} → ${p.label}`);
        } else {
          // Multiple matches or providers — show interactive selection
          console.log(`\nModels matching "${model}":\n`);
          let idx = 1;
          const choices: { finalModel: string; label: string }[] = [];
          for (const r of results.slice(0, 10)) {
            for (const p of r.providers) {
              console.log(`  ${idx}. ${r.entry.name} (${r.modelId}) via ${p.label}` +
                ` | ctx=${r.entry.limit.context.toLocaleString()} out=${r.entry.limit.output.toLocaleString()}` +
                ` reasoning=${r.entry.reasoning ? 'yes' : 'no'} complexity=${r.entry.complexity}`);
              choices.push({ finalModel: `${p.provider}:${p.modelId}`, label: p.label });
              idx++;
            }
          }

          if (results.length > 10) {
            console.log(`  ... and ${results.length - 10} more. Refine your search for fewer results.`);
          }

          const { promptUser } = await import('./readline.js');
          const answer = await promptUser(`Enter choice (1-${choices.length}) or press Enter for #1:`);
          const choice = parseInt(answer.trim()) || 1;
          if (choice < 1 || choice > choices.length) {
            console.error('❌ Invalid choice.');
            process.exit(1);
          }
          const selected = choices[choice - 1];
          finalModel = selected.finalModel;
          console.log(`Selected: ${finalModel}`);
        }
      }

      const result = await setModel(process.cwd(), finalModel, slot);
      console.log(result.info);
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

program
  .command('show-model')
  .description('Show the current LLM model')
  .option('--json', 'Output as JSON')
  .action(async (options) => {
    try {
      const result = await showModel(process.cwd());
      if (options.json) {
        console.log(JSON.stringify({ models: result.models, temperatures: result.temperatures }, null, 2));
      } else {
        console.log(result.formatted);
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

program
  .command('list-models')
  .description('List all models in the bundled model database')
  .option('--json', 'Output as JSON')
  .option('--complexity <level>', 'Filter by complexity (high, medium, low)')
  .option('--reasoning', 'Show only reasoning models')
  .action(async (options) => {
    try {
      const { getAllModels } = await import('../system/config/model-database.js');
      const models = getAllModels();
      const entries = Object.entries(models);

      let filtered = entries;
      if (options.complexity) {
        filtered = filtered.filter(([, m]) => m.complexity === options.complexity);
      }
      if (options.reasoning) {
        filtered = filtered.filter(([, m]) => m.reasoning);
      }

      if (options.json) {
        console.log(JSON.stringify(Object.fromEntries(filtered), null, 2));
      } else {
        console.log(`Model database (${filtered.length} models)\n`);
        for (const [id, m] of filtered) {
          const ctx = m.limit.context.toLocaleString();
          const out = m.limit.output.toLocaleString();
          const reasoning = m.reasoning ? 'yes' : 'no';
          const weights = m.open_weights ? 'open' : 'closed';
          console.log(`${id}`);
          console.log(`  ${m.name} | ctx=${ctx} out=${out} reasoning=${reasoning} complexity=${m.complexity} ${weights}`);
          console.log('');
        }
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

program
  .command('image-models')
  .description('List image-generation models for /gen and task(…, media(X))')
  .option('--json', 'Output as JSON')
  .action(async (options) => {
    try {
      const { openPiRuntime } = await import('../repl/backend.js');
      const handle = await openPiRuntime({});
      const runtime = handle.runtime as {
        getModelsOfType?: (type: string) => Array<{ provider?: string; id?: string }>;
        getAvailableOfType?: (type: string) => Promise<Array<{ provider?: string; id?: string }>>;
      };
      const all = runtime.getModelsOfType?.('image') ?? [];
      const available = typeof runtime.getAvailableOfType === 'function'
        ? await runtime.getAvailableOfType('image')
        : [];
      const ready = new Set(available.map((m) => `${m.provider}:${m.id}`));
      const rows = all.map((m) => ({ model: `${m.provider}:${m.id}`, authenticated: ready.has(`${m.provider}:${m.id}`) }));
      if (options.json) {
        console.log(JSON.stringify(rows, null, 2));
      } else if (all.length === 0) {
        console.log('No image-generation models are known. Run `deepclause login` and sign in to a provider that offers image models.');
      } else {
        console.log(`Image models (${available.length} available)\n`);
        for (const row of rows) {
          console.log(`  ${row.model}  [${row.authenticated ? 'ready' : 'needs auth'}]`);
        }
        const first = available[0];
        if (first) {
          console.log(`\nUse one with:  deepclause repl --image-model ${first.provider}:${first.id}`);
          console.log('or set "repl": { "media": { "imageModel": "' + `${first.provider}:${first.id}` + '" } } in .deepclause/config.json');
        }
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

// =============================================================================
// Execution Commands
// =============================================================================

program
  .command('run [file] [args...]')
  .description('Execute a DML file (one-shot)')
  .option('--workspace <path>', 'Working directory for file operations', './')
  .option('--backend <kind>', 'Model backend: pi (default) or env')
  .option('--goal <dml>', 'Run this goal instead of agent_main/N')
  .option('--verbose', 'Show debug output including tool calls')
  .option('--no-stream', 'Hide streamed model text/reasoning (show only outputs and answers)')
  .option('--show-reasoning <mode>', 'How much model reasoning to show: off|summary|full', (value: string) => {
    if (value !== 'off' && value !== 'summary' && value !== 'full') {
      throw new Error(`invalid --show-reasoning: ${value} (use off, summary, or full)`);
    }
    return value;
  })
  .option('--no-events', 'Hide task activity, tool calls, usage, and logs')
  .option('--plain', 'No color/formatting')
  .option('--json', 'Emit events as JSON lines')
  .option('--sandbox', 'Run shell tools inside AgentVM instead of the local workspace shell')
  .option('--trace <file>', 'Save execution trace to file')
  .option('--dry-run', 'Show what would be executed without running')
  .option('--model <model>', 'Override configured model (provider/model)')
  .option('--provider <provider>', 'Override configured provider (openai, anthropic, google, openrouter)')
  .option('--temperature <number>', 'Override temperature (0.0-2.0)', parseFloat)
  .option('--gas-limit <number>', 'Maximum number of execution steps', parseInt)
  .option('--usage <file>', 'Save token usage summary to file')
  .option('-P, --param <key=value>', 'Pass named parameter (can be repeated)', collectParams, {})
  .action(async (file, args, options) => {
    try {
      // Parse provider/model format if provided (e.g., "google/gemini-2.5-pro").
      let model = options.model;
      let provider = options.provider;
      if (model && model.includes('/')) {
        const [parsedProvider, ...modelParts] = model.split('/');
        provider = provider || parsedProvider;
        model = modelParts.join('/');
      }

      const result = await run(file, args, {
        workspace: options.workspace,
        backend: options.backend,
        goal: options.goal,
        verbose: options.verbose,
        stream: options.stream,
        events: options.events,
        showReasoning: options.showReasoning,
        plain: options.plain,
        json: options.json,
        sandbox: options.sandbox,
        trace: options.trace,
        dryRun: options.dryRun,
        model,
        provider: provider as import('../system/config/model-slots.js').Provider,
        temperature: options.temperature,
        gasLimit: options.gasLimit,
        params: options.param,
      });

      if (options.dryRun) {
        console.log(result.wouldExecute);
        return;
      }
      if (result.error) {
        console.error('❌ Error:', result.error);
        process.exit(1);
      }
      if (options.trace && result.trace) {
        console.log(`\n📊 Trace saved to: ${options.trace}`);
      } else if (options.trace) {
        console.log('\n(no execution trace was produced for this run)');
      }
      if (options.usage && result.usageByModel && Object.keys(result.usageByModel).length > 0) {
        const usagePath = path.resolve(options.usage);
        await fs.writeFile(usagePath, JSON.stringify(result.usageByModel, null, 2) + '\n', 'utf8');
        if (options.verbose) {
          console.log(`📊 Usage saved to: ${usagePath}`);
        }
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      if (options.verbose) {
        console.error((error as Error).stack);
      }
      process.exit(1);
    }
  });

// =============================================================================
// REPL Command
// =============================================================================

registerReplCommand(program);
registerVisualizeCommand(program);
registerSetBackendCommand(program);
registerLoginCommands(program);

// =============================================================================
// Helper Functions
// =============================================================================

function collectParams(value: string, previous: Record<string, string>): Record<string, string> {
  const [key, val] = value.split('=');
  if (!key || val === undefined) {
    throw new Error(`Invalid param format: ${value}. Use key=value`);
  }
  return { ...previous, [key]: val };
}

async function main(): Promise<void> {
  const argv = process.argv.slice(2);

  if (requiresInitializedWorkspace(argv) && !await deepClauseDirExists(process.cwd())) {
    console.warn('⚠️  No .deepclause directory found in this workspace.');
    console.warn("   Run 'deepclause init' first.");
    process.exit(1);
  }

  if (argv.length === 0) {
    program.help();
    return;
  }

  await program.parseAsync(process.argv);
}

function requiresInitializedWorkspace(argv: string[]): boolean {
  if (argv.includes('-h') || argv.includes('--help') || argv.includes('-V') || argv.includes('--version')) {
    return false;
  }

  const firstPositional = argv.find((arg) => !arg.startsWith('-'));
  if (!firstPositional) {
    return true;
  }

  return firstPositional !== 'init' && firstPositional !== 'help' && firstPositional !== 'repl';
}

main().catch((error) => {
  console.error('❌ Error:', (error as Error).message);
  process.exit(1);
});
