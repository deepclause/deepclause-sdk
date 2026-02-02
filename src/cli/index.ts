#!/usr/bin/env node
/**
 * DeepClause CLI
 * 
 * Command-line interface for compiling Markdown to DML and running DML programs.
 */

import { Command } from 'commander';
import { initConfig, setModel, showModel, loadConfig } from './config.js';
import { compile, compileAll } from './compile.js';
import { run } from './run.js';
import { listTools } from './tools.js';
import { listCommands } from './commands.js';
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
  .description('Compile Markdown to DML and run neurosymbolic AI agents')
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
  .description('Set the default LLM model (format: provider/model or just model)')
  .action(async (model) => {
    try {
      const result = await setModel(process.cwd(), model);
      console.log(`✅ Model set to: ${result.provider}/${result.model}`);
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
        console.log(JSON.stringify({ provider: result.provider, model: result.model }, null, 2));
      } else {
        console.log(result.formatted);
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

// =============================================================================
// Compilation Commands
// =============================================================================

program
  .command('compile <source> [output]')
  .description('Compile Markdown task description to DML')
  .option('-f, --force', 'Force recompilation even if source unchanged')
  .option('--validate-only', 'Validate without saving output')
  .option('--model <model>', 'Override model for compilation')
  .option('--temperature <number>', 'Override temperature (0.0-2.0)', parseFloat)
  .action(async (source, output, options) => {
    try {
      const config = await loadConfig(process.cwd());
      const outputDir = output || '.deepclause/tools';
      
      const result = await compile(source, outputDir, {
        force: options.force,
        validateOnly: options.validateOnly,
        model: options.model || config.model,
        temperature: options.temperature
      });
      
      if (result.skipped) {
        console.log(`⏭️  Skipped (unchanged): ${source}`);
      } else if (options.validateOnly) {
        console.log(`✅ Valid DML generated from: ${source}`);
      } else {
        console.log(`✅ Compiled: ${source} → ${result.output}`);
        console.log(`   Tools: ${result.tools.join(', ') || 'none'}`);
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

program
  .command('compile-all <sourceDir> [outputDir]')
  .description('Compile all Markdown files in a directory')
  .option('-f, --force', 'Force recompilation of all files')
  .option('--model <model>', 'Override model for compilation')
  .action(async (sourceDir, outputDir, options) => {
    try {
      const config = await loadConfig(process.cwd());
      const out = outputDir || '.deepclause/tools';
      
      const result = await compileAll(sourceDir, out, {
        force: options.force,
        model: options.model || config.model
      });
      
      console.log(`\n📊 Compilation Summary:`);
      console.log(`   Compiled: ${result.compiled}`);
      console.log(`   Skipped:  ${result.skipped}`);
      console.log(`   Failed:   ${result.failed}`);
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

// =============================================================================
// Execution Commands
// =============================================================================

program
  .command('run <file> [args...]')
  .description('Execute a compiled DML program')
  .option('--workspace <path>', 'Working directory for file operations', './')
  .option('--verbose', 'Show debug output including tool calls')
  .option('--stream', 'Stream LLM responses in real-time')
  .option('--headless', 'Plain output only, no TUI formatting')
  .option('--trace <file>', 'Save execution trace to file')
  .option('--dry-run', 'Show what would be executed without running')
  .option('--model <model>', 'Override configured model (can be provider/model format, e.g., google/gemini-2.5-pro)')
  .option('--provider <provider>', 'Override configured provider (openai, anthropic, google, openrouter)')
  .option('--temperature <number>', 'Override temperature (0.0-2.0)', parseFloat)
  .option('-p, --param <key=value>', 'Pass named parameter (can be repeated)', collectParams, {})
  .action(async (file, args, options) => {
    try {
      // Parse provider/model format if provided (e.g., "google/gemini-2.5-pro")
      let model = options.model;
      let provider = options.provider;
      if (model && model.includes('/')) {
        const [parsedProvider, ...modelParts] = model.split('/');
        provider = provider || parsedProvider;
        model = modelParts.join('/'); // Handle models with / in name
      }
      
      const result = await run(file, args, {
        workspace: options.workspace,
        verbose: options.verbose,
        stream: options.stream,
        headless: options.headless,
        trace: options.trace,
        dryRun: options.dryRun,
        model,
        provider: provider as import('./config.js').Provider,
        temperature: options.temperature,
        params: options.param
      });
      
      if (options.dryRun) {
        console.log(result.wouldExecute);
      } else {
        // Show outputs (unless in headless mode with verbose already showing them)
        if (!options.verbose && result.output.length > 0) {
          for (const out of result.output) {
            console.log(out);
          }
        }
        
        // Show answer
        if (result.answer) {
          if (result.output.length > 0) {
            console.log(''); // Blank line before answer
          }
          console.log(result.answer);
        }
        
        // Show error
        if (result.error) {
          console.error('❌ Error:', result.error);
          process.exit(1);
        }
        
        // Show trace location if saved
        if (options.trace) {
          console.log(`\n📊 Trace saved to: ${options.trace}`);
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
// Listing Commands
// =============================================================================

program
  .command('list-tools')
  .description('List all available tools from MCP servers and AgentVM')
  .option('--json', 'Output as JSON')
  .action(async (options) => {
    try {
      const tools = await listTools(process.cwd(), { json: options.json });
      
      if (options.json) {
        console.log(tools);
      } else {
        console.log('Available Tools:\n');
        console.log(tools);
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

program
  .command('list-commands')
  .description('List all compiled DML commands')
  .option('--json', 'Output as JSON')
  .option('--detailed', 'Show parameters and tool dependencies')
  .action(async (options) => {
    try {
      const commands = await listCommands(process.cwd(), {
        json: options.json,
        detailed: options.detailed
      });
      
      if (options.json) {
        console.log(JSON.stringify(commands, null, 2));
      } else {
        console.log('Compiled Commands:\n');
        for (const cmd of commands) {
          console.log(`📋 ${cmd.name}`);
          console.log(`   ${cmd.description}`);
          if (options.detailed && cmd.parameters) {
            console.log('   Parameters:');
            for (const p of cmd.parameters) {
              const req = p.required ? '(required)' : `(optional, default: ${p.default})`;
              console.log(`     • ${p.name} ${req} - ${p.description}`);
            }
            if (cmd.tools?.length) {
              console.log('   Tool Dependencies:');
              for (const t of cmd.tools) {
                console.log(`     • ${t}`);
              }
            }
          }
          console.log(`   Usage: deepclause run ${cmd.path}\n`);
        }
      }
    } catch (error) {
      console.error('❌ Error:', (error as Error).message);
      process.exit(1);
    }
  });

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

// Parse and run
program.parse();
