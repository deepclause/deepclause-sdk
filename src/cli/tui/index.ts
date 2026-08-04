/**
 * DeepClause CLI - Terminal UI Module (Ink-based v2)
 *
 * This is the Ink (React for CLIs) rewrite of the TUI.
 * Activated via the `--tui=v2` flag; the original monolithic TUI
 * remains the default in `../tui.ts`.
 */

import React from 'react';
import { render } from 'ink';
import { App } from './app.js';

export interface TUIOptions {
  headless?: boolean;
  verbose?: boolean;
}

/**
 * Start the Ink-based TUI (v2).
 */
export async function startTuiV2(
  workspaceRoot = process.cwd(),
  options: { sandbox?: boolean } = {},
): Promise<void> {
  const { waitUntilExit } = render(
    React.createElement(App, { workspaceRoot, sandbox: options.sandbox }),
  );
  await waitUntilExit();
}

/**
 * Render execution progress in TUI
 */
export function renderExecution(_options: TUIOptions): void {
  // Placeholder — will be wired to Ink components
}

/**
 * Render compilation progress in TUI
 */
export function renderCompilation(_options: TUIOptions): void {
  // Placeholder — will be wired to Ink components
}

/**
 * Check if TUI should be used
 */
export function shouldUseTUI(options: TUIOptions): boolean {
  if (options.headless) return false;
  if (!process.stdout.isTTY) return false;
  return true;
}
