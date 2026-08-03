/**
 * Context pane component — displays token usage and cost information per model.
 */

import type { Component, RequestRender } from '../types.js';
import { style, ANSI, truncate } from '../util/ansi.js';

export interface TokenUsage {
  input: number;
  output: number;
}

export class Context implements Component {
  dirty = true;
  minHeight = 1;
  flexGrow = 1;

  private requestRenderFn: RequestRender;
  private tokenUsage: Record<string, TokenUsage> = {};
  private contextTokens = 0;

  constructor(requestRender: RequestRender) {
    this.requestRenderFn = requestRender;
  }

  /** Set token usage data. */
  setTokenUsage(usage: Record<string, TokenUsage>): void {
    this.tokenUsage = usage;
    this.invalidate();
  }

  /** Set the approximate number of tokens currently held in session context. */
  setContextTokens(tokens: number): void {
    this.contextTokens = tokens;
    this.invalidate();
  }

  invalidate(): void {
    this.dirty = true;
    this.requestRenderFn();
  }

  render(width: number): string[] {
    const rows: string[] = [];

    rows.push(style('Session Context', ANSI.bold, ANSI.yellow));
    rows.push(`  ~${formatTokenCount(this.contextTokens)} tokens`);
    rows.push('');
    rows.push(style('Model Usage', ANSI.bold, ANSI.yellow));

    const models = Object.entries(this.tokenUsage);
    if (models.length === 0) {
      rows.push(style('  No usage data.', ANSI.dim));
    } else {
      for (const [model, usage] of models) {
        rows.push(style(truncate(`  ${model}`, width), ANSI.cyan));
        const info = `    in: ${formatTokenCount(usage.input)} | out: ${formatTokenCount(usage.output)}`;
        rows.push(style(truncate(info, width), ANSI.dim));
      }
    }

    this.dirty = false;
    return rows;
  }
}

function formatTokenCount(count: number): string {
  if (count >= 1_000_000) return `${(count / 1_000_000).toFixed(1)}M`;
  if (count >= 1_000) return `${(count / 1_000).toFixed(1)}k`;
  return String(count);
}
