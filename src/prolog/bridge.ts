/**
 * JavaScript-Prolog Bridge utilities
 */

import { google } from '@ai-sdk/google';
import { anthropic } from '@ai-sdk/anthropic';
import { createOpenAI } from '@ai-sdk/openai';
import type { LanguageModelV1 } from 'ai';

/**
 * Create a model provider for the Vercel AI SDK
 */
export function createModelProvider(
  provider: string,
  model: string,
  baseUrl?: string
): LanguageModelV1 {
  switch (provider) {
    case 'google':
      return google(model);

    case 'anthropic':
      return anthropic(model);

    case 'openai': {
      const openai = createOpenAI({
        apiKey: process.env.OPENAI_API_KEY,
        baseURL: baseUrl,
      });
      return openai(model);
    }

    case 'openrouter': {
      const openrouter = createOpenAI({
        name: 'openrouter',
        baseURL: 'https://openrouter.ai/api/v1',
        apiKey: process.env.OPENROUTER_API_KEY,
      });
      return openrouter(model);
    }

    default: {
      // Default to OpenAI-compatible
      const defaultProvider = createOpenAI({
        apiKey: process.env.OPENAI_API_KEY,
        baseURL: baseUrl,
      });
      return defaultProvider(model);
    }
  }
}

/**
 * Convert a JavaScript value to a Prolog term string
 */
export function jsToPrologTerm(value: unknown): string {
  if (value === null || value === undefined) {
    return 'null';
  }

  if (typeof value === 'string') {
    // Escape special characters and wrap in double quotes
    const escaped = value
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r')
      .replace(/\t/g, '\\t');
    return `"${escaped}"`;
  }

  if (typeof value === 'number') {
    return String(value);
  }

  if (typeof value === 'boolean') {
    return value ? 'true' : 'false';
  }

  if (Array.isArray(value)) {
    const items = value.map(jsToPrologTerm).join(', ');
    return `[${items}]`;
  }

  if (typeof value === 'object') {
    const entries = Object.entries(value)
      .map(([k, v]) => `${sanitizeAtom(k)}: ${jsToPrologTerm(v)}`)
      .join(', ');
    return `dict{${entries}}`;
  }

  return String(value);
}

/**
 * Convert a Prolog term to a JavaScript value
 */
export function prologTermToJs(term: unknown): unknown {
  if (term === null || term === undefined) {
    return null;
  }

  // Handle Prolog atoms/strings
  if (typeof term === 'string') {
    // Try to parse as JSON first
    try {
      return JSON.parse(term);
    } catch {
      return term;
    }
  }

  // Numbers pass through
  if (typeof term === 'number') {
    return term;
  }

  // Booleans pass through
  if (typeof term === 'boolean') {
    return term;
  }

  // Handle arrays (Prolog lists)
  if (Array.isArray(term)) {
    return term.map(prologTermToJs);
  }

  // Handle objects (Prolog dicts or structures)
  if (typeof term === 'object') {
    const result: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(term)) {
      // Skip the 'functor' property that represents the dict tag
      if (key !== 'functor' && key !== '_') {
        result[key] = prologTermToJs(value);
      }
    }
    return result;
  }

  return term;
}

/**
 * Sanitize a string to be a valid Prolog atom
 */
function sanitizeAtom(str: string): string {
  // If it starts with lowercase and contains only valid chars, it's already valid
  if (/^[a-z][a-zA-Z0-9_]*$/.test(str)) {
    return str;
  }

  // Otherwise, quote it
  const escaped = str.replace(/'/g, "''");
  return `'${escaped}'`;
}

/**
 * Parse a Prolog term string into arguments
 */
export function parsePrologArgs(termStr: string): unknown[] {
  // Simple parser for common cases
  const match = termStr.match(/\((.+)\)$/s);
  if (!match) {
    return [];
  }

  const content = match[1];
  const args: unknown[] = [];
  
  let current = '';
  let depth = 0;
  let inString = false;
  let stringChar = '';
  let escaped = false;

  for (const char of content) {
    if (escaped) {
      current += char;
      escaped = false;
      continue;
    }

    if (char === '\\') {
      current += char;
      escaped = true;
      continue;
    }

    if (!inString) {
      if (char === '"' || char === "'") {
        inString = true;
        stringChar = char;
        current += char;
      } else if ('([{'.includes(char)) {
        depth++;
        current += char;
      } else if (')]}'.includes(char)) {
        depth--;
        current += char;
      } else if (char === ',' && depth === 0) {
        args.push(parseArgValue(current.trim()));
        current = '';
      } else {
        current += char;
      }
    } else {
      current += char;
      if (char === stringChar) {
        inString = false;
      }
    }
  }

  if (current.trim()) {
    args.push(parseArgValue(current.trim()));
  }

  return args;
}

/**
 * Parse a single argument value
 */
function parseArgValue(str: string): unknown {
  // Quoted string
  if ((str.startsWith('"') && str.endsWith('"')) ||
      (str.startsWith("'") && str.endsWith("'"))) {
    return str.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\r/g, '\r')
      .replace(/\\t/g, '\t')
      .replace(/\\"/g, '"')
      .replace(/\\'/g, "'")
      .replace(/\\\\/g, '\\');
  }

  // Number
  const num = Number(str);
  if (!isNaN(num) && str !== '') {
    return num;
  }

  // Boolean atoms
  if (str === 'true') return true;
  if (str === 'false') return false;

  // Null
  if (str === 'null' || str === '[]') return null;

  // List (simplified - just return as string for now)
  if (str.startsWith('[')) {
    return str;
  }

  // Atom or variable
  return str;
}
