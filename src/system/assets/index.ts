import { existsSync } from 'fs';
import { access, readFile } from 'fs/promises';
import { dirname, join, resolve } from 'path';
import { fileURLToPath } from 'url';

export type SystemPromptAssetName = 'task';
export type WorkspaceDocAssetName =
  | 'dml-reference'
  | 'dml-authoring'
  | 'dml-forms'
  | 'harness-examples'
  | 'harness-patterns'
  | 'prolog-snippets';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const DOCS_DIR = join(__dirname, 'docs');
const SYSTEM_OVERRIDE_DIR = '.deepclause/system';

interface SystemAssetLookupOptions {
  workspaceRoot?: string;
  workspacePath?: string;
}

function getSystemPromptFileName(name: SystemPromptAssetName): string {
  switch (name) {
    case 'task':
      return 'TASK_PROMPT.md';
  }
}

export function getSystemPromptAssetPath(name: SystemPromptAssetName): string {
  return join(DOCS_DIR, getSystemPromptFileName(name));
}


function getWorkspaceDocFileName(name: WorkspaceDocAssetName): string {
  switch (name) {
    case 'dml-reference':
      return 'DML_REFERENCE.md';
    case 'dml-authoring':
      return 'DML_AUTHORING.md';
    case 'dml-forms':
      return 'DML_FORMS.md';
    case 'harness-examples':
      return 'HARNESS_EXAMPLES.md';
    case 'harness-patterns':
      return 'HARNESS_PATTERNS.md';
    case 'prolog-snippets':
      return 'PROLOG_SNIPPETS.md';
  }
}

export function getWorkspaceDocAssetPath(name: WorkspaceDocAssetName): string {
  return join(DOCS_DIR, getWorkspaceDocFileName(name));
}

/** Bundled authoring-agent system prompt (`AGENT_PROMPT.md`). */
export function getAgentPromptAssetPath(): string {
  return join(DOCS_DIR, 'AGENT_PROMPT.md');
}

/**
 * Read the authoring-agent system prompt, preferring a workspace override at
 * `.deepclause/system/AGENT_PROMPT.md` over the bundled asset.
 */
export async function readAgentPromptAsset(options: SystemAssetLookupOptions = {}): Promise<string> {
  const overridePath = findWorkspaceOverridePathSync('AGENT_PROMPT.md', options);
  if (overridePath) {
    try {
      return await readFile(overridePath, 'utf8');
    } catch {
      // Fall through to the bundled asset.
    }
  }
  return readFile(getAgentPromptAssetPath(), 'utf8');
}

function getWorkspaceOverridePath(workspaceRoot: string, fileName: string): string {
  return join(workspaceRoot, SYSTEM_OVERRIDE_DIR, fileName);
}

function findWorkspaceOverridePathSync(
  fileName: string,
  options: SystemAssetLookupOptions = {},
): string | null {
  if (options.workspaceRoot) {
    const overridePath = getWorkspaceOverridePath(options.workspaceRoot, fileName);
    return existsSync(overridePath) ? overridePath : null;
  }

  if (options.workspacePath) {
    let current = resolve(options.workspacePath);
    while (true) {
      const overridePath = join(current, SYSTEM_OVERRIDE_DIR, fileName);
      if (existsSync(overridePath)) {
        return overridePath;
      }
      const parent = dirname(current);
      if (parent === current) {
        break;
      }
      current = parent;
    }
  }

  return null;
}

async function resolveSystemPromptAssetPath(
  name: SystemPromptAssetName,
  options: SystemAssetLookupOptions = {},
): Promise<string> {
  const overridePath = findWorkspaceOverridePathSync(getSystemPromptFileName(name), options);
  if (overridePath) {
    try {
      await access(overridePath);
      return overridePath;
    } catch {
      // Fall back to the packaged asset when no workspace override exists.
    }
  }

  return getSystemPromptAssetPath(name);
}

export async function readSystemPromptAsset(
  name: SystemPromptAssetName,
  options: SystemAssetLookupOptions = {},
): Promise<string> {
  return readFile(await resolveSystemPromptAssetPath(name, options), 'utf8');
}
