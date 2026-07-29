/**
 * SWI-Prolog WASM Loader
 */

import { readFileSync, existsSync, mkdirSync } from 'fs';
import { fileURLToPath, pathToFileURL } from 'url';
import { dirname, join, resolve } from 'path';

// Query interface matching swipl-wasm
interface Query {
  next(): unknown;
  once(): unknown;
}

// Prolog interface matching swipl-wasm
interface Prolog {
  call(goal: string, opts?: { module?: string; async?: boolean }): unknown;
  query(goal: string, input?: Record<string, unknown>): Query;
  forEach(
    goal: string,
    input?: unknown,
    callback?: (prolog: Prolog, answer: unknown) => void
  ): Promise<unknown>;
}

// Emscripten FS filesystems interface
interface FSFilesystems {
  NODEFS: unknown;
  MEMFS: unknown;
}

interface SWIPLFileSystem {
  filesystems: FSFilesystems;
  mount(type: unknown, options: { root: string }, mountpoint: string): void;
  unmount(mountpoint: string): void;
  mkdir(path: string): void;
  writeFile(path: string, data: string): void;
  unlink(path: string): void;
}

// Type definition for SWI-Prolog WASM module
export interface SWIPLModule {
  prolog: Prolog;
  FS: SWIPLFileSystem;
}

let swiplInstance: SWIPLModule | null = null;
let initPromise: Promise<SWIPLModule> | null = null;
let currentWorkspacePath: string | null = null;

// Get the directory of this module for resolving Prolog source files
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
// Allow env override for bundled deployments where prolog-src is co-located with the bundle.
// Falls back to the standard SDK layout: ../prolog-src relative to this module.
const PROLOG_SRC_DIR = (() => {
  if (process.env.PROLOG_SRC_DIR && existsSync(process.env.PROLOG_SRC_DIR)) return process.env.PROLOG_SRC_DIR;
  const sibling = join(__dirname, 'prolog-src');
  if (existsSync(sibling)) return sibling;
  return join(__dirname, '..', 'prolog-src');
})();

const SWIPL_WASM_ENTRY_URL = (() => {
  const entryPath = join(__dirname, '..', '..', 'vendor', 'swipl-wasm', 'dist', 'index.js');
  return pathToFileURL(entryPath).href;
})();

/**
 * Load and initialize SWI-Prolog WASM
 */
export async function loadProlog(): Promise<SWIPLModule> {
  // Return existing instance if available
  if (swiplInstance) {
    return swiplInstance;
  }

  // If initialization is in progress, wait for it
  if (initPromise) {
    return initPromise;
  }

  // Start initialization
  initPromise = initializeProlog();
  swiplInstance = await initPromise;
  return swiplInstance;
}

/**
 * Mount a workspace directory to /workspace in the WASM filesystem
 * Uses NODEFS for direct host filesystem access
 */
export function mountWorkspace(swipl: SWIPLModule, workspacePath: string): void {
  const absolutePath = resolve(workspacePath);
  
  // Ensure the host directory exists
  if (!existsSync(absolutePath)) {
    mkdirSync(absolutePath, { recursive: true });
  }
  
  // If same workspace is already mounted, skip
  if (currentWorkspacePath === absolutePath) {
    return;
  }
  
  // Unmount previous workspace if any
  if (currentWorkspacePath !== null) {
    try {
      swipl.FS.unmount('/workspace');
    } catch {
      // Ignore unmount errors
    }
  }
  
  // Mount the new workspace using NODEFS
  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    swipl.FS.mount(swipl.FS.filesystems.NODEFS as any, { root: absolutePath }, '/workspace');
    currentWorkspacePath = absolutePath;
  } catch (error) {
    // If mount fails (maybe already mounted), try to remount
    try {
      swipl.FS.unmount('/workspace');
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      swipl.FS.mount(swipl.FS.filesystems.NODEFS as any, { root: absolutePath }, '/workspace');
      currentWorkspacePath = absolutePath;
    } catch {
      // If still fails, the directory might not exist in WASM FS
      throw new Error(`Failed to mount workspace: ${error}`);
    }
  }
}

/**
 * Get the currently mounted workspace path
 */
export function getWorkspacePath(): string | null {
  return currentWorkspacePath;
}

/**
 * Initialize SWI-Prolog WASM
 */
async function initializeProlog(): Promise<SWIPLModule> {
  // Load the vendored CommonJS entry directly so published installs do not depend
  // on npm resolving a local file: dependency at consumer install time.
  const SWIPL = await import(SWIPL_WASM_ENTRY_URL);
  const initSWIPL = SWIPL.default || SWIPL;
  
  // Create WASM instance
  const swipl = await initSWIPL({
    // Configuration options
    arguments: [
      '-g', 'true',
      '--nosignals',
    ],
  }) as SWIPLModule;

  // Create required directories
  try {
    swipl.FS.mkdir('/tmp');
  } catch {
    // Directory may already exist
  }

  try {
    swipl.FS.mkdir('/workspace');
  } catch {
    // Directory may already exist
  }

  // Load the DeepClause Prolog modules
  loadPrologModules(swipl);

  // Pre-load commonly needed libraries so DML skills don't need explicit :- use_module
  // Note: use_module/1 is called as a goal, NOT as a :- directive (directives can't be called from JS)
  try {
    swipl.prolog.call('use_module(library(http/json)).');
  } catch {
    // library(http/json) may not be available in all WASM builds — skip silently
  }

  return swipl;
}

/**
 * Load DeepClause Prolog source files from disk
 */
function loadPrologModules(swipl: SWIPLModule): void {
  // List of Prolog modules to load (order matters for dependencies)
  // Note: deepclause_memory.pl is deprecated - memory is now state-threaded in MI
  const moduleNames = [
    'deepclause_strings.pl',
    'deepclause_mi.pl',
    'deepclause_analysis.pl',
  ];

  for (const name of moduleNames) {
    // Read the source file from disk
    const sourcePath = join(PROLOG_SRC_DIR, name);
    const content = readFileSync(sourcePath, 'utf-8');
    
    // Write to WASM virtual filesystem
    const wasmPath = `/tmp/${name}`;
    swipl.FS.writeFile(wasmPath, content);
    
    // Consult the file
    swipl.prolog.call(`consult('${wasmPath}')`);
  }
}
