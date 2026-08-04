/**
 * SWI-Prolog WASM Loader
 */
import { readFileSync, existsSync, mkdirSync } from 'fs';
import { fileURLToPath, pathToFileURL } from 'url';
import { dirname, join, resolve } from 'path';
let swiplInstance = null;
let initPromise = null;
let currentWorkspacePath = null;
// Get the directory of this module for resolving Prolog source files
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
// Allow env override for bundled deployments where prolog-src is co-located with the bundle.
// Falls back to the standard SDK layout: ../prolog-src relative to this module.
const PROLOG_SRC_DIR = (() => {
    if (process.env.PROLOG_SRC_DIR && existsSync(process.env.PROLOG_SRC_DIR))
        return process.env.PROLOG_SRC_DIR;
    const sibling = join(__dirname, 'prolog-src');
    if (existsSync(sibling))
        return sibling;
    return join(__dirname, '..', 'prolog-src');
})();
const SWIPL_WASM_ENTRY_URL = (() => {
    const entryPath = join(__dirname, '..', '..', 'vendor', 'swipl-wasm', 'dist', 'index.js');
    return pathToFileURL(entryPath).href;
})();
/**
 * Load and initialize SWI-Prolog WASM
 */
export async function loadProlog() {
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
export function mountWorkspace(swipl, workspacePath) {
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
        }
        catch {
            // Ignore unmount errors
        }
    }
    // Mount the new workspace using NODEFS
    try {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        swipl.FS.mount(swipl.FS.filesystems.NODEFS, { root: absolutePath }, '/workspace');
        currentWorkspacePath = absolutePath;
    }
    catch (error) {
        // If mount fails (maybe already mounted), try to remount
        try {
            swipl.FS.unmount('/workspace');
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            swipl.FS.mount(swipl.FS.filesystems.NODEFS, { root: absolutePath }, '/workspace');
            currentWorkspacePath = absolutePath;
        }
        catch {
            // If still fails, the directory might not exist in WASM FS
            throw new Error(`Failed to mount workspace: ${error}`);
        }
    }
}
/**
 * Get the currently mounted workspace path
 */
export function getWorkspacePath() {
    return currentWorkspacePath;
}
/**
 * Initialize SWI-Prolog WASM
 */
async function initializeProlog() {
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
    });
    // Create required directories
    try {
        swipl.FS.mkdir('/tmp');
    }
    catch {
        // Directory may already exist
    }
    try {
        swipl.FS.mkdir('/workspace');
    }
    catch {
        // Directory may already exist
    }
    // Load the DeepClause Prolog modules
    loadPrologModules(swipl);
    // Pre-load commonly needed libraries so DML skills don't need explicit :- use_module
    // Note: use_module/1 is called as a goal, NOT as a :- directive (directives can't be called from JS)
    try {
        swipl.prolog.call('use_module(library(http/json)).');
    }
    catch {
        // library(http/json) may not be available in all WASM builds — skip silently
    }
    return swipl;
}
/**
 * Load DeepClause Prolog source files from disk
 */
function loadPrologModules(swipl) {
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
//# sourceMappingURL=loader.js.map