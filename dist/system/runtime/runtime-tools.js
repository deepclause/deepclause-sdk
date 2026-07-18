import * as fs from 'fs/promises';
import * as path from 'path';
import { newsSearch, webSearch } from '../../cli/search.js';
import { validateWithProlog } from '../../compiler.js';
import { listRecipeCatalog, searchRecipeCatalog } from './catalog-recipes.js';
import { createShellToolEventBridge } from './shell-tool-events.js';
const BUILT_IN_RUNTIME_TOOLS = new Set([
    'vm_exec',
    'bash',
    'consult_recipes',
    'write_file',
    'validate_dml',
    'web_search',
    'news_search',
    'url_fetch',
    'list_skills',
    'run_skill',
]);
const INTERNAL_TOOLS = new Set(['ask_user', 'finish', 'set_result', 'store']);
const DEFAULT_URL_FETCH_MAX_TEXT_CHARS = 20_000;
export function getBuiltInRuntimeToolNames() {
    return [...BUILT_IN_RUNTIME_TOOLS].sort();
}
export function verifyRuntimeToolsAvailable(config, toolNames) {
    const missing = toolNames.filter((name) => {
        if (INTERNAL_TOOLS.has(name)) {
            return false;
        }
        return !BUILT_IN_RUNTIME_TOOLS.has(name);
    });
    if (missing.length === 0) {
        return { available: true, missing: [] };
    }
    if (config.mcp?.servers && Object.keys(config.mcp.servers).length > 0) {
        return { available: true, missing: [] };
    }
    return { available: false, missing };
}
export function registerLocalRuntimeTools(sdk, options) {
    const getExecSignal = () => {
        const toolSignal = options.toolAbortSignalRef?.signal;
        if (toolSignal && options.signal) {
            return AbortSignal.any([options.signal, toolSignal]);
        }
        return toolSignal ?? options.signal;
    };
    sdk.registerTool('web_search', {
        description: 'Search the web for information.',
        parameters: {
            type: 'object',
            properties: {
                query: { type: 'string', description: 'Search query.' },
                count: { type: 'number', description: 'Maximum result count.' },
                freshness: { type: 'string', description: 'Optional freshness filter.' },
            },
            required: ['query'],
        },
        execute: async (args) => webSearch({
            query: String(args.query ?? args.arg1 ?? ''),
            count: typeof args.count === 'number' ? args.count : 10,
            freshness: typeof args.freshness === 'string' ? args.freshness : undefined,
            signal: getExecSignal(),
        }),
    });
    sdk.registerTool('news_search', {
        description: 'Search recent news.',
        parameters: {
            type: 'object',
            properties: {
                query: { type: 'string', description: 'Search query.' },
                count: { type: 'number', description: 'Maximum result count.' },
                freshness: { type: 'string', description: 'Optional freshness filter.' },
            },
            required: ['query'],
        },
        execute: async (args) => newsSearch({
            query: String(args.query ?? args.arg1 ?? ''),
            count: typeof args.count === 'number' ? args.count : 10,
            freshness: typeof args.freshness === 'string' ? args.freshness : undefined,
            signal: getExecSignal(),
        }),
    });
    sdk.registerTool('url_fetch', {
        description: 'Fetch a URL or save it into the workspace.',
        parameters: {
            type: 'object',
            properties: {
                url: { type: 'string', description: 'Absolute URL to fetch.' },
                headers: { type: 'object', description: 'Optional request headers.' },
                save_to: { type: 'string', description: 'Optional workspace-relative output file.' },
            },
            required: ['url'],
        },
        execute: async (args) => urlFetch(options.workspacePath, args, getExecSignal()),
    });
    sdk.registerTool('consult_recipes', {
        description: 'Search the recipe library for workflow guidance, conventions, and reusable approaches.',
        parameters: {
            type: 'object',
            properties: {
                query: { type: 'string', description: 'Natural-language description of the workflow or convention you need.' },
                max_results: { type: 'number', description: 'Optional maximum number of matching recipes to return.' },
            },
            required: ['query'],
        },
        execute: async (args) => consultRecipes({
            workspaceRoot: options.workspaceRoot ?? options.workspacePath,
            query: String(args.query ?? ''),
            maxResults: typeof args.max_results === 'number' ? args.max_results : undefined,
        }),
    });
    sdk.registerTool('bash', {
        description: 'Run a shell command in the active workspace shell.',
        parameters: {
            type: 'object',
            properties: {
                command: { type: 'string', description: 'Shell command to execute.' },
            },
            required: ['command'],
        },
        execute: async (args) => {
            const command = String(args.command ?? args.arg1 ?? '');
            return options.shell.exec(command, getExecSignal(), createShellToolEventBridge({
                toolName: 'bash',
                toolArgs: { command },
                emit: options.onEvent,
            }));
        },
    });
    sdk.registerTool('write_file', {
        description: 'Write or overwrite a file inside the workspace.',
        parameters: {
            type: 'object',
            properties: {
                path: { type: 'string', description: 'Workspace-relative file path.' },
                content: { type: 'string', description: 'Full file content.' },
            },
            required: ['path', 'content'],
        },
        execute: async (args) => writeWorkspaceFile(options.workspacePath, args),
    });
    sdk.registerTool('validate_dml', {
        description: 'Validate DML code from a file path inside the workspace.',
        parameters: {
            type: 'object',
            properties: {
                dml_file: { type: 'string', description: 'Workspace-relative path to the DML file.' },
            },
            required: ['dml_file'],
        },
        execute: async (args) => validateWorkspaceDml(options.workspacePath, args),
    });
    sdk.registerTool('vm_exec', {
        description: 'Execute a shell command using the active workspace shell backend.',
        parameters: {
            type: 'object',
            properties: {
                command: { type: 'string', description: 'Shell command to execute.' },
            },
            required: ['command'],
        },
        execute: async (args) => {
            const command = String(args.command ?? args.arg1 ?? '');
            return options.shell.exec(command, getExecSignal(), createShellToolEventBridge({
                toolName: 'vm_exec',
                toolArgs: { command },
                emit: options.onEvent,
            }));
        },
    });
    if (options.skillCatalog) {
        sdk.registerTool('list_skills', {
            description: 'List available local skills from the current workspace catalog.',
            parameters: {
                type: 'object',
                properties: {},
            },
            execute: async () => options.skillCatalog?.listSkills(),
        });
        sdk.registerTool('run_skill', {
            description: 'Run a local compiled skill from the current workspace catalog.',
            parameters: {
                type: 'object',
                properties: {
                    slug: { type: 'string', description: 'Skill slug from the local tools directory.' },
                    args: { type: 'array', description: 'Optional array of string arguments.' },
                },
                required: ['slug'],
            },
            execute: async (args) => options.skillCatalog?.runSkill(args),
        });
    }
}
export async function urlFetch(workspacePath, args, signal) {
    const url = String(args.url ?? '');
    if (!url) {
        throw new Error('url is required');
    }
    const headers = isStringRecord(args.headers) ? args.headers : undefined;
    const response = await fetch(url, { headers, signal });
    const responseHeaders = Object.fromEntries(response.headers.entries());
    if (typeof args.save_to === 'string' && args.save_to.trim()) {
        const targetPath = resolveWorkspacePath(workspacePath, args.save_to);
        await fs.mkdir(path.dirname(targetPath), { recursive: true });
        const buffer = Buffer.from(await response.arrayBuffer());
        await fs.writeFile(targetPath, buffer);
        return {
            file_path: targetPath,
            size: buffer.byteLength,
            status: response.status,
            headers: responseHeaders,
        };
    }
    const body = await response.text();
    const truncated = truncateUrlFetchTextBody(body);
    return {
        body: truncated.body,
        truncated: truncated.truncated,
        original_length: truncated.originalLength,
        returned_length: truncated.returnedLength,
        status: response.status,
        headers: responseHeaders,
    };
}
export function truncateUrlFetchTextBody(body, maxChars = DEFAULT_URL_FETCH_MAX_TEXT_CHARS) {
    const originalLength = body.length;
    if (originalLength <= maxChars) {
        return {
            body,
            truncated: false,
            originalLength,
            returnedLength: originalLength,
        };
    }
    const notice = `\n... (truncated from ${originalLength} chars to ${maxChars} chars; use save_to to keep the full response)`;
    const clipped = body.slice(0, maxChars) + notice;
    return {
        body: clipped,
        truncated: true,
        originalLength,
        returnedLength: clipped.length,
    };
}
export function resolveWorkspacePath(workspacePath, filePath) {
    if (!filePath) {
        throw new Error('Path is required');
    }
    const root = path.resolve(workspacePath);
    const resolved = path.resolve(root, filePath);
    if (resolved !== root && !resolved.startsWith(root + path.sep)) {
        throw new Error(`Path must stay inside workspace: ${filePath}`);
    }
    return resolved;
}
async function writeWorkspaceFile(workspacePath, args) {
    const relPath = String(args.path ?? '');
    const content = String(args.content ?? '');
    if (!relPath) {
        return { success: false, error: 'path is required' };
    }
    const filePath = resolveWorkspacePath(workspacePath, relPath);
    await fs.mkdir(path.dirname(filePath), { recursive: true });
    await fs.writeFile(filePath, content, 'utf8');
    return { success: true, path: relPath, bytes: content.length };
}
async function validateWorkspaceDml(workspacePath, args) {
    const dmlPath = resolveWorkspacePath(workspacePath, String(args.dml_file ?? ''));
    const dml = await fs.readFile(dmlPath, 'utf8');
    const result = await validateWithProlog(dml);
    return {
        valid: result.valid,
        errors: result.errors,
        warnings: result.warnings ?? [],
    };
}
async function consultRecipes(options) {
    const query = options.query.trim();
    if (!query) {
        throw new Error('query is required');
    }
    const [catalog, matches] = await Promise.all([
        listRecipeCatalog(options.workspaceRoot),
        searchRecipeCatalog(options.workspaceRoot, query, { maxResults: options.maxResults ?? 3 }),
    ]);
    return {
        success: true,
        query,
        total_recipes: catalog.length,
        matches: matches.map((recipe) => ({
            slug: recipe.slug,
            name: recipe.name,
            description: recipe.description,
            tags: recipe.tags,
            when_to_use: recipe.whenToUse,
            when_not_to_use: recipe.whenNotToUse,
            globs: recipe.globs,
            priority: recipe.priority,
            matched_on: recipe.matchedOn,
            score: recipe.score,
            source: recipe.source,
            source_path: recipe.sourcePath,
            content: recipe.content,
        })),
    };
}
function isStringRecord(value) {
    if (!value || typeof value !== 'object' || Array.isArray(value)) {
        return false;
    }
    return Object.values(value).every((entry) => typeof entry === 'string');
}
//# sourceMappingURL=runtime-tools.js.map