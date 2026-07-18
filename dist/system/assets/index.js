import { existsSync } from 'fs';
import { access, readFile } from 'fs/promises';
import { dirname, join, resolve } from 'path';
import { fileURLToPath } from 'url';
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const SKILLS_DIR = join(__dirname, 'skills');
const COMPACTORS_DIR = join(__dirname, 'compactors');
const DOCS_DIR = join(__dirname, 'docs');
const RECIPES_DIR = join(__dirname, 'recipes');
const SYSTEM_OVERRIDE_DIR = '.deepclause/system';
function getSystemSkillFileName(name) {
    switch (name) {
        case 'conductor':
            return 'conductor.dml';
        case 'skill-creator':
            return 'skill-creator.dml';
        case 'plan':
            return 'plan.dml';
        case 'deep-planner':
            return 'deep-planner.dml';
        case 'security-planner':
            return 'security-planner.dml';
    }
}
function getSystemPromptFileName(name) {
    switch (name) {
        case 'conductor':
            return 'CONDUCTOR_PROMPT.md';
        case 'skill-creator':
            return 'DML_COMPILER_PROMPT.md';
        case 'task':
            return 'TASK_PROMPT.md';
    }
}
function getSystemCompactorFileName(name) {
    switch (name) {
        case 'default-session-compactor':
            return 'default-session-compactor.dml';
        case 'default-loop-compactor':
            return 'default-loop-compactor.dml';
    }
}
export function getSystemSkillAssetPath(name) {
    return join(SKILLS_DIR, getSystemSkillFileName(name));
}
export function getSystemPromptAssetPath(name) {
    return join(DOCS_DIR, getSystemPromptFileName(name));
}
export function getSystemCompactorAssetPath(name) {
    return join(COMPACTORS_DIR, getSystemCompactorFileName(name));
}
function getWorkspaceDocFileName(name) {
    switch (name) {
        case 'tui':
            return 'TUI.md';
        case 'dml-reference':
            return 'DML_REFERENCE.md';
    }
}
export function getWorkspaceDocAssetPath(name) {
    return join(DOCS_DIR, getWorkspaceDocFileName(name));
}
export function getPackagedRecipeAssetsDir() {
    return RECIPES_DIR;
}
export function getWorkspaceRecipeAssetsDir(workspaceRoot) {
    return join(workspaceRoot, SYSTEM_OVERRIDE_DIR, 'recipes');
}
function getWorkspaceOverridePath(workspaceRoot, fileName) {
    return join(workspaceRoot, SYSTEM_OVERRIDE_DIR, fileName);
}
function findWorkspaceOverridePathSync(fileName, options = {}) {
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
async function resolveSystemSkillAssetPath(name, options = {}) {
    const overridePath = findWorkspaceOverridePathSync(getSystemSkillFileName(name), options);
    if (overridePath) {
        try {
            await access(overridePath);
            return overridePath;
        }
        catch {
            // Fall back to the packaged asset when no workspace override exists.
        }
    }
    return getSystemSkillAssetPath(name);
}
async function resolveSystemPromptAssetPath(name, options = {}) {
    const overridePath = findWorkspaceOverridePathSync(getSystemPromptFileName(name), options);
    if (overridePath) {
        try {
            await access(overridePath);
            return overridePath;
        }
        catch {
            // Fall back to the packaged asset when no workspace override exists.
        }
    }
    return getSystemPromptAssetPath(name);
}
function resolveSystemSkillAssetPathSync(name, workspaceRoot) {
    if (workspaceRoot) {
        const overridePath = getWorkspaceOverridePath(workspaceRoot, getSystemSkillFileName(name));
        if (existsSync(overridePath)) {
            return overridePath;
        }
    }
    return getSystemSkillAssetPath(name);
}
function resolveSystemPromptAssetPathSync(name, workspaceRoot) {
    if (workspaceRoot) {
        const overridePath = getWorkspaceOverridePath(workspaceRoot, getSystemPromptFileName(name));
        if (existsSync(overridePath)) {
            return overridePath;
        }
    }
    return getSystemPromptAssetPath(name);
}
export function getSystemAssetSourcePaths(workspaceRoot) {
    return {
        conductorDml: resolveSystemSkillAssetPathSync('conductor', workspaceRoot),
        conductorPrompt: resolveSystemPromptAssetPathSync('conductor', workspaceRoot),
        planDml: resolveSystemSkillAssetPathSync('plan', workspaceRoot),
        skillCreatorDml: resolveSystemSkillAssetPathSync('skill-creator', workspaceRoot),
        skillCreatorPrompt: resolveSystemPromptAssetPathSync('skill-creator', workspaceRoot),
        taskPrompt: resolveSystemPromptAssetPathSync('task', workspaceRoot),
    };
}
export async function readSystemSkillAsset(name, options = {}) {
    return readFile(await resolveSystemSkillAssetPath(name, options), 'utf8');
}
export async function readSystemPromptAsset(name, options = {}) {
    return readFile(await resolveSystemPromptAssetPath(name, options), 'utf8');
}
//# sourceMappingURL=index.js.map