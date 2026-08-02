import * as fs from 'fs/promises';
import * as path from 'path';
import { listCommands } from '../../cli/commands.js';
import { getToolsDir } from '../../cli/config.js';
import { buildDmlParams } from './dml-params.js';
import { verifyRuntimeToolsAvailable } from './runtime-tools.js';
export const DEFAULT_MAX_SKILL_DEPTH = 3;
export async function listLocalSkillCatalog(workspaceRoot, options = {}) {
    const commands = await listCommands(workspaceRoot, { detailed: options.detailed ?? true });
    return commands
        .filter((command) => options.includeSystemSkills || !isSystemSkillSlug(command.name))
        .map((command) => ({
        slug: command.name,
        name: command.displayName,
        description: command.description,
        usage: buildRunSkillUsage(command.name, command.parameters),
        trigger_phrases: command.triggerPhrases,
        parameters: command.parameters,
        capabilities: command.capabilities,
        tools: command.tools,
        compiled_at: command.compiledAt,
        model: command.model,
    }));
}
export function createLocalSkillCatalogRuntime(options) {
    const stack = getInvocationStack(options.currentSkillSlug, options.invocationStack);
    return {
        listSkills: () => listLocalSkillCatalog(options.workspaceRoot, {
            detailed: true,
            includeSystemSkills: options.includeSystemSkillsInList ?? false,
        }),
        runSkill: async (args) => {
            const slug = String(args.slug ?? '').trim();
            if (!slug) {
                throw new Error('slug is required');
            }
            if (isSystemSkillSlug(slug)) {
                throw new Error(`run_skill cannot invoke system skill "${slug}"`);
            }
            if (stack.includes(slug)) {
                throw new Error(`run_skill cannot create a cycle: ${[...stack, slug].join(' -> ')}`);
            }
            const maxDepth = options.maxDepth ?? DEFAULT_MAX_SKILL_DEPTH;
            if (stack.length >= maxDepth) {
                throw new Error(`run_skill exceeded maximum depth of ${maxDepth}`);
            }
            const childArgs = normalizeRuntimeArgList(args.args);
            const { dmlCode, meta } = await loadSkillProgram(options.workspaceRoot, slug);
            const toolCheck = verifyRuntimeToolsAvailable(options.config, meta?.tools ?? []);
            if (!toolCheck.available) {
                throw new Error(`Missing required tools for skill "${slug}": ${toolCheck.missing.join(', ')}`);
            }
            const result = await options.executeNestedSkill({
                slug,
                dmlCode,
                args: childArgs,
                params: buildDmlParams(childArgs, undefined, meta),
                currentSkillSlug: slug,
                invocationStack: [...stack, slug],
            });
            return {
                success: !result.error,
                slug,
                answer: result.answer,
                output: result.output,
                error: result.error,
            };
        },
    };
}
function getInvocationStack(currentSkillSlug, invocationStack) {
    if (invocationStack && invocationStack.length > 0) {
        return [...invocationStack];
    }
    return currentSkillSlug ? [currentSkillSlug] : [];
}
function isSystemSkillSlug(slug) {
    return slug.startsWith('_');
}
function normalizeRuntimeArgList(value) {
    if (value === undefined || value === null) {
        return [];
    }
    if (Array.isArray(value)) {
        return value;
    }
    if (typeof value === 'string') {
        const trimmed = value.trim();
        if ((trimmed.startsWith('[') && trimmed.endsWith(']'))
            || (trimmed.startsWith('{') && trimmed.endsWith('}'))) {
            try {
                const parsed = JSON.parse(trimmed);
                return Array.isArray(parsed) ? parsed : [parsed];
            }
            catch {
                return [value];
            }
        }
        return [value];
    }
    return [value];
}
async function loadSkillProgram(workspaceRoot, slug) {
    const toolsDir = getToolsDir(workspaceRoot);
    const dmlPath = path.join(toolsDir, `${slug}.dml`);
    const metaPath = path.join(toolsDir, `${slug}.meta.json`);
    let dmlCode;
    try {
        dmlCode = await fs.readFile(dmlPath, 'utf8');
    }
    catch (error) {
        if (error.code === 'ENOENT') {
            throw new Error(`Skill "${slug}" not found in the local catalog`);
        }
        throw error;
    }
    let meta = null;
    try {
        meta = JSON.parse(await fs.readFile(metaPath, 'utf8'));
    }
    catch (error) {
        if (error.code !== 'ENOENT') {
            throw error;
        }
    }
    return { dmlCode, meta };
}
function buildRunSkillUsage(slug, parameters) {
    const args = orderParameters(parameters).map((parameter) => `"${formatRunSkillArgument(parameter)}"`);
    if (args.length === 0) {
        return `run_skill(slug: "${slug}")`;
    }
    return `run_skill(slug: "${slug}", args: [${args.join(', ')}])`;
}
function orderParameters(parameters) {
    if (!parameters || parameters.length === 0) {
        return [];
    }
    return [...parameters]
        .map((parameter, index) => ({ parameter, index }))
        .sort((left, right) => (left.parameter.position ?? left.index) - (right.parameter.position ?? right.index))
        .map(({ parameter }) => parameter);
}
function formatRunSkillArgument(parameter) {
    if (parameter.required === false || parameter.default !== undefined) {
        return parameter.default !== undefined
            ? `[${parameter.name}=${parameter.default}]`
            : `[${parameter.name}]`;
    }
    return `<${parameter.name}>`;
}
//# sourceMappingURL=catalog-skills.js.map