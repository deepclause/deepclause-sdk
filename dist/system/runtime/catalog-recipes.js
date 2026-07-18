import * as fs from 'fs/promises';
import * as path from 'path';
import { getPackagedRecipeAssetsDir, getWorkspaceRecipeAssetsDir, } from '../assets/index.js';
export async function listRecipeCatalog(workspaceRoot) {
    const packaged = await readRecipeEntries(getPackagedRecipeAssetsDir(), 'packaged');
    const workspace = await readRecipeEntries(getWorkspaceRecipeAssetsDir(workspaceRoot), 'workspace');
    const bySlug = new Map();
    for (const entry of packaged) {
        bySlug.set(entry.slug, entry);
    }
    for (const entry of workspace) {
        bySlug.set(entry.slug, entry);
    }
    return [...bySlug.values()].sort((left, right) => left.slug.localeCompare(right.slug));
}
export async function searchRecipeCatalog(workspaceRoot, query, options = {}) {
    const trimmedQuery = query.trim();
    const recipes = await listRecipeCatalog(workspaceRoot);
    if (!trimmedQuery) {
        return [];
    }
    const tokens = tokenize(trimmedQuery);
    const matches = recipes
        .map((recipe) => scoreRecipe(recipe, trimmedQuery, tokens))
        .filter((recipe) => recipe !== null)
        .sort((left, right) => {
        if (right.score !== left.score) {
            return right.score - left.score;
        }
        return left.slug.localeCompare(right.slug);
    });
    return matches.slice(0, Math.max(1, options.maxResults ?? 3));
}
async function readRecipeEntries(recipesDir, source) {
    let entries;
    try {
        entries = await fs.readdir(recipesDir, { withFileTypes: true });
    }
    catch (error) {
        if (error.code === 'ENOENT') {
            return [];
        }
        throw error;
    }
    const recipes = await Promise.all(entries
        .filter((entry) => entry.isDirectory())
        .map(async (entry) => loadRecipeEntry(path.join(recipesDir, entry.name), entry.name, source)));
    return recipes.filter((entry) => entry !== null);
}
async function loadRecipeEntry(recipeDir, slug, source) {
    const skillPath = await findRecipeSkillPath(recipeDir);
    if (!skillPath) {
        return null;
    }
    const rawMarkdown = await fs.readFile(skillPath, 'utf8');
    const { frontmatter, body } = parseRecipeMarkdown(rawMarkdown);
    return {
        slug,
        name: frontmatter.name?.trim() || humanizeSlug(slug),
        description: frontmatter.description?.trim() || inferDescription(body),
        tags: normalizeStringList(frontmatter.tags),
        whenToUse: normalizeStringList(frontmatter.when_to_use),
        whenNotToUse: normalizeStringList(frontmatter.when_not_to_use),
        globs: normalizeStringList(frontmatter.globs),
        priority: normalizePriority(frontmatter.priority),
        content: body.trim(),
        sourcePath: skillPath,
        source,
    };
}
async function findRecipeSkillPath(recipeDir) {
    let entries;
    try {
        entries = await fs.readdir(recipeDir);
    }
    catch (error) {
        if (error.code === 'ENOENT') {
            return null;
        }
        throw error;
    }
    const skillFile = entries.find((entry) => /^skill\.md$/i.test(entry));
    return skillFile ? path.join(recipeDir, skillFile) : null;
}
function parseRecipeMarkdown(markdown) {
    const lines = markdown.split(/\r?\n/);
    if (lines[0]?.trim() !== '---') {
        return { frontmatter: {}, body: markdown };
    }
    const frontmatterLines = [];
    let closingIndex = -1;
    for (let index = 1; index < lines.length; index += 1) {
        if (lines[index].trim() === '---') {
            closingIndex = index;
            break;
        }
        frontmatterLines.push(lines[index]);
    }
    if (closingIndex === -1) {
        return { frontmatter: {}, body: markdown };
    }
    return {
        frontmatter: parseFrontmatter(frontmatterLines),
        body: lines.slice(closingIndex + 1).join('\n'),
    };
}
function parseFrontmatter(lines) {
    const frontmatter = {};
    let currentListKey = null;
    for (const line of lines) {
        const trimmed = line.trim();
        if (!trimmed || trimmed.startsWith('#')) {
            continue;
        }
        const listStartMatch = line.match(/^\s*([a-zA-Z0-9_]+):\s*$/);
        if (listStartMatch) {
            currentListKey = listStartMatch[1];
            frontmatter[currentListKey] = [];
            continue;
        }
        const inlineArrayMatch = line.match(/^\s*([a-zA-Z0-9_]+):\s*\[(.*)\]\s*$/);
        if (inlineArrayMatch) {
            currentListKey = null;
            frontmatter[inlineArrayMatch[1]] = inlineArrayMatch[2]
                .split(',')
                .map((item) => stripQuotes(item.trim()))
                .filter((item) => item.length > 0);
            continue;
        }
        const scalarMatch = line.match(/^\s*([a-zA-Z0-9_]+):\s*(.+?)\s*$/);
        if (scalarMatch) {
            currentListKey = null;
            frontmatter[scalarMatch[1]] = stripQuotes(scalarMatch[2]);
            continue;
        }
        const listItemMatch = line.match(/^\s*-\s*(.+?)\s*$/);
        if (listItemMatch && currentListKey) {
            const current = frontmatter[currentListKey];
            if (Array.isArray(current)) {
                current.push(stripQuotes(listItemMatch[1]));
            }
        }
    }
    return frontmatter;
}
function stripQuotes(value) {
    if ((value.startsWith('"') && value.endsWith('"')) || (value.startsWith("'") && value.endsWith("'"))) {
        return value.slice(1, -1);
    }
    return value;
}
function normalizeStringList(value) {
    if (!value || value.length === 0) {
        return [];
    }
    return Array.from(new Set(value
        .map((item) => item.trim())
        .filter((item) => item.length > 0)));
}
function normalizePriority(value) {
    switch ((value ?? '').trim().toLowerCase()) {
        case 'high':
            return 'high';
        case 'low':
            return 'low';
        default:
            return 'normal';
    }
}
function inferDescription(markdown) {
    const lines = markdown.split(/\r?\n/)
        .map((line) => line.trim())
        .filter((line) => line.length > 0 && !line.startsWith('#'));
    return lines[0] ?? 'No description provided.';
}
function humanizeSlug(slug) {
    return slug
        .split(/[-_]+/)
        .filter((part) => part.length > 0)
        .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
        .join(' ');
}
function tokenize(value) {
    return Array.from(new Set(value
        .toLowerCase()
        .split(/[^a-z0-9]+/)
        .map((token) => token.trim())
        .filter((token) => token.length >= 3)));
}
function scoreRecipe(recipe, query, tokens) {
    const fields = {
        slug: recipe.slug.toLowerCase(),
        name: recipe.name.toLowerCase(),
        description: recipe.description.toLowerCase(),
        tags: recipe.tags.join(' ').toLowerCase(),
        whenToUse: recipe.whenToUse.join(' ').toLowerCase(),
        whenNotToUse: recipe.whenNotToUse.join(' ').toLowerCase(),
        content: recipe.content.toLowerCase(),
    };
    const normalizedQuery = query.toLowerCase();
    let score = priorityBonus(recipe.priority);
    const matchedOn = new Set();
    const strongMatchFields = new Set();
    if (fields.slug.includes(normalizedQuery)) {
        score += 40;
        matchedOn.add('slug');
        strongMatchFields.add('slug');
    }
    if (fields.name.includes(normalizedQuery)) {
        score += 36;
        matchedOn.add('name');
        strongMatchFields.add('name');
    }
    if (fields.description.includes(normalizedQuery)) {
        score += 28;
        matchedOn.add('description');
        strongMatchFields.add('description');
    }
    for (const token of tokens) {
        if (fields.slug.includes(token)) {
            score += 12;
            matchedOn.add('slug');
            strongMatchFields.add('slug');
        }
        if (fields.name.includes(token)) {
            score += 10;
            matchedOn.add('name');
            strongMatchFields.add('name');
        }
        if (fields.description.includes(token)) {
            score += 8;
            matchedOn.add('description');
            strongMatchFields.add('description');
        }
        if (fields.tags.includes(token)) {
            score += 7;
            matchedOn.add('tags');
            strongMatchFields.add('tags');
        }
        if (fields.whenToUse.includes(token)) {
            score += 6;
            matchedOn.add('when_to_use');
            strongMatchFields.add('when_to_use');
        }
        if (fields.whenNotToUse.includes(token)) {
            score += 3;
            matchedOn.add('when_not_to_use');
        }
        if (fields.content.includes(token)) {
            score += 2;
            matchedOn.add('content');
        }
    }
    if (strongMatchFields.size === 0 || score <= 0) {
        return null;
    }
    return {
        ...recipe,
        score,
        matchedOn: [...matchedOn],
    };
}
function priorityBonus(priority) {
    switch (priority) {
        case 'high':
            return 3;
        case 'normal':
            return 1;
        case 'low':
            return 0;
    }
}
//# sourceMappingURL=catalog-recipes.js.map