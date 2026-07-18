import { type Parameter } from '../../cli/commands.js';
import { type Config, type ResolvedModelConfig } from '../../cli/config.js';
export declare const DEFAULT_MAX_SKILL_DEPTH = 3;
export interface LocalSkillCatalogEntry {
    slug: string;
    name?: string;
    description: string;
    usage?: string;
    trigger_phrases?: string[];
    parameters?: Parameter[];
    capabilities?: string[];
    tools?: string[];
    compiled_at?: string;
    model?: string;
}
export interface ExecuteNestedSkillRequest {
    slug: string;
    dmlCode: string;
    args: unknown[];
    params: Record<string, unknown>;
    currentSkillSlug: string;
    invocationStack: string[];
}
export interface ExecuteNestedSkillResult {
    output: string[];
    answer?: string;
    error?: string;
    trace?: object;
}
export interface LocalSkillCatalogRuntime {
    listSkills(): Promise<LocalSkillCatalogEntry[]>;
    runSkill(args: Record<string, unknown>): Promise<Record<string, unknown>>;
}
export interface LocalSkillCatalogRuntimeOptions {
    workspaceRoot: string;
    workspacePath: string;
    config: Config;
    selection: ResolvedModelConfig;
    currentSkillSlug?: string;
    invocationStack?: string[];
    maxDepth?: number;
    includeSystemSkillsInList?: boolean;
    executeNestedSkill(request: ExecuteNestedSkillRequest): Promise<ExecuteNestedSkillResult>;
}
export declare function listLocalSkillCatalog(workspaceRoot: string, options?: {
    detailed?: boolean;
    includeSystemSkills?: boolean;
}): Promise<LocalSkillCatalogEntry[]>;
export declare function createLocalSkillCatalogRuntime(options: LocalSkillCatalogRuntimeOptions): LocalSkillCatalogRuntime;
//# sourceMappingURL=catalog-skills.d.ts.map