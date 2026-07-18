import type { MetaFile } from './compile.js';
export interface DefaultSkillSeed {
    slug: string;
    dml: string;
    meta: MetaFile;
}
export declare function writeDefaultSkillSeeds(toolsDir: string, modelId: string): Promise<void>;
export declare function ensureDefaultSkillSeeds(toolsDir: string, modelId: string): Promise<string[]>;
export declare function getDefaultSkillSeeds(modelId: string, compiledAt?: string): DefaultSkillSeed[];
//# sourceMappingURL=default-skills.d.ts.map