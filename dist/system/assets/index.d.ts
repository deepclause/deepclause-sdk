export type SystemSkillAssetName = 'conductor' | 'skill-creator' | 'plan' | 'deep-planner' | 'security-planner';
export type SystemPromptAssetName = 'conductor' | 'skill-creator' | 'task';
export type SystemCompactorAssetName = 'default-session-compactor' | 'default-loop-compactor';
export type WorkspaceDocAssetName = 'tui' | 'dml-reference';
interface SystemAssetLookupOptions {
    workspaceRoot?: string;
    workspacePath?: string;
}
export declare function getSystemSkillAssetPath(name: SystemSkillAssetName): string;
export declare function getSystemPromptAssetPath(name: SystemPromptAssetName): string;
export declare function getSystemCompactorAssetPath(name: SystemCompactorAssetName): string;
export declare function getWorkspaceDocAssetPath(name: WorkspaceDocAssetName): string;
export declare function getPackagedRecipeAssetsDir(): string;
export declare function getWorkspaceRecipeAssetsDir(workspaceRoot: string): string;
export declare function getSystemAssetSourcePaths(workspaceRoot?: string): {
    conductorDml: string;
    conductorPrompt: string;
    planDml: string;
    skillCreatorDml: string;
    skillCreatorPrompt: string;
    taskPrompt: string;
};
export declare function readSystemSkillAsset(name: SystemSkillAssetName, options?: SystemAssetLookupOptions): Promise<string>;
export declare function readSystemPromptAsset(name: SystemPromptAssetName, options?: SystemAssetLookupOptions): Promise<string>;
export {};
//# sourceMappingURL=index.d.ts.map