type RecipePriority = 'low' | 'normal' | 'high';
export interface RecipeCatalogEntry {
    slug: string;
    name: string;
    description: string;
    tags: string[];
    whenToUse: string[];
    whenNotToUse: string[];
    globs: string[];
    priority: RecipePriority;
    content: string;
    sourcePath: string;
    source: 'packaged' | 'workspace';
}
export interface RecipeCatalogMatch extends RecipeCatalogEntry {
    score: number;
    matchedOn: string[];
}
export declare function listRecipeCatalog(workspaceRoot: string): Promise<RecipeCatalogEntry[]>;
export declare function searchRecipeCatalog(workspaceRoot: string, query: string, options?: {
    maxResults?: number;
}): Promise<RecipeCatalogMatch[]>;
export {};
//# sourceMappingURL=catalog-recipes.d.ts.map