interface DmlMetaParameter {
    name: string;
    position: number;
}
interface DmlMetaLike {
    parameters?: DmlMetaParameter[];
}
export declare function buildDmlParams(args: unknown[], namedParams: Record<string, unknown> | undefined, meta: DmlMetaLike | null | undefined): Record<string, unknown>;
export declare function parseDmlArgValue(value: unknown): unknown;
export {};
//# sourceMappingURL=dml-params.d.ts.map