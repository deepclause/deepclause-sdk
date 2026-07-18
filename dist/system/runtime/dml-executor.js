import * as path from 'path';
import { createDeepClause } from '../../sdk.js';
import { applyResolvedModelConfig, resolveCompactionConfig } from '../../cli/config.js';
import { formatToolArgs } from '../../cli/tool-args.js';
import { recordTokenUsage } from './token-usage.js';
import { createLocalSkillCatalogRuntime, } from './catalog-skills.js';
import { withCapturedConsole } from './console-capture.js';
import { registerLocalRuntimeTools } from './runtime-tools.js';
import { createShellManager } from './shell-manager.js';
export async function executeDml(options) {
    if (options.headless && options.onEvent) {
        return withCapturedConsole((entry) => options.onEvent?.({
            type: 'log',
            content: `[${entry.level}] ${entry.text}`,
        }), () => executeDmlInternal(options));
    }
    return executeDmlInternal(options);
}
async function executeDmlInternal(options) {
    applyResolvedModelConfig(options.selection);
    const resolvedWorkspaceRoot = path.resolve(options.workspaceRoot ?? process.cwd());
    const shell = createShellManager({
        workspacePath: options.workspacePath,
        sandbox: options.sandbox,
        network: options.config.agentvm?.network ?? false,
        hostConfig: options.config.shell,
    });
    const sdk = await createDeepClause({
        model: options.selection.model,
        provider: options.selection.provider,
        apiKey: options.selection.apiKey,
        baseUrl: options.selection.baseUrl,
        temperature: options.selection.temperature,
        trace: !!options.trace,
        streaming: options.stream,
        debug: options.verbose,
        maxTokens: options.selection.maxOutputTokens ?? 65536,
        compaction: resolveCompactionConfig(options.config, resolvedWorkspaceRoot),
        reasoningType: options.selection.reasoningType,
        reasoningBudgetMap: options.selection.reasoningBudgetMap,
        contextWindow: options.selection.contextWindow,
    });
    const result = {
        output: [],
        events: [],
    };
    const usageByModel = {};
    let finished = false;
    const handleEvent = (event) => {
        result.events.push(event);
        options.onEvent?.(event);
        switch (event.type) {
            case 'output':
                if (event.content) {
                    result.output.push(event.content);
                    if (!options.headless) {
                        console.log(event.content);
                    }
                }
                break;
            case 'stream':
                if (options.stream && !options.headless && event.content) {
                    process.stdout.write(event.content);
                }
                if (options.stream && !options.headless && event.done) {
                    process.stdout.write('\n');
                }
                break;
            case 'log':
                if (options.verbose && event.content && !options.headless) {
                    console.log(`[log] ${event.content}`);
                }
                break;
            case 'tool_call':
                if (!options.headless && options.verbose && event.toolName) {
                    console.log(`  🔧 ${event.toolName}(${formatToolArgs(event.toolArgs)})`);
                }
                break;
            case 'answer':
                result.answer = event.content;
                break;
            case 'error':
                result.error = event.content;
                if (event.trace) {
                    result.trace = event.trace;
                }
                finished = true;
                break;
            case 'finished':
                if (event.trace) {
                    result.trace = event.trace;
                }
                finished = true;
                break;
            case 'input_required':
                if (options.verbose && event.prompt && !options.headless) {
                    console.log(`[input_required] ${event.prompt}`);
                }
                break;
            case 'usage':
                if (event.usage) {
                    recordTokenUsage(usageByModel, options.selection.id, event.usage);
                }
                break;
            case 'task_activity':
                break;
        }
    };
    try {
        const skillCatalog = options.skillCatalog
            ? createLocalSkillCatalogRuntime({
                workspaceRoot: options.skillCatalog.workspaceRoot,
                workspacePath: options.workspacePath,
                config: options.config,
                selection: options.selection,
                currentSkillSlug: options.skillCatalog.currentSkillSlug,
                invocationStack: options.skillCatalog.invocationStack,
                maxDepth: options.skillCatalog.maxDepth,
                includeSystemSkillsInList: options.skillCatalog.includeSystemSkillsInList,
                executeNestedSkill: (child) => executeNestedSkill(options, child),
            })
            : undefined;
        registerLocalRuntimeTools(sdk, {
            workspaceRoot: options.workspaceRoot,
            workspacePath: options.workspacePath,
            shell,
            signal: options.signal,
            toolAbortSignalRef: options.toolAbortSignalRef,
            onEvent: handleEvent,
            skillCatalog,
        });
        await options.registerAdditionalTools?.(sdk, {
            config: options.config,
            workspacePath: options.workspacePath,
            selection: options.selection,
            shell,
        });
        const modelInfo = {
            modelId: options.selection.model,
            complexity: options.selection.complexity ?? 'medium',
            reasoning: options.selection.reasoning ?? false,
            contextWindow: options.selection.contextWindow,
            maxOutputTokens: options.selection.maxOutputTokens,
        };
        for await (const event of sdk.runDML(options.dmlCode, {
            args: options.args,
            params: { ...options.params, model_info: modelInfo },
            workspacePath: options.workspacePath,
            gasLimit: options.gasLimit,
            signal: options.signal,
            onUserInput: options.onUserInput,
            initialMessages: options.initialMessages,
        })) {
            if (finished) {
                break;
            }
            handleEvent(event);
        }
        if (Object.keys(usageByModel).length > 0) {
            result.usageByModel = usageByModel;
        }
        return result;
    }
    finally {
        await sdk.dispose();
        await shell.dispose();
    }
}
function executeNestedSkill(parentOptions, child) {
    return executeDml({
        dmlCode: child.dmlCode,
        config: parentOptions.config,
        workspacePath: parentOptions.workspacePath,
        selection: parentOptions.selection,
        args: child.args,
        params: child.params,
        gasLimit: parentOptions.gasLimit,
        stream: parentOptions.stream,
        trace: parentOptions.trace,
        verbose: parentOptions.verbose,
        headless: true,
        sandbox: parentOptions.sandbox,
        signal: parentOptions.signal,
        toolAbortSignalRef: parentOptions.toolAbortSignalRef,
        onUserInput: parentOptions.onUserInput
            ? (prompt) => parentOptions.onUserInput(`[${child.slug}] ${prompt}`)
            : undefined,
        onEvent: parentOptions.skillCatalog?.onChildEvent
            ? (event) => parentOptions.skillCatalog?.onChildEvent?.(child.slug, event)
            : undefined,
        skillCatalog: parentOptions.skillCatalog
            ? {
                workspaceRoot: parentOptions.skillCatalog.workspaceRoot,
                currentSkillSlug: child.currentSkillSlug,
                invocationStack: child.invocationStack,
                maxDepth: parentOptions.skillCatalog.maxDepth,
                includeSystemSkillsInList: parentOptions.skillCatalog.includeSystemSkillsInList,
                onChildEvent: parentOptions.skillCatalog.onChildEvent,
            }
            : undefined,
    });
}
//# sourceMappingURL=dml-executor.js.map