const DEFAULT_DML_COMPACTOR_TIMEOUT_MS = 15_000;
export function resolveCompactionOptions(base, override) {
    const enabled = override?.enabled ?? base?.enabled ?? false;
    if (!enabled) {
        return null;
    }
    const bindings = [
        ...(base?.bindings ?? []),
        ...(override?.bindings ?? []),
    ].map(resolveBinding);
    if (bindings.length === 0) {
        return null;
    }
    return {
        enabled: true,
        bindings,
    };
}
export function resolveBinding(binding) {
    return {
        name: binding.name,
        scope: binding.scope,
        trigger: binding.trigger,
        compactor: resolveCompactorDefinition(binding.compactor),
    };
}
export function resolveCompactorDefinition(definition) {
    return {
        source: definition.source,
        sourceType: definition.sourceType ?? 'auto',
        timeoutMs: Math.max(0, definition.timeoutMs ?? DEFAULT_DML_COMPACTOR_TIMEOUT_MS),
        gasLimit: definition.gasLimit,
        model: definition.model,
        provider: definition.provider,
        inheritTools: definition.inheritTools ?? false,
        toolPolicy: definition.toolPolicy,
    };
}
export function getCompactionBindings(options, scope, trigger) {
    if (!options) {
        return [];
    }
    return options.bindings.filter((binding) => binding.scope === scope && binding.trigger === trigger);
}
export function estimateTokensForText(text) {
    return Math.max(1, Math.ceil(text.length / 4));
}
const PER_MESSAGE_OVERHEAD = 4;
export function estimateTokensForMessages(messages) {
    return messages.reduce((total, message) => total + PER_MESSAGE_OVERHEAD + estimateTokensForText(message.content), 0);
}
export function detectProviderFromModel(model) {
    const lower = model.toLowerCase();
    if (lower.includes('gpt') || lower.includes('o1') || lower.includes('o3')) {
        return 'openai';
    }
    if (lower.includes('claude')) {
        return 'anthropic';
    }
    if (lower.includes('gemini') || lower.includes('palm')) {
        return 'google';
    }
    return 'openrouter';
}
export function resolveCompactorModelConfig(params) {
    const model = params.binding.compactor.model ?? params.selection.model;
    const provider = params.binding.compactor.provider
        ?? (params.binding.compactor.model ? detectProviderFromModel(model) : params.selection.provider);
    const providerConfig = provider === params.selection.provider
        ? {
            apiKey: params.selection.apiKey,
            baseUrl: params.baseUrl ?? params.selection.baseUrl,
        }
        : {
            apiKey: params.providerConfigs?.[provider]?.apiKey,
            baseUrl: params.baseUrl ?? params.providerConfigs?.[provider]?.baseUrl,
        };
    return {
        model,
        modelId: params.binding.compactor.model ?? params.selection.id,
        provider,
        apiKey: providerConfig.apiKey,
        baseUrl: providerConfig.baseUrl,
        temperature: params.selection.temperature,
        maxOutputTokens: params.selection.maxOutputTokens,
    };
}
export function buildCompactorParams(binding, messages, knownInputTokens, maxContextTokens) {
    return {
        compact_scope: binding.scope,
        compact_trigger: binding.trigger,
        compact_binding_name: binding.name ?? getBindingLabel(binding),
        message_count: messages.length,
        estimated_tokens: knownInputTokens ?? estimateTokensForMessages(messages),
        max_context_tokens: maxContextTokens ?? 0,
        messages_json: JSON.stringify(messages),
    };
}
export async function executeCompactor(params) {
    const estimatedTokens = params.knownInputTokens ?? estimateTokensForMessages(params.messages);
    const beforeTokens = estimatedTokens;
    if (params.messages.length === 0) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'skipped',
                beforeTokens,
                afterTokens: beforeTokens,
            }),
        };
    }
    const request = {
        binding: params.binding,
        messages: params.messages,
        params: buildCompactorParams(params.binding, params.messages, params.knownInputTokens, params.maxContextTokens),
    };
    params.emitEvent?.(buildCompactionEvent({
        binding: params.binding,
        action: 'running',
        beforeTokens,
    }));
    let response;
    try {
        response = await params.execute(request);
    }
    catch (error) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'failed',
                beforeTokens,
                afterTokens: beforeTokens,
                error: error instanceof Error ? error.message : String(error),
            }),
        };
    }
    if (response.error) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'failed',
                beforeTokens,
                afterTokens: beforeTokens,
                error: response.error,
            }),
            usageByModel: response.usageByModel,
        };
    }
    const parsed = parseCompactorAnswer(response.answer ?? '');
    if (!parsed) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'failed',
                beforeTokens,
                afterTokens: beforeTokens,
                error: 'Compactor returned an unreadable response',
            }),
            usageByModel: response.usageByModel,
        };
    }
    if (!parsed.apply) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'skipped',
                beforeTokens,
                afterTokens: beforeTokens,
            }),
            usageByModel: response.usageByModel,
        };
    }
    const rewrittenMessages = !parsed.messages && parsed.rewrite
        ? applyCompactorRewrite(params.binding, params.messages, parsed.rewrite)
        : parsed.messages;
    if (!rewrittenMessages) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'failed',
                beforeTokens,
                afterTokens: beforeTokens,
                error: 'Compactor applied but did not return messages',
            }),
            usageByModel: response.usageByModel,
        };
    }
    const validationError = validateMessageArray(rewrittenMessages);
    if (validationError) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'failed',
                beforeTokens,
                afterTokens: beforeTokens,
                error: validationError,
            }),
            usageByModel: response.usageByModel,
        };
    }
    const afterTokens = estimateTokensForMessages(rewrittenMessages);
    if (afterTokens >= beforeTokens) {
        return {
            messages: params.messages,
            applied: false,
            event: buildCompactionEvent({
                binding: params.binding,
                action: 'skipped',
                beforeTokens,
                afterTokens,
                error: 'Compactor did not reduce message size',
            }),
            usageByModel: response.usageByModel,
        };
    }
    return {
        messages: rewrittenMessages,
        applied: true,
        event: buildCompactionEvent({
            binding: params.binding,
            action: 'applied',
            beforeTokens,
            afterTokens,
        }),
        usageByModel: response.usageByModel,
    };
}
export function parseCompactorAnswer(answer) {
    const trimmed = answer.trim();
    if (!trimmed) {
        return null;
    }
    const rewriteSpec = parseCompactorRewriteSpec(trimmed);
    if (rewriteSpec) {
        return rewriteSpec;
    }
    if (trimmed === 'no_op' || trimmed === 'skip') {
        return { apply: false };
    }
    const parsed = unwrapCompactorJson(tryParseCompactorJson(trimmed));
    if (!parsed || typeof parsed !== 'object' || parsed === null) {
        return null;
    }
    const record = parsed;
    const action = typeof record.action === 'string' ? record.action : undefined;
    if (action === 'no_op' || action === 'skip' || action === 'noop') {
        return { apply: false };
    }
    const apply = typeof record.apply === 'boolean'
        ? record.apply
        : Boolean(record.messages
            ?? record.messages_out
            ?? record.memory
            ?? record.memory_out
            ?? record.summary
            ?? record.compacted_summary);
    if (!apply) {
        return { apply: false };
    }
    const rawMessages = record.messages ?? record.messages_out ?? record.memory ?? record.memory_out;
    const messages = normalizeMessageArray(rawMessages);
    if (messages) {
        return {
            apply: true,
            messages,
        };
    }
    const rewrite = parseCompactorRewriteRecord(record);
    if (!rewrite) {
        return null;
    }
    return {
        apply: true,
        rewrite,
    };
}
export function applyCompactorRewrite(binding, messages, rewrite) {
    const summary = rewrite.summary.trim();
    if (!summary) {
        return null;
    }
    const keepLastMessages = clampKeepLastMessages(rewrite.keepLastMessages);
    const summaryMessage = {
        role: 'assistant',
        content: summary,
    };
    if (binding.scope === 'session') {
        return [
            summaryMessage,
            ...messages.slice(-keepLastMessages),
        ];
    }
    const systemMessages = messages.filter((message) => message.role === 'system');
    const conversationalMessages = messages.filter((message) => message.role !== 'system');
    return [
        ...systemMessages,
        summaryMessage,
        ...conversationalMessages.slice(-keepLastMessages),
    ];
}
export function normalizeMessageArray(value) {
    if (!Array.isArray(value)) {
        return null;
    }
    const messages = [];
    for (const item of value) {
        if (!item || typeof item !== 'object') {
            return null;
        }
        const record = item;
        const role = record.role;
        const content = record.content;
        if ((role !== 'system' && role !== 'user' && role !== 'assistant') || typeof content !== 'string') {
            return null;
        }
        messages.push({ role, content });
    }
    return messages;
}
export function validateMessageArray(messages) {
    for (const message of messages) {
        if (message.role !== 'system' && message.role !== 'user' && message.role !== 'assistant') {
            return `Invalid message role: ${String(message.role)}`;
        }
        if (typeof message.content !== 'string') {
            return 'Message content must be a string';
        }
    }
    return null;
}
export function buildCompactionEvent(params) {
    return {
        type: 'memory_compaction',
        content: formatCompactionEventContent(params),
        compactionScope: params.binding.scope,
        compactionTrigger: params.binding.trigger,
        compactionAction: params.action,
        compactionBindingName: params.binding.name,
        beforeTokens: params.beforeTokens,
        afterTokens: params.afterTokens,
        compactionError: params.error,
    };
}
export function formatCompactionEventContent(params) {
    const label = params.binding.name ?? getBindingLabel(params.binding);
    const errorSuffix = params.error ? ` (${params.error})` : '';
    if (params.action === 'running') {
        return `compact ${params.binding.scope}.${params.binding.trigger} running ${label} ${params.beforeTokens} tokens${errorSuffix}`;
    }
    return `compact ${params.binding.scope}.${params.binding.trigger} ${params.action} ${label} ${params.beforeTokens} -> ${params.afterTokens ?? params.beforeTokens} tokens${errorSuffix}`;
}
export function getBindingLabel(binding) {
    return `${binding.scope}:${binding.trigger}`;
}
function tryParseJson(value) {
    try {
        return JSON.parse(value);
    }
    catch {
        return null;
    }
}
function tryParseCompactorJson(value) {
    const direct = tryParseJson(value);
    if (direct !== null) {
        return direct;
    }
    const fenced = extractFencedJson(value);
    if (!fenced) {
        return null;
    }
    return tryParseJson(fenced);
}
function extractFencedJson(value) {
    const match = value.match(/```(?:json)?\s*([\s\S]*?)\s*```/i);
    return match?.[1]?.trim() || null;
}
function unwrapCompactorJson(value) {
    if (typeof value !== 'string') {
        return value ?? null;
    }
    return tryParseJson(value);
}
function parseCompactorRewriteRecord(record) {
    const summary = typeof record.summary === 'string'
        ? record.summary
        : typeof record.compacted_summary === 'string'
            ? record.compacted_summary
            : null;
    if (!summary) {
        return null;
    }
    const keepLastMessages = parseKeepLastMessages(record.keep_last_messages
        ?? record.keepLastMessages
        ?? record.tail_messages
        ?? record.tailMessages);
    if (keepLastMessages === null) {
        return null;
    }
    return {
        keepLastMessages,
        summary,
    };
}
function parseCompactorRewriteSpec(answer) {
    if (!answer.startsWith('DC_COMPACTOR_REWRITE_V1\n')) {
        return null;
    }
    const withoutPrefix = answer.slice('DC_COMPACTOR_REWRITE_V1\n'.length);
    const summaryMarker = '\nsummary:\n';
    const summaryIndex = withoutPrefix.indexOf(summaryMarker);
    if (summaryIndex === -1) {
        return null;
    }
    const header = withoutPrefix.slice(0, summaryIndex);
    const summary = withoutPrefix.slice(summaryIndex + summaryMarker.length);
    const headerLines = header.split('\n').map((line) => line.trim()).filter(Boolean);
    const applyLine = headerLines.find((line) => line.startsWith('apply='));
    const keepLine = headerLines.find((line) => line.startsWith('keep_last_messages='));
    if (!applyLine || !keepLine) {
        return null;
    }
    const applyValue = applyLine.slice('apply='.length).trim();
    if (applyValue === 'false') {
        return { apply: false };
    }
    if (applyValue !== 'true') {
        return null;
    }
    const keepLastMessages = parseKeepLastMessages(keepLine.slice('keep_last_messages='.length).trim());
    if (keepLastMessages === null) {
        return null;
    }
    return {
        apply: true,
        rewrite: {
            keepLastMessages,
            summary,
        },
    };
}
function parseKeepLastMessages(value) {
    if (typeof value === 'number' && Number.isInteger(value)) {
        return value;
    }
    if (typeof value === 'string') {
        const parsed = Number.parseInt(value, 10);
        if (Number.isInteger(parsed)) {
            return parsed;
        }
    }
    return null;
}
function clampKeepLastMessages(value) {
    return Math.max(0, Math.min(8, value));
}
//# sourceMappingURL=compaction.js.map