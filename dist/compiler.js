/**
 * DeepClause SDK - Compilation Module
 *
 * Compiles Markdown task descriptions to DML programs using an agentic loop
 * with LLM generation and Prolog validation.
 */
import { generateText } from 'ai';
import { openai, createOpenAI } from '@ai-sdk/openai';
import { anthropic } from '@ai-sdk/anthropic';
import { google } from '@ai-sdk/google';
import { buildCompilationPrompt, buildUserMessage } from './compiler_prompt.js';
import { loadProlog } from './prolog/loader.js';
import { recordTokenUsage } from './system/runtime/token-usage.js';
// =============================================================================
// Prolog Validation
// =============================================================================
let prologInstance = null;
/**
 * Get or initialize Prolog instance for validation
 */
async function getProlog() {
    if (!prologInstance) {
        prologInstance = await loadProlog();
    }
    return prologInstance;
}
/**
 * Validate DML code using the actual Prolog parser
 */
export async function validateWithProlog(dml) {
    const errors = [];
    const warnings = [];
    try {
        const swipl = await getProlog();
        // Write code to temp file
        const tempPath = `/tmp/validate_${Date.now()}_${Math.random().toString(36).substring(7)}.pl`;
        swipl.FS.writeFile(tempPath, dml);
        // Try to parse the file using read_term which will catch syntax errors
        try {
            swipl.prolog.query(`catch(
          (
            open('${tempPath}', read, Stream, []),
            call_cleanup(
              read_all_terms(Stream),
              close(Stream)
            )
          ),
          Error,
          (
            Error = error(syntax_error(Message), _) -> 
              true 
            ; 
              (term_to_atom(Error, Message), true)
          )
        ), 
        read_all_terms(S) :- 
          repeat,
          read_term(S, T, [syntax_errors(error)]),
          (T == end_of_file -> ! ; fail)`);
            // Simple approach: try to consult the file
            try {
                swipl.prolog.call(`consult('${tempPath}')`);
            }
            catch (consultError) {
                const errMsg = consultError instanceof Error ? consultError.message : String(consultError);
                if (errMsg.includes('syntax') || errMsg.includes('error')) {
                    // Extract meaningful error message
                    const match = errMsg.match(/(?:syntax_error|error)\s*\(\s*['"]?([^'")\]]+)/i);
                    if (match) {
                        errors.push(`Syntax error: ${match[1]}`);
                    }
                    else {
                        errors.push(`Parse error: ${errMsg.substring(0, 200)}`);
                    }
                }
            }
        }
        catch (e) {
            const errMsg = e instanceof Error ? e.message : String(e);
            if (errMsg.includes('syntax') || errMsg.includes('error')) {
                errors.push(errMsg.substring(0, 200));
            }
        }
        // Check for required elements
        if (!dml.includes('agent_main')) {
            errors.push('Missing agent_main predicate - every DML program must define agent_main');
        }
        // Check for balanced delimiters
        const delimiterCheck = checkBalancedDelimiters(dml);
        errors.push(...delimiterCheck.errors);
        // Check for common mistakes
        const lintResult = lintDML(dml);
        errors.push(...lintResult.errors);
        warnings.push(...lintResult.warnings);
        // Cleanup temp file
        try {
            swipl.FS.unlink(tempPath);
        }
        catch {
            // Ignore cleanup errors
        }
    }
    catch (error) {
        // If Prolog itself fails, fall back to basic validation
        return validateDMLSyntaxBasic(dml);
    }
    return {
        valid: errors.length === 0,
        errors,
        warnings: warnings.length > 0 ? warnings : undefined
    };
}
/**
 * Check balanced delimiters
 */
function checkBalancedDelimiters(dml) {
    const errors = [];
    // Remove strings and comments for delimiter checking
    const stripped = dml
        .replace(/"(?:[^"\\]|\\.)*"/g, '""') // Remove string contents
        .replace(/%.*$/gm, '') // Remove line comments
        .replace(/\/\*[\s\S]*?\*\//g, ''); // Remove block comments
    let parenCount = 0;
    let bracketCount = 0;
    let braceCount = 0;
    for (const char of stripped) {
        if (char === '(')
            parenCount++;
        if (char === ')')
            parenCount--;
        if (char === '[')
            bracketCount++;
        if (char === ']')
            bracketCount--;
        if (char === '{')
            braceCount++;
        if (char === '}')
            braceCount--;
        if (parenCount < 0) {
            errors.push('Unbalanced parentheses: extra closing )');
            break;
        }
        if (bracketCount < 0) {
            errors.push('Unbalanced brackets: extra closing ]');
            break;
        }
        if (braceCount < 0) {
            errors.push('Unbalanced braces: extra closing }');
            break;
        }
    }
    if (parenCount > 0)
        errors.push(`Unbalanced parentheses: ${parenCount} unclosed (`);
    if (bracketCount > 0)
        errors.push(`Unbalanced brackets: ${bracketCount} unclosed [`);
    if (braceCount > 0)
        errors.push(`Unbalanced braces: ${braceCount} unclosed {`);
    return { errors };
}
/**
 * Extract all string literals from DML code (both single and double quoted)
 */
function extractStringLiterals(dml) {
    const doubleQuoted = [];
    const singleQuoted = [];
    // Double-quoted strings
    for (const m of dml.matchAll(/"((?:[^"\\]|\\.)*)"/g)) {
        doubleQuoted.push(m[1]);
    }
    // Single-quoted atoms
    for (const m of dml.matchAll(/'((?:[^'\\]|\\.)*)'/g)) {
        singleQuoted.push(m[1]);
    }
    return { doubleQuoted, singleQuoted };
}
/**
 * Strip comments and string contents for structural analysis
 */
function stripForAnalysis(dml) {
    return dml
        .replace(/%.*$/gm, '') // line comments
        .replace(/\/\*[\s\S]*?\*\//g, '') // block comments
        .replace(/"(?:[^"\\]|\\.)*"/g, '""') // double-quoted string contents
        .replace(/'(?:[^'\\]|\\.)*'/g, "''"); // single-quoted atom contents
}
/**
 * Lint DML for common issues — focused on string handling and structural errors.
 * Returns errors (will crash at runtime) and warnings (likely wrong).
 */
function lintDML(dml) {
    const errors = [];
    const warnings = [];
    const stripped = stripForAnalysis(dml);
    const { doubleQuoted, singleQuoted } = extractStringLiterals(dml);
    const allStrings = [...doubleQuoted, ...singleQuoted];
    // ═══════════════════════════════════════════════════════════════════════════
    // STRING HANDLING ERRORS — these WILL crash or produce wrong output
    // ═══════════════════════════════════════════════════════════════════════════
    // E1: format() passed directly to output() or answer()
    //     format/3 binds its first arg, it does NOT return a value.
    //     output(format("...", [...])) is always wrong.
    if (/output\s*\(\s*format\s*\(/.test(dml)) {
        errors.push('ERROR: format/3 does not return a value. `output(format(...))` will crash. Fix: format(string(S), "...", [...]), output(S).');
    }
    if (/answer\s*\(\s*format\s*\(/.test(dml)) {
        errors.push('ERROR: format/3 does not return a value. `answer(format(...))` will crash. Fix: format(string(S), "...", [...]), answer(S).');
    }
    // E2: ~Variable in task/prompt description strings (only ~w, ~d, ~a, ~s work with format)
    //     task("Analyze ~Topic", R) does NOT interpolate. Use {Topic}.
    for (const s of doubleQuoted) {
        if (/~[A-Z][a-zA-Z0-9_]*/.test(s)) {
            errors.push(`ERROR: ~Variable interpolation does not work in DML. Found in: "...${s.substring(0, 60)}...". Use {Variable} syntax instead (e.g. {Topic} not ~Topic).`);
            break;
        }
    }
    // E3: {Variable} inside format/3 template string
    //     format(string(S), "Hello {Name}", []) — the {Name} won't be interpolated by format.
    //     This is a style-mixing bug.
    const formatCalls = dml.matchAll(/format\s*\(\s*(?:string|atom)\s*\([^)]*\)\s*,\s*"([^"]*)"/g);
    for (const m of formatCalls) {
        const tmpl = m[1];
        if (/\{[A-Z][a-zA-Z0-9_]*\}/.test(tmpl)) {
            errors.push(`ERROR: {Variable} inside format() template does nothing — format uses ~w placeholders. Found: "${tmpl.substring(0, 60)}". Use EITHER {Variable} in task() strings OR ~w in format() strings, never both in the same string.`);
            break;
        }
    }
    // E4: String concatenation with + operator
    //     "hello" + "world" or Var + Var — Prolog's + is arithmetic only.
    if (/"\s*\+\s*"/.test(dml) || /"\s*\+\s*[A-Z]/.test(dml) || /[A-Z][a-zA-Z_0-9]*\s*\+\s*"/.test(dml)) {
        errors.push('ERROR: The + operator is arithmetic only in Prolog, not string concatenation. Use {Variable} interpolation in task() strings, or atom_concat/3, or format/3 for building strings.');
    }
    // E5: task() with non-string first arg — task(llm(...)) or task(prompt(...))
    if (/task\s*\(\s*(?:llm|prompt)\s*\(/.test(stripped)) {
        errors.push('ERROR: task() takes a plain string as first argument. task(llm(...)) and task(prompt(...)) are wrong. Use: task("description string", Var).');
    }
    // E6: Hallucinated predicates — these do NOT exist in DML runtime
    //     But skip if user defines the predicate themselves (pred(...) :- ...).
    const hallucinated = [
        ['json_parse', 'json_parse/2 does not exist. Use task() to extract data from JSON via LLM.'],
        ['http_get', 'http_get does not exist. Use exec(url_fetch(url: URL), R).'],
        ['string_format', 'string_format does not exist. Use format(string(R), FormatStr, Args).'],
        ['get_field', 'get_field does not exist. Use get_dict(Key, Dict, Value) — note: Key comes first.'],
        ['string_join', 'string_join does not exist. Use atomic_list_concat(List, Sep, Atom).'],
        ['string_lower', 'string_lower does not exist. Use downcase_atom(Atom, Lower).'],
        ['string_trim', 'string_trim does not exist. Use normalize_space(string(Out), In).'],
        ['contains', 'contains/2 does not exist. Use sub_atom/5 or sub_string/5.'],
    ];
    for (const [name, msg] of hallucinated) {
        const usePattern = new RegExp(`(?<!\\w)${name}\\s*\\(`, 'g');
        if (usePattern.test(stripped)) {
            // Check if user defines this predicate (name(...) :- or name(...) .)
            const defPattern = new RegExp(`^${name}\\s*\\([^)]*\\)\\s*(?::-|\\.)`, 'm');
            if (!defPattern.test(stripped)) {
                errors.push(`ERROR: ${msg}`);
            }
        }
    }
    // E7: url_fetch used as a bare predicate (not wrapped in exec)
    //     url_fetch("url", R) is wrong. Must be exec(url_fetch(url: "..."), R).
    if (/(?<!exec\s*\(\s*)url_fetch\s*\(\s*"/.test(stripped)) {
        errors.push('ERROR: url_fetch is not a predicate. Must use exec/2 wrapper: exec(url_fetch(url: URL), R).');
    }
    // E8: curl/wget in bash command strings
    for (const s of allStrings) {
        if (/\b(?:curl|wget)\s/.test(s)) {
            errors.push('ERROR: curl and wget are banned — the VM has no direct internet. Use exec(url_fetch(url: URL), R) to fetch web content.');
            break;
        }
    }
    // E9: writeln() used for user output — not a DML predicate
    if (/(?<!\w)writeln\s*\(/.test(stripped)) {
        // Check it's not inside an open/close file I/O block (write to stream is OK)
        // Simple heuristic: if there's no open() call, writeln is definitely wrong
        if (!/\bopen\s*\(/.test(stripped) || /writeln\s*\(\s*['"]/.test(dml)) {
            errors.push('ERROR: writeln() is not a DML output predicate. Use output() for progress messages and answer() for final results.');
        }
    }
    // ═══════════════════════════════════════════════════════════════════════════
    // STRING HANDLING WARNINGS — likely wrong, should fix
    // ═══════════════════════════════════════════════════════════════════════════
    // W1: Mixing {Variable} interpolation and format/3 in the same clause
    //     Not always wrong (different strings), but a code smell with small models
    const hasInterpolation = /\{[A-Z][a-zA-Z0-9_]*\}/.test(dml);
    const hasFormat = /format\s*\(/.test(stripped);
    if (hasInterpolation && hasFormat) {
        warnings.push('WARNING: Code mixes {Variable} interpolation and format/3. This is allowed but can confuse the runtime if mixed in the same string. Ensure {Variable} is only in task()/output()/answer() strings and ~w is only in format() strings.');
    }
    // W2: No tool definitions — model probably wrote pure Prolog
    if (!/\btool\s*\(/.test(dml)) {
        warnings.push('WARNING: No tool() definitions found. This is fine for pure Prolog or direct exec()-only skills, but LLM-driven skills usually need tool()/3 wrappers for task() calls.');
    }
    // W3: No task() call — model probably wrote pure Prolog logic
    if (!/\btask\s*\(/.test(stripped)) {
        warnings.push('WARNING: No task() call found. This is fine for deterministic or simple skills. Skills that need open-ended reasoning, summarization, extraction, or classification should usually use task().');
    }
    // W4: No system() call — LLM has no instructions
    if (!/\bsystem\s*\(/.test(stripped)) {
        warnings.push('WARNING: No system() call found. Add system() when the skill uses task()/prompt() and the LLM needs instructions on how to use tools.');
    }
    // W5: No answer() call — skill completes silently
    if (!/\banswer\s*\(/.test(stripped)) {
        warnings.push('WARNING: No answer() call found. The skill will complete without returning a result to the user.');
    }
    // W6: No output() before task/exec — skill appears frozen
    if (/\btask\s*\(/.test(stripped) && !/\boutput\s*\(/.test(stripped)) {
        warnings.push('WARNING: No output() calls found. Add output() before long-running task() or exec() calls so the skill doesn\'t appear frozen.');
    }
    // W7: Single agent_main clause — no fallback
    const mainMatches = stripped.match(/agent_main\s*[\(\.]/g);
    if (mainMatches && mainMatches.length === 1) {
        warnings.push('WARNING: Only one agent_main clause. Add a fallback clause with a static error message (no LLM calls) as required by Rule 7.');
    }
    // W8: Dict dot notation — Result.field etc.
    //     Only check outside strings — match Var.key pattern in code
    if (/[A-Z][a-zA-Z_0-9]*\.[a-z][a-z_]+(?!\s*\()/.test(stripped)) {
        warnings.push('WARNING: Possible dict dot notation (e.g. Result.stdout). This may not work — use get_dict(Key, Dict, Value) instead.');
    }
    return { errors, warnings };
}
/**
 * Basic syntax validation (fallback when Prolog fails)
 */
function validateDMLSyntaxBasic(dml) {
    const errors = [];
    if (!dml.includes('agent_main')) {
        errors.push('Missing agent_main predicate');
    }
    const delimiterCheck = checkBalancedDelimiters(dml);
    errors.push(...delimiterCheck.errors);
    // Check for unclosed strings
    const stringPattern = /"(?:[^"\\]|\\.)*"/g;
    const cleanedDml = dml.replace(stringPattern, '""');
    if ((cleanedDml.match(/"/g) || []).length % 2 !== 0) {
        errors.push('Unclosed string literal');
    }
    return {
        valid: errors.length === 0,
        errors: [...new Set(errors)]
    };
}
// =============================================================================
// Analysis & Auditing
// =============================================================================
/**
 * Run static analysis on the DML code
 */
export async function analyzeDML(dml) {
    const swipl = await getProlog();
    const tempPath = `/tmp/analysis_${Date.now()}_${Math.random().toString(36).substring(7)}.dml`;
    try {
        swipl.FS.writeFile(tempPath, dml);
        // Query Prolog to read file and analyze
        const query = `
      read_file_to_string('${tempPath}', Code, []),
      deepclause_analysis:analyze_source(Code, Result).
    `;
        const result = await swipl.prolog.query(query).once();
        // @ts-ignore
        const rawAnalysis = result.Result;
        // Convert Prolog result to AnalysisResult
        // Prolog dicts come back as objects with keys
        const warnings = [];
        if (rawAnalysis.warnings && Array.isArray(rawAnalysis.warnings)) {
            for (const w of rawAnalysis.warnings) {
                let level = 'low';
                let message = 'Unknown warning';
                // Handle swipl-wasm compound term structure: { functor: 'name', name: [[args]] }
                let args = [];
                if (w.args) {
                    args = w.args;
                }
                else if (w.functor && w[w.functor] && Array.isArray(w[w.functor])) {
                    // It seems arguments are wrapped in a nested array: [ [Arg1, Arg2] ]
                    const inner = w[w.functor];
                    args = Array.isArray(inner[0]) ? inner[0] : inner;
                }
                if (args.length >= 2) {
                    level = args[0];
                    const msgObj = args[1];
                    // Handle PrologString object { v: "text" }
                    message = (typeof msgObj === 'object' && msgObj && 'v' in msgObj) ? msgObj.v : String(msgObj);
                }
                warnings.push({
                    level: level,
                    message
                });
            }
        }
        const capabilities = [];
        if (rawAnalysis.capabilities && Array.isArray(rawAnalysis.capabilities)) {
            for (const cap of rawAnalysis.capabilities) {
                if (typeof cap === 'string') {
                    capabilities.push(cap);
                }
                else if (typeof cap === 'object' && cap) {
                    // Handle compound terms like tool_use(name)
                    const functor = cap.functor;
                    if (functor) {
                        let args = [];
                        const inner = cap[functor];
                        if (Array.isArray(inner) && Array.isArray(inner[0])) {
                            args = inner[0];
                        }
                        if (functor === 'tool_use' && args.length > 0) {
                            capabilities.push(`tool_use(${args[0]})`);
                        }
                        else {
                            capabilities.push(`${functor}(${args.join(', ')})`);
                        }
                    }
                    else {
                        capabilities.push(String(cap));
                    }
                }
            }
        }
        return {
            valid: rawAnalysis.valid === true || rawAnalysis.valid === 'true',
            warnings,
            capabilities,
            auditorReport: undefined,
            error: rawAnalysis.error ? String(rawAnalysis.error) : undefined
        };
    }
    catch (e) {
        console.warn('Analysis failed:', e);
        return {
            valid: false, // Fail if analysis throws (e.g. timeout or prolog error)
            warnings: [{ level: 'low', message: `Analysis failed: ${e}` }],
            capabilities: [],
            error: String(e)
        };
    }
    finally {
        try {
            swipl.FS.unlink(tempPath);
        }
        catch { }
    }
}
/**
 * Run LLM-based security audit
 */
export async function runLLMSecurityAudit(dml, staticAnalysis, model, provider, baseUrl, usageByModel) {
    const llm = getLanguageModel(provider, model, baseUrl);
    const prompt = `
You are a senior security engineer auditing a DeepClause agent (DML).
DML is a Prolog dialect for controlling LLMs.

RUNTIME CONSTRAINTS:
- Only recommend mitigations that are actually supported by the current DeepClause runtime or standard SWI-Prolog predicates used here.
- Do NOT recommend nonexistent helpers or framework features such as sanitize_for_prompt/2, structured message envelopes for task()/prompt(), or built-in ask_user rate limiting.
- task()/prompt() take a plain string as their first argument. If you suggest prompt-injection mitigations, prefer prompt() for fresh context, narrower prompt text, explicit quoting/fencing of untrusted data, and minimizing tainted interpolation into system()/user().
- Positional agent_main arguments arrive as runtime strings by default. If you suggest type checks, prefer must_be(string, Arg) first, then explicit parsing/conversion.
- Safe concrete suggestions in this codebase include must_be/2, simple string/atom guards, explicit empty-input checks, output-shape checks written in Prolog, atom_json_dict/3, get_dict/3, prompt(), and using gas limits or explicit counters for retry loops.
- If an idea would require adding a new runtime helper or framework feature, label it clearly as "not currently built in" instead of presenting it as already available.
- Do not invent warnings or remediation steps when the static findings do not support them.

CODE:
\`\`\`prolog
${dml}
\`\`\`

STATIC ANALYSIS FINDINGS:
${staticAnalysis.warnings.map(w => `- [${w.level.toUpperCase()}] ${w.message}`).join('\n')}

DETECTED CAPABILITIES:
${staticAnalysis.capabilities.join(', ')}

TASK:
Review the code for logic flaws, security risks, and bad practices.
Focus on:
1. Prompt Injection: Is user input properly isolated?
2. Command Injection: Are tool arguments validated?
3. Logic: Does the flow make sense?
4. Tool Misuse: Are dangerous tools used appropriately?

OUTPUT:
Provide a concise Markdown report with:
- Critical Issues (if any)
- Warnings
- Suggestions for improvement

Each suggestion must be either:
- A supported mitigation that exists in this runtime, or
- Explicitly labeled as requiring new runtime support.
`;
    try {
        const result = await generateText({
            model: llm,
            prompt,
            temperature: 0.1
        });
        if (result.usage && usageByModel) {
            recordTokenUsage(usageByModel, model, {
                inputTokens: result.usage.inputTokens ?? 0,
                outputTokens: result.usage.outputTokens ?? 0,
                totalTokens: result.usage.totalTokens ?? 0,
            });
        }
        return result.text;
    }
    catch (e) {
        return `Audit failed: ${e}`;
    }
}
export async function analyzeAndAuditDML(dml, options) {
    const analysis = await analyzeDML(dml);
    if (options.audit) {
        analysis.auditorReport = await runLLMSecurityAudit(dml, analysis, options.model, options.provider, options.baseUrl, options.usageByModel);
    }
    return analysis;
}
// =============================================================================
// Main Compilation Function
// =============================================================================
/**
 * Compile a natural language prompt or markdown directly to DML
 */
export async function compileToDML(source, options) {
    const model = options.model || 'gpt-4o';
    const provider = options.provider || 'openai';
    const maxAttempts = options.maxAttempts ?? 3;
    const temperature = options.temperature ?? 0.3;
    const tools = options.tools || [];
    const baseUrl = options.baseUrl;
    // Build the compilation prompt
    const systemPrompt = buildCompilationPrompt(tools);
    const userMessage = buildUserMessage(source);
    const attempts = [];
    let lastDml = "";
    let lastValidation = { valid: false, errors: [] };
    const usageByModel = {};
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
            const dml = await generateDML(systemPrompt, userMessage, attempts, model, provider, temperature, baseUrl, usageByModel);
            lastDml = dml;
            const validation = await validateWithProlog(dml);
            lastValidation = validation;
            attempts.push({ dml, validation });
            if (validation.valid) {
                // Run Static Analysis
                const analysis = await analyzeAndAuditDML(dml, {
                    audit: options.audit,
                    model,
                    provider,
                    baseUrl,
                    usageByModel,
                });
                // Generate explanation
                const explanation = await generateExplanation(dml, model, provider, baseUrl, usageByModel);
                return {
                    dml,
                    tools: extractToolDependencies(dml),
                    explanation,
                    attempts: attempt,
                    valid: true,
                    analysis,
                    usageByModel: Object.keys(usageByModel).length > 0 ? usageByModel : undefined,
                };
            }
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            attempts.push({
                dml: lastDml,
                validation: { valid: false, errors: [message] }
            });
        }
    }
    const allErrors = new Set(lastValidation.errors);
    for (const attempt of attempts) {
        for (const err of attempt.validation.errors) {
            allErrors.add(err);
        }
    }
    return {
        dml: lastDml,
        tools: extractToolDependencies(lastDml),
        valid: false,
        errors: Array.from(allErrors),
        attempts: maxAttempts,
        usageByModel: Object.keys(usageByModel).length > 0 ? usageByModel : undefined,
    };
}
// =============================================================================
// LLM Integration
// =============================================================================
/**
 * Generate DML using LLM (non-streaming)
 */
async function generateDML(systemPrompt, userMessage, previousAttempts, model, provider, temperature, baseUrl, usageByModel) {
    const llm = getLanguageModel(provider, model, baseUrl);
    // Build messages including previous attempts for self-correction
    const messages = buildMessages(systemPrompt, userMessage, previousAttempts);
    const result = await generateText({
        model: llm,
        messages,
        temperature: temperature ?? 0.3,
        maxOutputTokens: 8192
    });
    if (result.usage && usageByModel) {
        recordTokenUsage(usageByModel, model, {
            inputTokens: result.usage.inputTokens ?? 0,
            outputTokens: result.usage.outputTokens ?? 0,
            totalTokens: result.usage.totalTokens ?? 0,
        });
    }
    return cleanDMLResponse(result.text);
}
/**
 * Build messages for the compilation, including error feedback from previous attempts
 */
function buildMessages(systemPrompt, userMessage, previousAttempts) {
    const messages = [
        { role: 'system', content: systemPrompt },
        { role: 'user', content: userMessage }
    ];
    // Add previous attempts and their errors for self-correction
    for (const attempt of previousAttempts) {
        messages.push({
            role: 'assistant',
            content: attempt.dml
        });
        const errorFeedback = [
            'The code above has the following validation errors:',
            ...attempt.validation.errors.map(e => `- ${e}`),
            '',
            'Please fix these errors and generate corrected DML code.'
        ].join('\n');
        messages.push({
            role: 'user',
            content: errorFeedback
        });
    }
    return messages;
}
/**
 * Generate a brief explanation of the compiled DML
 */
async function generateExplanation(dml, model, provider, baseUrl, usageByModel) {
    const llm = getLanguageModel(provider, model, baseUrl);
    try {
        const result = await generateText({
            model: llm,
            system: 'You are a technical writer. Explain code briefly and clearly.',
            prompt: `Briefly explain what this DML program does (2-3 sentences max):\n\n\`\`\`prolog\n${dml}\n\`\`\`\n\nFocus on: what it does, what tools it uses, and how it handles input. Be concise.`,
            temperature: 0.3,
            maxOutputTokens: 256
        });
        if (result.usage && usageByModel) {
            recordTokenUsage(usageByModel, model, {
                inputTokens: result.usage.inputTokens ?? 0,
                outputTokens: result.usage.outputTokens ?? 0,
                totalTokens: result.usage.totalTokens ?? 0,
            });
        }
        return result.text.trim();
    }
    catch {
        // If explanation generation fails, return a generic message
        return 'DML program compiled successfully.';
    }
}
/**
 * Clean up LLM response - remove markdown code fences
 */
function cleanDMLResponse(text) {
    let dml = text.trim();
    // Remove markdown code fences
    if (dml.startsWith('```prolog') || dml.startsWith('```')) {
        dml = dml.replace(/^```(?:prolog)?\n?/, '').replace(/\n?```$/, '');
    }
    return dml.trim();
}
/**
 * Get the appropriate language model instance
 */
function getLanguageModel(provider, model, baseUrl) {
    // If a custom baseUrl is provided (e.g. a proxy), route all providers through it
    if (baseUrl) {
        const proxy = createOpenAI({
            baseURL: baseUrl,
            apiKey: process.env.OPENAI_API_KEY || process.env.OPENROUTER_API_KEY || 'proxy',
        });
        return proxy.chat(model);
    }
    switch (provider) {
        case 'openai':
            if (!process.env.OPENAI_API_KEY) {
                throw new Error('Missing OPENAI_API_KEY environment variable');
            }
            return openai(model);
        case 'anthropic':
            if (!process.env.ANTHROPIC_API_KEY) {
                throw new Error('Missing ANTHROPIC_API_KEY environment variable');
            }
            return anthropic(model);
        case 'google':
            if (!process.env.GOOGLE_GENERATIVE_AI_API_KEY) {
                throw new Error('Missing GOOGLE_GENERATIVE_AI_API_KEY environment variable');
            }
            return google(model);
        case 'openrouter': {
            if (!process.env.OPENROUTER_API_KEY) {
                throw new Error('Missing OPENROUTER_API_KEY environment variable');
            }
            const openrouter = createOpenAI({
                baseURL: 'https://openrouter.ai/api/v1',
                apiKey: process.env.OPENROUTER_API_KEY
            });
            return openrouter(model);
        }
        default:
            throw new Error(`Unsupported provider: ${provider}`);
    }
}
// =============================================================================
// DML Analysis
// =============================================================================
/**
 * Extract tool dependencies from DML code
 */
export function extractToolDependencies(dml) {
    const execPattern = /exec\s*\(\s*([a-z_][a-z0-9_]*)\s*\(/gi;
    const tools = new Set();
    let match;
    while ((match = execPattern.exec(dml)) !== null) {
        tools.add(match[1]);
    }
    return Array.from(tools).sort();
}
/**
 * Extract parameters from agent_main signature
 */
export function extractParameters(dml) {
    const mainPattern = /agent_main\s*\(([^)]*)\)\s*:-/;
    const match = mainPattern.exec(dml);
    if (!match || !match[1].trim()) {
        return [];
    }
    const args = match[1]
        .split(',')
        .map(arg => arg.trim())
        .filter(arg => arg.length > 0);
    return args
        .map((arg, index) => ({
        name: arg.replace(/([A-Z])/g, (_m, c, i) => (i > 0 ? '_' : '') + c.toLowerCase()).replace(/^_/, ''),
        position: index,
        required: true
    }));
}
/**
 * Extract description from markdown
 */
export function extractDescription(markdown) {
    const lines = markdown.split('\n');
    for (const line of lines) {
        if (line.startsWith('# ')) {
            return line.substring(2).trim();
        }
    }
    for (const line of lines) {
        const trimmed = line.trim();
        if (trimmed && !trimmed.startsWith('#')) {
            return trimmed.substring(0, 100);
        }
    }
    return 'No description';
}
//# sourceMappingURL=compiler.js.map