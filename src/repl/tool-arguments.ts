/**
 * Tool-call parameter validation.
 *
 * A tool call with an unknown parameter used to be accepted and the extra key
 * silently dropped, so `exec(bash(command: "cat", stdin: "…"), R)` looked
 * reasonable, ran `cat` with no input, and reported success. That is the worst
 * kind of failure: nothing to see, nothing to fix, and the DML author blames
 * their own logic. Rejecting the call names the mistake at the point it is made.
 *
 * Deliberately shallow — unknown keys, missing required keys, and the primitive
 * types. Deep or conditional validation belongs in the tool, which knows its own
 * semantics; this exists to catch a call that cannot mean what it says.
 */

export interface ToolJsonSchema {
  type?: string;
  properties?: Record<string, { type?: string | string[] }>;
  required?: string[];
  additionalProperties?: boolean;
}

export type ArgumentValidation = { ok: true } | { ok: false; error: string };

/**
 * Extra advice for mistakes that are easy to make and hard to read back. The
 * generic "unknown parameter" message is enough for a typo, but not for a field
 * that looks like it should exist.
 */
const PARAMETER_HINTS: Record<string, string> = {
  stdin: 'exec(bash(...)) has no stdin. Pass the data as an `argv` element '
    + '(quoted by the runtime), or write it to a workspace file and name the file.',
  input: 'Pass data as an `argv` element or through a workspace file.',
  shell: 'exec(bash(...)) always runs a shell; put the command in `command`, '
    + 'or pass a program and its arguments as `argv`.',
  cwd: 'The working directory is always the workspace (/workspace); do not pass one.',
};

/** Read `properties`/`required` out of a JSON Schema tool parameter block. */
export function readToolSchema(schema: unknown): ToolJsonSchema | undefined {
  if (!schema || typeof schema !== 'object') return undefined;
  const record = schema as Record<string, unknown>;
  const properties = record.properties;
  if (!properties || typeof properties !== 'object') return undefined;
  return {
    properties: properties as ToolJsonSchema['properties'],
    required: Array.isArray(record.required) ? (record.required as string[]) : [],
    additionalProperties: record.additionalProperties as boolean | undefined,
  };
}

/** Does `value` satisfy the declared primitive type? */
function matchesType(value: unknown, type: string | string[] | undefined): boolean {
  if (!type) return true;
  const types = Array.isArray(type) ? type : [type];
  return types.some((candidate) => {
    switch (candidate) {
      case 'string':
        return typeof value === 'string';
      case 'number':
        return typeof value === 'number' && Number.isFinite(value);
      case 'integer':
        return typeof value === 'number' && Number.isInteger(value);
      case 'boolean':
        return typeof value === 'boolean';
      case 'array':
        return Array.isArray(value);
      case 'object':
        return typeof value === 'object' && value !== null && !Array.isArray(value);
      case 'null':
        return value === null;
      default:
        return true;
    }
  });
}

function describeAccepted(properties: Record<string, unknown>): string {
  const names = Object.keys(properties);
  return names.length > 0 ? names.join(', ') : '(none)';
}

/**
 * Validate a tool call's arguments. Nothing to check is not a failure: a tool
 * with no declared properties, or a schema this cannot read (a Zod schema is
 * enforced by the AI SDK instead), passes.
 */
export function validateToolArguments(
  toolName: string,
  schema: unknown,
  args: Record<string, unknown>,
): ArgumentValidation {
  const declared = readToolSchema(schema);
  // Nothing declared means nothing to check — a tool with no parameters (which
  // may still be called positionally) cannot have a wrong one.
  if (!declared?.properties || Object.keys(declared.properties).length === 0) return { ok: true };
  const properties = declared.properties;

  const unknownKeys = declared.additionalProperties === true
    ? []
    : Object.keys(args).filter((key) => !(key in properties) && !key.startsWith('$'));
  if (unknownKeys.length > 0) {
    const quoted = unknownKeys.map((key) => `'${key}'`).join(', ');
    // The hint explains the mistake; the accepted list says what to write
    // instead. Both, or the author is left guessing which parameter was meant.
    const hints = unknownKeys
      .map((key) => PARAMETER_HINTS[key])
      .filter(Boolean)
      .join(' ');
    return {
      ok: false,
      error: `${toolName}: unknown parameter ${quoted}. ${hints} `
        + `Accepted: ${describeAccepted(properties)}.`.replace(/\s+/g, ' '),
    };
  }

  const missing = (declared.required ?? []).filter(
    (key) => args[key] === undefined || args[key] === null,
  );
  if (missing.length > 0) {
    return {
      ok: false,
      error: `${toolName}: missing required parameter${missing.length > 1 ? 's' : ''} `
        + `${missing.join(', ')}.`,
    };
  }

  for (const [key, value] of Object.entries(args)) {
    if (value === undefined || key.startsWith('$')) continue;
    const expected = properties[key]?.type;
    if (!matchesType(value, expected)) {
      const typeLabel = Array.isArray(expected) ? expected.join('|') : String(expected);
      const actual = Array.isArray(value) ? 'array' : value === null ? 'null' : typeof value;
      return {
        ok: false,
        error: `${toolName}: parameter ${key} should be ${typeLabel}, got ${actual}.`,
      };
    }
  }

  return { ok: true };
}
