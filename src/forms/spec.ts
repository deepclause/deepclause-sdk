/**
 * The form representation: the declarative spec a harness (or a model) produces,
 * the values a UI collects, and the two validators.
 *
 * Pure functions and types — no runtime, no registry, no events. See
 * `docs/DML_FORMS.md` §5.
 */

export type Json = string | number | boolean | null | Json[] | { [key: string]: Json };
export type JsonObject = { [key: string]: Json };

/** Presentation hint only; validation never reads it. */
export type Widget =
  | 'text'
  | 'textarea'
  | 'number'
  | 'slider'
  | 'select'
  | 'radio'
  | 'multiselect'
  | 'checkbox'
  | 'switch'
  | 'date'
  | 'time'
  | 'datetime'
  | 'file'
  | 'hidden';

export type FieldType = 'string' | 'number' | 'integer' | 'boolean' | 'object' | 'array' | 'null';

export interface FieldOption {
  value: string;
  title?: string;
  description?: string;
}

interface FieldBase {
  /** Unique within its parent. The contract: rules and patches address fields by path. */
  name: string;
  type: FieldType;
  required?: boolean;
  default?: Json;
  title?: string;
  description?: string;
  widget?: Widget;
}

export interface StringField extends FieldBase {
  type: 'string';
  format?: 'date' | 'time' | 'datetime' | 'email' | 'uri' | 'media';
  enum?: string[];
  options?: FieldOption[];
  minLength?: number;
  maxLength?: number;
  pattern?: string;
}

export interface NumberField extends FieldBase {
  type: 'number' | 'integer';
  minimum?: number;
  maximum?: number;
  multipleOf?: number;
}

export interface BooleanField extends FieldBase {
  type: 'boolean';
}

export interface ObjectField extends FieldBase {
  type: 'object';
  properties: Field[];
}

export interface ArrayField extends FieldBase {
  type: 'array';
  items: Field;
  minItems?: number;
  maxItems?: number;
}

export interface NullField extends FieldBase {
  type: 'null';
}

export type Field = StringField | NumberField | BooleanField | ObjectField | ArrayField | NullField;

export interface LayoutBase {
  /** Grid columns this node occupies. */
  span?: number;
}

export interface LayoutRow extends LayoutBase {
  kind: 'row';
  children: LayoutNode[];
  gap?: number;
  align?: 'start' | 'center' | 'end' | 'stretch';
  wrap?: boolean;
}

export interface LayoutColumn extends LayoutBase {
  kind: 'column';
  children: LayoutNode[];
  gap?: number;
}

export interface LayoutGrid extends LayoutBase {
  kind: 'grid';
  columns: number;
  gap?: number;
  children: LayoutNode[];
}

export interface LayoutBox extends LayoutBase {
  kind: 'box';
  title?: string;
  description?: string;
  variant?: 'plain' | 'card' | 'fieldset';
  children: LayoutNode[];
}

export interface LayoutTabs extends LayoutBase {
  kind: 'tabs';
  tabs: Array<{ title: string; children: LayoutNode[] }>;
}

export interface LayoutField extends LayoutBase {
  kind: 'field';
  /** Path of a declared field. */
  field: string;
  widget?: Widget;
  label?: string;
  help?: string;
}

export interface LayoutText extends LayoutBase {
  kind: 'text';
  text: string;
  variant?: 'body' | 'caption' | 'heading';
}

export interface LayoutDivider extends LayoutBase {
  kind: 'divider';
}

export interface LayoutSpacer extends LayoutBase {
  kind: 'spacer';
  size?: number;
}

export type LayoutNode =
  | LayoutRow
  | LayoutColumn
  | LayoutGrid
  | LayoutBox
  | LayoutTabs
  | LayoutField
  | LayoutText
  | LayoutDivider
  | LayoutSpacer;

export interface FormAction {
  name: string;
  title: string;
  kind?: 'primary' | 'default' | 'danger' | 'link';
  confirm?: string;
  /** Paths to validate before the action is allowed, or `hard`/`all`. */
  validates?: 'all' | 'hard' | string[];
}

export interface FormSpec {
  version: 1;
  id?: string;
  title?: string;
  description?: string;
  fields: Field[];
  layout?: LayoutNode[];
  initial?: JsonObject;
  actions?: FormAction[];
  ui?: { submitOnEnter?: boolean; dense?: boolean };
  meta?: JsonObject;
}

/** A per-field message, from the host (shape) or the harness (domain). */
export interface FormIssue {
  path: string;
  level: 'error' | 'warning';
  message: string;
}

/** What a UI sends back when the user acts. */
export interface FormChange {
  formId: string;
  /** Monotonic; the race-free cursor. */
  revision: number;
  action: string;
  values: JsonObject;
  changed?: string[];
  errors?: FormIssue[];
}

export const DEFAULT_ACTIONS: FormAction[] = [
  { name: 'submit', title: 'Submit', kind: 'primary' },
  { name: 'cancel', title: 'Cancel', kind: 'link' },
];

export function actionsOf(spec: FormSpec): FormAction[] {
  return spec.actions && spec.actions.length > 0 ? spec.actions : DEFAULT_ACTIONS;
}

// ---------------------------------------------------------------------------
// Walking fields
// ---------------------------------------------------------------------------

export interface FieldRef {
  path: string;
  field: Field;
}

/** Every field, depth-first, with its dotted path (`a.b`, `items[0].c` is the value form). */
export function walkFields(fields: Field[], prefix = ''): FieldRef[] {
  const out: FieldRef[] = [];
  for (const field of fields) {
    const path = prefix ? `${prefix}.${field.name}` : field.name;
    out.push({ path, field });
    if (field.type === 'object') out.push(...walkFields(field.properties, path));
  }
  return out;
}

/** The declared fields, keyed by path. */
export function fieldMap(spec: FormSpec): Map<string, Field> {
  const map = new Map<string, Field>();
  for (const ref of walkFields(spec.fields)) map.set(ref.path, ref.field);
  return map;
}

// ---------------------------------------------------------------------------
// Defaults
// ---------------------------------------------------------------------------

/** Resolve defaults into a complete value set (deepest-first). Never mutates input. */
export function applyDefaults(spec: FormSpec, values: JsonObject = {}): JsonObject {
  const out: JsonObject = { ...(spec.initial ?? {}), ...values };
  applyFieldDefaults(spec.fields, out);
  return out;
}

function applyFieldDefaults(fields: Field[], target: JsonObject): void {
  for (const field of fields) {
    const current = target[field.name];
    if (field.type === 'object') {
      const child = isPlainObject(current) ? (current as JsonObject) : {};
      target[field.name] = child;
      applyFieldDefaults(field.properties, child);
    } else if (field.type === 'array') {
      if (!Array.isArray(current)) target[field.name] = Array.isArray(field.default) ? field.default : [];
    } else if (current === undefined && field.default !== undefined) {
      target[field.name] = field.default;
    }
  }
}

export function isPlainObject(value: unknown): value is JsonObject {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

// ---------------------------------------------------------------------------
// Static validation: is this a well-formed spec?
// ---------------------------------------------------------------------------

export function validateSpec(spec: FormSpec): FormIssue[] {
  const issues: FormIssue[] = [];
  const err = (path: string, message: string): void => {
    issues.push({ path, level: 'error', message });
  };

  if (spec.version !== 1) err('', `unsupported form version: ${String(spec.version)}`);
  if (!Array.isArray(spec.fields) || spec.fields.length === 0) err('', 'a form needs at least one field');

  const seen = new Set<string>();
  for (const ref of walkFields(spec.fields ?? [])) {
    if (seen.has(ref.path)) err(ref.path, 'duplicate field path');
    seen.add(ref.path);
    validateFieldSpec(ref.field, ref.path, err);
  }

  if (spec.layout) {
    const columns = new Map<string, number>();
    collectGridColumns(spec.layout, columns);
    validateLayout(spec.layout, seen, columns, err);
  }
  return issues;
}

function validateFieldSpec(field: Field, path: string, err: (path: string, message: string) => void): void {
  if (!field.name || !/^[A-Za-z_][A-Za-z0-9_]*$/.test(field.name)) {
    err(path, `invalid field name: ${JSON.stringify(field.name)}`);
  }
  if (field.type === 'string') {
    const s = field as StringField;
    if (s.enum && s.enum.length === 0) err(path, 'enum must not be empty');
    if (s.minLength !== undefined && s.maxLength !== undefined && s.minLength > s.maxLength) {
      err(path, 'minLength exceeds maxLength');
    }
  }
  if (field.type === 'number' || field.type === 'integer') {
    const n = field as NumberField;
    if (n.minimum !== undefined && n.maximum !== undefined && n.minimum > n.maximum) {
      err(path, 'minimum exceeds maximum');
    }
  }
  if (field.type === 'array') {
    const a = field as ArrayField;
    if (a.minItems !== undefined && a.maxItems !== undefined && a.minItems > a.maxItems) {
      err(path, 'minItems exceeds maxItems');
    }
  }
}

function collectGridColumns(nodes: LayoutNode[], out: Map<string, number>): void {
  for (const node of nodes) {
    if (node.kind === 'grid') out.set('grid', node.columns);
    for (const child of layoutChildren(node)) collectGridColumns(child, out);
  }
}

function layoutChildren(node: LayoutNode): LayoutNode[][] {
  switch (node.kind) {
    case 'row':
    case 'column':
    case 'grid':
    case 'box':
      return [node.children];
    case 'tabs':
      return node.tabs.map((tab) => tab.children);
    default:
      return [];
  }
}

function validateLayout(
  nodes: LayoutNode[],
  fieldPaths: Set<string>,
  gridColumns: Map<string, number>,
  err: (path: string, message: string) => void,
): void {
  for (const node of nodes) {
    if (node.kind === 'field' && !fieldPaths.has(node.field)) {
      err(node.field, 'layout references an unknown field');
    }
    if (node.kind === 'grid' && (!Number.isInteger(node.columns) || node.columns < 1)) {
      err('', 'grid columns must be a positive integer');
    }
    if (node.span !== undefined) {
      const columns = node.kind === 'grid' ? node.columns : gridColumns.get('grid');
      if (columns !== undefined && node.span > columns) {
        err('', `span ${node.span} exceeds the grid width ${columns}`);
      }
    }
    for (const children of layoutChildren(node)) validateLayout(children, fieldPaths, gridColumns, err);
  }
}

// ---------------------------------------------------------------------------
// Dynamic validation: are these values acceptable? (shape only — domain rules
// are the harness's `form_validate/2`.)
// ---------------------------------------------------------------------------

export function validateValues(spec: FormSpec, values: JsonObject): FormIssue[] {
  const issues: FormIssue[] = [];
  for (const ref of walkFields(spec.fields)) {
    validateValue(ref.field, ref.path, valueAtPath(values, ref.path), issues);
  }
  return issues;
}

/** Read a dotted field path out of a value tree. */
export function valueAtPath(values: JsonObject, path: string): Json | undefined {
  const parts = path.split('.');
  let current: Json | undefined = values;
  for (const part of parts) {
    if (!isPlainObject(current)) return undefined;
    current = (current as JsonObject)[part];
  }
  return current;
}

function isEmpty(value: Json | undefined): boolean {
  return value === undefined || value === null || value === '' || (Array.isArray(value) && value.length === 0);
}

function validateValue(field: Field, path: string, value: Json | undefined, issues: FormIssue[]): void {
  const warn = (message: string): void => {
    issues.push({ path, level: 'error', message });
  };
  if (isEmpty(value)) {
    if (field.required) warn('is required');
    return;
  }
  switch (field.type) {
    case 'string': {
      const s = field as StringField;
      if (typeof value !== 'string') return warn('must be a string');
      if (s.enum && !s.enum.includes(value)) warn(`must be one of: ${s.enum.join(', ')}`);
      if (s.minLength !== undefined && value.length < s.minLength) warn(`must be at least ${s.minLength} characters`);
      if (s.maxLength !== undefined && value.length > s.maxLength) warn(`must be at most ${s.maxLength} characters`);
      if (s.pattern !== undefined && !new RegExp(s.pattern).test(value)) warn('does not match the required pattern');
      if (s.format === 'email' && !/^[^@\s]+@[^@\s]+$/.test(value)) warn('is not a valid email address');
      break;
    }
    case 'number':
    case 'integer': {
      const n = field as NumberField;
      if (typeof value !== 'number' || Number.isNaN(value)) return warn('must be a number');
      if (field.type === 'integer' && !Number.isInteger(value)) warn('must be a whole number');
      if (n.minimum !== undefined && value < n.minimum) warn(`must be at least ${n.minimum}`);
      if (n.maximum !== undefined && value > n.maximum) warn(`must be at most ${n.maximum}`);
      break;
    }
    case 'boolean':
      if (typeof value !== 'boolean') warn('must be true or false');
      break;
    case 'array': {
      const a = field as ArrayField;
      if (!Array.isArray(value)) return warn('must be a list');
      if (a.minItems !== undefined && value.length < a.minItems) warn(`needs at least ${a.minItems} items`);
      if (a.maxItems !== undefined && value.length > a.maxItems) warn(`allows at most ${a.maxItems} items`);
      break;
    }
    default:
      break;
  }
}
