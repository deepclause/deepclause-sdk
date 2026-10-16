/**
 * The form representation: defaults, path walking, static spec validation, and
 * value validation. Pure — no runtime, no registry.
 */
import { describe, expect, it } from 'vitest';

import {
  applyDefaults,
  validateSpec,
  validateValues,
  walkFields,
  type FormSpec,
} from '../src/forms/spec.js';

const spec: FormSpec = {
  version: 1,
  title: 'Claim',
  fields: [
    { name: 'vendor', type: 'string', required: true, enum: ['acme', 'globex'] },
    { name: 'amount', type: 'number', minimum: 0, maximum: 100 },
    { name: 'category', type: 'string', default: 'meals' },
    {
      name: 'customer',
      type: 'object',
      properties: [{ name: 'email', type: 'string', format: 'email', required: true }],
    },
  ],
  layout: [
    { kind: 'field', field: 'vendor' },
    { kind: 'grid', columns: 2, children: [{ kind: 'field', field: 'amount', span: 2 }] },
  ],
};

describe('form spec', () => {
  it('walks fields depth-first with dotted paths', () => {
    expect(walkFields(spec.fields).map((ref) => ref.path)).toEqual([
      'vendor',
      'amount',
      'category',
      'customer',
      'customer.email',
    ]);
  });

  it('resolves defaults deepest-first without mutating the input', () => {
    const given = { vendor: 'acme' };
    const values = applyDefaults(spec, given);
    expect(values).toMatchObject({ vendor: 'acme', category: 'meals' });
    expect(values.customer).toEqual({});
    expect(given).toEqual({ vendor: 'acme' });
  });

  it('accepts a well-formed spec', () => {
    expect(validateSpec(spec)).toEqual([]);
  });

  it('rejects a layout that references an unknown field', () => {
    const bad: FormSpec = { ...spec, layout: [{ kind: 'field', field: 'nope' }] };
    expect(validateSpec(bad).map((issue) => issue.message)).toContain('layout references an unknown field');
  });

  it('rejects a span wider than its grid', () => {
    const bad: FormSpec = {
      ...spec,
      layout: [{ kind: 'grid', columns: 2, children: [{ kind: 'field', field: 'vendor', span: 3 }] }],
    };
    expect(validateSpec(bad).some((issue) => issue.message.includes('exceeds the grid width'))).toBe(true);
  });

  it('rejects duplicate field paths', () => {
    const bad: FormSpec = {
      ...spec,
      fields: [{ name: 'a', type: 'string' }, { name: 'a', type: 'string' }],
    };
    expect(validateSpec(bad).map((issue) => issue.message)).toContain('duplicate field path');
  });

  it('flags required, enum, range and format on values', () => {
    const issues = validateValues(spec, {
      vendor: 'nope',
      amount: -1,
      customer: { email: 'not-an-email' },
    });
    const byPath = new Map(issues.map((issue) => [issue.path, issue.message]));
    expect(byPath.get('vendor')).toContain('must be one of');
    expect(byPath.get('amount')).toContain('at least 0');
    expect(byPath.get('customer.email')).toContain('not a valid email');
  });

  it('flags a missing required field, including inside a group', () => {
    const issues = validateValues(spec, {});
    const paths = issues.map((issue) => issue.path);
    expect(paths).toContain('vendor');
    expect(paths).toContain('customer.email');
  });
});
