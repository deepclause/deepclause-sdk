/**
 * `/login` accepts an optional provider plus llama.cpp endpoint flags.
 */
import { describe, expect, it } from 'vitest';

import { classifyReplInput, parseLoginArgs } from '../src/cli/repl.js';

describe('classifyReplInput /login', () => {
  it('parses an optional provider and llama.cpp endpoint flags', () => {
    expect(classifyReplInput('/login')).toMatchObject({ kind: 'login' });
    expect(classifyReplInput('/login llama.cpp')).toMatchObject({
      kind: 'login',
      provider: 'llama.cpp',
    });
    expect(classifyReplInput('/login llama.cpp --host 192.168.1.50 --port 9000')).toMatchObject({
      kind: 'login',
      provider: 'llama.cpp',
      host: '192.168.1.50',
      port: '9000',
    });
    expect(classifyReplInput('/login --base-url=http://gpu-box:9000')).toMatchObject({
      kind: 'login',
      baseUrl: 'http://gpu-box:9000',
    });
    expect(classifyReplInput('/login llama.cpp http://gpu-box:9000')).toMatchObject({
      kind: 'login',
      provider: 'llama.cpp',
      baseUrl: 'http://gpu-box:9000',
    });
    expect(parseLoginArgs('--provider llama.cpp --port=11434')).toMatchObject({
      kind: 'login',
      provider: 'llama.cpp',
      port: '11434',
    });
  });

  it('rejects malformed login arguments', () => {
    expect(classifyReplInput('/login --host').kind).toBe('unknown-command');
    expect(classifyReplInput('/login --bogus').kind).toBe('unknown-command');
    expect(classifyReplInput('/login llama.cpp extra').kind).toBe('unknown-command');
  });
});
