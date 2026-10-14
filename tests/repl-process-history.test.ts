/**
 * Service records in the session directory: a declaration and its exit event
 * round-trip through `services.jsonl`.
 */
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import * as fs from 'fs/promises';
import * as os from 'os';
import * as path from 'path';

import { HistoryStore } from '../src/repl/history.js';

let workspace: string;

beforeEach(async () => {
  workspace = await fs.mkdtemp(path.join(os.tmpdir(), 'deepclause-svc-history-'));
});

afterEach(async () => {
  await fs.rm(workspace, { recursive: true, force: true });
});

describe('service records', () => {
  it('appends and reads declarations and exits in order', async () => {
    const store = new HistoryStore(workspace, 'sess1', { dir: '.deepclause/repl', trace: 'summary' }, 'main');
    await store.appendServiceRecord({ type: 'declare', name: 'keeper', goal: 'keeper([])' });
    await store.appendServiceRecord({ type: 'exit', name: 'keeper', state: 'done' });

    const records = await store.readServiceRecords();
    expect(records).toHaveLength(2);
    expect(records[0]).toMatchObject({ type: 'declare', name: 'keeper' });
    expect(records[1]).toMatchObject({ type: 'exit', state: 'done' });
    expect(store.sessionDirectory).toContain('sessions/sess1');
  });

  it('returns an empty list when nothing was recorded', async () => {
    const store = new HistoryStore(workspace, 'sess2', { dir: '.deepclause/repl', trace: 'summary' }, 'main');
    expect(await store.readServiceRecords()).toEqual([]);
  });
});
