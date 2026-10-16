#!/usr/bin/env node
/**
 * Keep the bundled forms reference identical to the design doc.
 *
 * `docs/DML_FORMS.md` is the source of truth. The `reference` tool can only serve
 * files under `src/system/assets/docs/` — those are what `copy:assets` ships — so
 * the doc is copied there. A copy drifts: this repo already had one reference
 * file that fell six sections behind the prompt that was supposed to mirror it.
 *
 * `tests/forms-discoverability.test.ts` fails when the two differ.
 */
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, '..');
const source = join(root, 'docs', 'DML_FORMS.md');
const target = join(root, 'src', 'system', 'assets', 'docs', 'DML_FORMS.md');

const content = readFileSync(source, 'utf8');

if (process.argv.includes('--check')) {
  const current = readFileSync(target, 'utf8');
  if (current !== content) {
    console.error(
      'src/system/assets/docs/DML_FORMS.md is out of sync with docs/DML_FORMS.md — run: npm run sync:docs',
    );
    process.exit(1);
  }
  console.log('bundled DML_FORMS.md is in sync');
} else {
  writeFileSync(target, content);
  console.log(`synced DML_FORMS.md (${content.length} bytes)`);
}
