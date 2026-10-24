// Stamp a built SDK, or verify that dist and its source still match.
// --check is read-only. No credentials, model calls or network access.
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { readFile, readdir, writeFile } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';
const root = fileURLToPath(new URL('../', import.meta.url));
const target = join(root, 'dist/runtime-identity.json');
async function files(dir) {
  const out = [];
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) out.push(...await files(path));
    else if (entry.isFile() && path !== target) out.push(path);
  }
  return out.sort();
}
async function digest(paths) {
  const hash = createHash('sha256');
  for (const path of [...paths].sort()) {
    hash.update(relative(root, path).replaceAll('\\', '/')).update('\0');
    hash.update(await readFile(path)).update('\0');
  }
  return hash.digest('hex');
}
try {
  const pkg = JSON.parse(await readFile(join(root, 'package.json'), 'utf8'));
  const sourceDigest = await digest([
    ...await files(join(root, 'src')),
    join(root, 'package.json'), join(root, 'tsconfig.json'),
    fileURLToPath(import.meta.url),
    join(root, 'vendor/swipl-wasm/dist/swipl/swipl-web.wasm'),
    join(root, 'vendor/swipl-wasm/dist/swipl/swipl-web.js'),
  ]);
  const artifactDigest = await digest(await files(join(root, 'dist')));
  const { WORK_CONTRACT_VERSION } = await import(new URL('../dist/work/index.js', import.meta.url));
  if (process.argv.includes('--check')) {
    const saved = JSON.parse(await readFile(target, 'utf8'));
    if (saved.sourceDigest !== sourceDigest || saved.artifactDigest !== artifactDigest || saved.sdkVersion !== pkg.version || saved.workContractVersion !== WORK_CONTRACT_VERSION) {
      throw new Error('SDK dist is stale or modified. Run npm run build in deepclause-sdk before syncing/building the web app.');
    }
    console.log(`checked SDK runtime ${artifactDigest.slice(0, 12)} (Work contract ${WORK_CONTRACT_VERSION})`);
  } else {
    let gitRevision = 'unknown';
    let dirty = true;
    try {
      gitRevision = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim();
      dirty = !!execFileSync('git', ['status', '--porcelain', '--', 'src', 'scripts/runtime-identity.mjs', 'package.json', 'tsconfig.json'], { cwd: root, encoding: 'utf8' }).trim();
    } catch { /* a source archive may not have Git metadata */ }
    await writeFile(target, JSON.stringify({ formatVersion: 1, sdkVersion: pkg.version, workContractVersion: WORK_CONTRACT_VERSION,
      gitRevision, dirty, sourceDigest, artifactDigest, builtAt: new Date().toISOString() }, null, 2) + '\n');
    console.log(`stamped SDK runtime ${artifactDigest.slice(0, 12)} (Work contract ${WORK_CONTRACT_VERSION})`);
  }
} catch (error) {
  console.error(`[runtime identity] ${error.message}`);
  process.exitCode = 1;
}
