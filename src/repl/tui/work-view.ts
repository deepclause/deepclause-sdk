import { truncateToWidth, type Component } from '@earendil-works/pi-tui';
import type { WorkSpec } from '../../work/index.js';
import { dim } from './theme.js';

/** A compact, read-only Work index. /workflows opens the keyboard chooser. */
export class WorkView implements Component {
  constructor(private readonly getSpec: () => WorkSpec | undefined) {}
  render(width: number): string[] {
    const spec = this.getSpec();
    const shell = spec?.shell;
    const lines = ['Work', spec ? `${spec.harness} · ${spec.version !== undefined ? `v${spec.version}` : spec.hash.slice(0, 8)}` : '', ''];
    if (!shell) lines.push(...(spec?.diagnostics ?? ['Use /mode work <harness>']));
    else {
      lines.push('WORKFLOWS', '/workflows to choose', '');
      shell.workflows.forEach(flow => lines.push(`▷ ${flow.name}`, dim(`  ${flow.inputs.length} in · ${flow.outputs.length} out`)));
      if (shell.tools.length) {
        lines.push('', 'ASSISTANT TOOLS', dim('Not directly runnable'), '');
        shell.tools.forEach(tool => lines.push(`  ${tool.name}`));
      }
      lines.push('', dim(shell.kind === 'agent' ? 'Type to talk to the assistant' : 'Choose a declared workflow'));
    }
    return lines.map(line => truncateToWidth(line, width, '…'));
  }
  invalidate(): void {}
}
