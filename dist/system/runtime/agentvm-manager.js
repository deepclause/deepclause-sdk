let AgentVMClass = null;
export class AgentVMManager {
    workspacePath;
    network;
    vm = null;
    kind = 'sandbox';
    constructor(workspacePath, network) {
        this.workspacePath = workspacePath;
        this.network = network;
    }
    async exec(command, signal, observer) {
        if (signal?.aborted) {
            throw abortError(signal.reason);
        }
        const vm = await this.getVM();
        observer?.onStart?.({ command, backend: this.kind, backendLabel: 'sandbox[agentvm]' });
        const result = signal
            ? await new Promise((resolve, reject) => {
                let settled = false;
                const finishResolve = (value) => {
                    if (settled) {
                        return;
                    }
                    settled = true;
                    cleanup();
                    resolve(value);
                };
                const finishReject = (error) => {
                    if (settled) {
                        return;
                    }
                    settled = true;
                    cleanup();
                    reject(error);
                };
                const cleanup = () => {
                    signal.removeEventListener('abort', onAbort);
                };
                const onAbort = () => {
                    void this.dispose().catch(() => { });
                    finishReject(abortError(signal.reason));
                };
                signal.addEventListener('abort', onAbort, { once: true });
                void vm.exec(command)
                    .then((value) => {
                    if (signal.aborted) {
                        finishReject(abortError(signal.reason));
                        return;
                    }
                    finishResolve(value);
                })
                    .catch((error) => {
                    if (signal.aborted) {
                        finishReject(abortError(signal.reason));
                        return;
                    }
                    finishReject(error);
                });
            })
            : await vm.exec(command);
        const stdout = result.stdout ?? '';
        const stderr = result.stderr ?? '';
        const exitCode = result.exitCode ?? 0;
        const execResult = {
            success: exitCode === 0,
            stdout,
            stderr,
            exitCode,
            backend: this.kind,
            backendLabel: 'sandbox[agentvm]',
            summary: exitCode === 0 ? 'Command completed successfully' : (stderr || `Command failed with exit code ${exitCode}`),
        };
        if (stdout) {
            observer?.onStdout?.({ command, chunk: stdout, backend: this.kind, backendLabel: execResult.backendLabel });
        }
        if (stderr) {
            observer?.onStderr?.({ command, chunk: stderr, backend: this.kind, backendLabel: execResult.backendLabel });
        }
        observer?.onExit?.({
            command,
            backend: this.kind,
            backendLabel: execResult.backendLabel,
            success: execResult.success,
            exitCode,
            summary: execResult.summary,
        });
        return execResult;
    }
    async dispose() {
        if (this.vm) {
            await this.vm.stop();
            this.vm = null;
        }
    }
    async getVM() {
        if (!AgentVMClass) {
            const mod = await import('deepclause-agentvm');
            AgentVMClass = mod.AgentVM;
        }
        if (!this.vm) {
            this.vm = new AgentVMClass({
                network: this.network,
                mounts: { '/workspace': this.workspacePath },
            });
            await this.vm.start();
            await this.vm.exec('cd /workspace');
        }
        return this.vm;
    }
}
function abortError(reason) {
    return reason instanceof Error
        ? reason
        : Object.assign(new Error('This operation was aborted'), { name: 'AbortError' });
}
//# sourceMappingURL=agentvm-manager.js.map