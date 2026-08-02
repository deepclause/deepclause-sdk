/**
 * Command input bar with basic Emacs/Vi key bindings.
 */
import React from 'react';
interface CommandInputProps {
    onSubmit: (value: string) => void;
    onEscape: () => void;
    placeholder?: string;
    prefix?: string;
    active: boolean;
}
export declare const CommandInput: React.FC<CommandInputProps>;
export {};
//# sourceMappingURL=CommandInput.d.ts.map