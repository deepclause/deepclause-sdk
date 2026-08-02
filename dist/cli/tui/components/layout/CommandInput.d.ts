/**
 * Command input bar with basic Emacs/Vi key bindings.
 * Always visible; activates on any character input when in normal mode.
 */
import React from 'react';
interface CommandInputProps {
    onSubmit: (value: string) => void;
    onEscape: () => void;
    onActivate: () => void;
    placeholder?: string;
    prefix?: string;
    active: boolean;
    busy?: boolean;
}
export declare const CommandInput: React.FC<CommandInputProps>;
export {};
//# sourceMappingURL=CommandInput.d.ts.map