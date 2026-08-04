/**
 * Fuzzy picker overlay component.
 * Provides a searchable list for selecting sessions, skills, files, etc.
 */
import React from 'react';
export interface PickerItem {
    id: string;
    label: string;
    description: string;
    detail?: string;
}
interface PickerProps {
    title: string;
    items: PickerItem[];
    onSelect: (item: PickerItem) => void;
    onCancel: () => void;
    emptyText?: string;
}
export declare const Picker: React.FC<PickerProps>;
export {};
//# sourceMappingURL=Picker.d.ts.map