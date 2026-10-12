/**
 * Render a Mermaid `flowchart` (the subset `visualize` asks the model for) as a
 * Unicode tree, so a diagram can be shown inline in the REPL, in a tool result,
 * or over a terminal that has no image protocol.
 *
 * No dependencies and no browser: the diagram is parsed into nodes and edges and
 * laid out as an indented flow. `parseMermaidFlowchart` / `buildFlowTree` are
 * also consumed by `svg.ts` for the terminal-image path.
 *
 * Supported: `flowchart TD|TB|BT|LR|RL`, node shapes `[..] (..) {..} ([..])
 * [[..]] ((..)) {{..}}`, quoted labels, chained edges (`A --> B --> C`),
 * connectors `--> --- -.-> ==> -.- ===`, edge labels (`-->|x|`, `-- x -->`),
 * and `subgraph`/`end` (grouping is flattened).
 */

export type NodeShape = 'rect' | 'round' | 'diamond' | 'stadium' | 'subroutine' | 'circle' | 'hexagon';

export interface MermaidNode {
  id: string;
  /** The display label (defaults to the node id). */
  label: string;
  shape: NodeShape;
}

export interface MermaidEdge {
  from: string;
  to: string;
  /** Edge label (`-->|yes|` or `-- yes -->`). */
  label?: string;
}

export interface ParsedFlowchart {
  /** `TD`, `LR`, … (defaults to `TD`). */
  direction: string;
  nodes: MermaidNode[];
  edges: MermaidEdge[];
}

export interface FlowTreeNode {
  id: string;
  label: string;
  shape: NodeShape;
  /** Label on the edge from the parent (undefined for a root). */
  edgeLabel?: string;
  /** True for a back/duplicate reference (the subtree is not expanded again). */
  repeated?: boolean;
  children: FlowTreeNode[];
}

const HEADER = /^\s*(?:flowchart|graph)\s+(TD|TB|BT|LR|RL)\b/i;
const CONNECTOR = /^(-{2,3}>|={2,}>|-\.-+>|-\.-|--o|--x|-{2,}|={2,})/;

/** Shape open/close pairs, longest opener first. */
const SHAPES: Array<{ open: string; close: string; shape: NodeShape }> = [
  { open: '([', close: '])', shape: 'stadium' },
  { open: '[[', close: ']]', shape: 'subroutine' },
  { open: '((', close: '))', shape: 'circle' },
  { open: '{{', close: '}}', shape: 'hexagon' },
  { open: '[', close: ']', shape: 'rect' },
  { open: '(', close: ')', shape: 'round' },
  { open: '{', close: '}', shape: 'diamond' },
];

/** Normalize `-- label -->` into `-->|label|` so one edge form is parsed. */
function normalizeEdgeLabels(line: string): string {
  return line
    .replace(/--\s+([^|>{}[\]]+?)\s+-->/g, '-->|$1|')
    .replace(/--\s+([^|>{}[\]]+?)\s+---/g, '---|$1|');
}

/** Read one node reference (`id` plus an optional shape) starting at `i`. */
function readNode(text: string, i: number): { node: MermaidNode; next: number } | undefined {
  let j = i;
  while (j < text.length && /\s/.test(text[j])) {
    j += 1;
  }
  const idMatch = /^([A-Za-z_][A-Za-z0-9_-]*|\d+)/.exec(text.slice(j));
  if (!idMatch) {
    return undefined;
  }
  const id = idMatch[1];
  j += id.length;
  let label = id;
  let shape: NodeShape = 'rect';

  const spec = SHAPES.find((s) => text.startsWith(s.open, j));
  if (spec) {
    const start = j + spec.open.length;
    const close = text.indexOf(spec.close, start);
    if (close !== -1) {
      label = text.slice(start, close).trim();
      shape = spec.shape;
      j = close + spec.close.length;
    }
  }

  if ((label.startsWith('"') && label.endsWith('"')) || (label.startsWith("'") && label.endsWith("'"))) {
    label = label.slice(1, -1);
  }
  return { node: { id, label: label || id, shape }, next: j };
}

/** Parse a single flowchart line into edges (and register any new nodes). */
function parseLine(line: string, nodes: Map<string, MermaidNode>, edges: MermaidEdge[]): void {
  const text = normalizeEdgeLabels(line.trim().replace(/;+\s*$/, ''));
  if (!text) {
    return;
  }
  const register = (node: MermaidNode): void => {
    const existing = nodes.get(node.id);
    if (existing) {
      // Prefer a real label over a bare id reference.
      if (node.label !== node.id && existing.label === existing.id) {
        nodes.set(node.id, { ...existing, label: node.label, shape: node.shape });
      }
      return;
    }
    nodes.set(node.id, node);
  };

  let cursor = 0;
  let prev = readNode(text, cursor);
  if (!prev) {
    return;
  }
  register(prev.node);
  cursor = prev.next;

  for (;;) {
    while (cursor < text.length && /\s/.test(text[cursor])) {
      cursor += 1;
    }
    if (cursor >= text.length) {
      return;
    }
    const connector = CONNECTOR.exec(text.slice(cursor));
    if (!connector) {
      const extra = readNode(text, cursor);
      if (!extra) {
        return;
      }
      register(extra.node);
      prev = extra;
      cursor = extra.next;
      continue;
    }
    cursor += connector[1].length;
    while (cursor < text.length && /\s/.test(text[cursor])) {
      cursor += 1;
    }
    let label: string | undefined;
    if (text[cursor] === '|') {
      const close = text.indexOf('|', cursor + 1);
      if (close !== -1) {
        label = text.slice(cursor + 1, close).trim() || undefined;
        cursor = close + 1;
      }
    }
    const next = readNode(text, cursor);
    if (!next) {
      return;
    }
    register(next.node);
    edges.push({ from: prev.node.id, to: next.node.id, label });
    prev = next;
    cursor = next.next;
  }
}

/** Parse a Mermaid flowchart into nodes and edges. */
export function parseMermaidFlowchart(source: string): ParsedFlowchart {
  const nodes = new Map<string, MermaidNode>();
  const edges: MermaidEdge[] = [];
  let direction = 'TD';
  let subgraphs = 0;
  let sawHeader = false;

  for (const rawLine of (source ?? '').split('\n')) {
    const trimmed = rawLine.replace(/\s+$/, '').trim();
    if (!trimmed || trimmed.startsWith('%%')) {
      continue;
    }
    const header = HEADER.exec(trimmed);
    if (header) {
      direction = header[1].toUpperCase() === 'TB' ? 'TD' : header[1].toUpperCase();
      sawHeader = true;
      continue;
    }
    // Only parse a body once a flowchart header has been seen, so prose that
    // was not a diagram is not mistaken for a list of bare nodes.
    if (!sawHeader) {
      continue;
    }
    if (/^subgraph\b/i.test(trimmed)) {
      subgraphs += 1;
      continue;
    }
    if (/^end\b/i.test(trimmed) && subgraphs > 0) {
      subgraphs -= 1;
      continue;
    }
    if (/^(classDef|class|style|linkStyle|click|direction)\b/i.test(trimmed)) {
      continue;
    }
    parseLine(trimmed, nodes, edges);
  }

  return { direction, nodes: [...nodes.values()], edges };
}

/** Build the root(s) of a tree view, marking back/duplicate references. */
export function buildFlowTree(parsed: ParsedFlowchart): FlowTreeNode[] {
  const byId = new Map(parsed.nodes.map((n) => [n.id, n]));
  const children = new Map<string, MermaidEdge[]>();
  const indegree = new Map<string, number>();
  for (const node of parsed.nodes) {
    children.set(node.id, []);
    indegree.set(node.id, 0);
  }
  for (const edge of parsed.edges) {
    if (!byId.has(edge.from)) {
      byId.set(edge.from, { id: edge.from, label: edge.from, shape: 'rect' });
    }
    if (!byId.has(edge.to)) {
      byId.set(edge.to, { id: edge.to, label: edge.to, shape: 'rect' });
    }
    indegree.set(edge.to, (indegree.get(edge.to) ?? 0) + 1);
    indegree.set(edge.from, indegree.get(edge.from) ?? 0);
    if (!children.has(edge.from)) {
      children.set(edge.from, []);
    }
    children.get(edge.from)!.push(edge);
  }

  const build = (id: string, edgeLabel: string | undefined, seen: Set<string>): FlowTreeNode => {
    const node = byId.get(id) ?? { id, label: id, shape: 'rect' as NodeShape };
    const repeated = seen.has(id);
    const treeNode: FlowTreeNode = {
      id,
      label: node.label,
      shape: node.shape,
      edgeLabel,
      repeated,
      children: [],
    };
    if (repeated) {
      return treeNode;
    }
    seen.add(id);
    for (const edge of children.get(id) ?? []) {
      treeNode.children.push(build(edge.to, edge.label, seen));
    }
    return treeNode;
  };

  const roots = parsed.nodes.filter((n) => (indegree.get(n.id) ?? 0) === 0).map((n) => n.id);
  const rootIds = roots.length > 0 ? roots : parsed.nodes.slice(0, 1).map((n) => n.id);
  return rootIds.map((id) => build(id, undefined, new Set()));
}

/** Lay a tree view out as indented Unicode lines. */
export function renderFlowTree(roots: FlowTreeNode[]): string[] {
  const lines: string[] = [];
  const emit = (node: FlowTreeNode, prefix: string, isLast: boolean): void => {
    const edge = node.edgeLabel ? `(${node.edgeLabel}) ` : '';
    lines.push(`${prefix}${isLast ? '└─▶ ' : '├─▶ '}${edge}${node.label}${node.repeated ? '  ↻' : ''}`);
    const childPrefix = prefix + (isLast ? '    ' : '│   ');
    node.children.forEach((child, index) => emit(child, childPrefix, index === node.children.length - 1));
  };

  for (const root of roots) {
    if (roots.length === 1) {
      lines.push(root.label);
      root.children.forEach((child, index) => emit(child, '', index === root.children.length - 1));
    } else {
      emit(root, '', root === roots[roots.length - 1]);
    }
  }
  return lines;
}

/**
 * Render Mermaid flowchart source as terminal text. Falls back to the trimmed
 * source when the input is not a recognizable flowchart.
 */
export function renderMermaidAscii(source: string): string {
  const parsed = parseMermaidFlowchart(source);
  if (parsed.nodes.length === 0) {
    return (source ?? '').trim();
  }
  return renderFlowTree(buildFlowTree(parsed)).join('\n');
}
