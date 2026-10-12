/**
 * Render a parsed Mermaid flowchart as a self-contained SVG — deterministically,
 * with no browser and no layout engine. The SVG is the input to `raster.ts`
 * (SVG → PNG, via `@resvg/resvg-js`) for the terminal-image path, and is also
 * useful on its own (open in a browser, embed in docs).
 *
 * The layout is a simple top-down tree: leaves are placed left-to-right, an
 * internal node is centered over its children, and edges are orthogonal with an
 * arrowhead. Long labels widen the box rather than wrapping.
 */

import { buildFlowTree, type FlowTreeNode, type NodeShape, type ParsedFlowchart } from './ascii.js';

export interface FlowchartSvg {
  svg: string;
  width: number;
  height: number;
  /** Number of nodes drawn (0 for an empty/unparsable diagram). */
  nodes: number;
}

export interface SvgTheme {
  background: string;
  text: string;
  edge: string;
  edgeLabel: string;
  rectFill: string;
  rectStroke: string;
  decisionFill: string;
  decisionStroke: string;
  roundFill: string;
  roundStroke: string;
}

const THEME: SvgTheme = {
  background: '#ffffff',
  text: '#0f172a',
  edge: '#64748b',
  edgeLabel: '#475569',
  rectFill: '#f8fafc',
  rectStroke: '#94a3b8',
  decisionFill: '#eef2ff',
  decisionStroke: '#6366f1',
  roundFill: '#f0fdf4',
  roundStroke: '#16a34a',
};

const MARGIN = 28;
const HGAP = 30;
const VGAP = 74;
const BOX_H = 42;
const ROW_H = BOX_H + VGAP;
const FONT = 'DejaVu Sans, Liberation Sans, Helvetica, Arial, sans-serif';

function escapeXml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

function boxWidth(node: FlowTreeNode): number {
  const base = Math.min(480, Math.max(104, node.label.length * 8.8 + 34));
  return node.shape === 'diamond' ? base + 34 : base;
}

function boxHeight(node: FlowTreeNode): number {
  return node.shape === 'diamond' ? BOX_H + 16 : BOX_H;
}

interface Placed {
  node: FlowTreeNode;
  x: number;
  y: number;
  w: number;
  h: number;
}

interface PlacedEdge {
  from: Placed;
  to: Placed;
  label?: string;
}

/** Options for the SVG generator. */
export interface SvgOptions {
  theme?: Partial<SvgTheme>;
  /** Include the `module_doc` title as a caption (default: none). */
  title?: string;
}

/** Render a parsed flowchart to SVG text. */
export function renderFlowchartSvg(parsed: ParsedFlowchart, options: SvgOptions = {}): FlowchartSvg {
  const theme = { ...THEME, ...options.theme };
  const roots = buildFlowTree(parsed);
  if (roots.length === 0) {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="200" height="60"><rect width="200" height="60" fill="${theme.background}"/><text x="100" y="34" text-anchor="middle" font-family="${FONT}" font-size="14" fill="${theme.text}">(empty diagram)</text></svg>`;
    return { svg, width: 200, height: 60, nodes: 0 };
  }

  const placed: Placed[] = [];
  const edges: PlacedEdge[] = [];
  const titleHeight = options.title ? 34 : 0;
  let cursorX = MARGIN;
  let maxDepth = 0;
  let rightMost = 0;

  const place = (node: FlowTreeNode, depth: number): Placed => {
    maxDepth = Math.max(maxDepth, depth);
    const w = boxWidth(node);
    const h = boxHeight(node);
    const y = titleHeight + MARGIN + depth * ROW_H;
    if (node.children.length === 0) {
      const x = cursorX;
      cursorX += w + HGAP;
      const p: Placed = { node, x, y, w, h };
      placed.push(p);
      rightMost = Math.max(rightMost, x + w);
      return p;
    }
    const children = node.children.map((child) => place(child, depth + 1));
    const first = children[0];
    const last = children[children.length - 1];
    const center = (first.x + first.w / 2 + last.x + last.w / 2) / 2;
    const x = center - w / 2;
    const p: Placed = { node, x, y, w, h };
    placed.push(p);
    rightMost = Math.max(rightMost, x + w);
    for (const child of children) {
      edges.push({ from: p, to: child, label: child.node.edgeLabel });
    }
    return p;
  };

  for (const root of roots) {
    place(root, 0);
  }

  const width = Math.max(240, rightMost + MARGIN);
  const height = titleHeight + MARGIN * 2 + (maxDepth + 1) * ROW_H - VGAP;

  const parts: string[] = [];
  parts.push(`<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">`);
  parts.push('<defs>');
  parts.push(`<marker id="dc-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="${theme.edge}"/></marker>`);
  parts.push('</defs>');
  parts.push(`<rect width="${width}" height="${height}" fill="${theme.background}"/>`);
  if (options.title) {
    parts.push(`<text x="${MARGIN}" y="24" font-family="${FONT}" font-size="15" font-weight="600" fill="${theme.text}">${escapeXml(options.title)}</text>`);
  }

  // Edges first so nodes sit on top.
  for (const edge of edges) {
    const x1 = edge.from.x + edge.from.w / 2;
    const y1 = edge.from.y + edge.from.h;
    const x2 = edge.to.x + edge.to.w / 2;
    const y2 = edge.to.y;
    const midY = y1 + (y2 - y1) / 2;
    const path = x1 === x2
      ? `M ${x1} ${y1} L ${x2} ${y2}`
      : `M ${x1} ${y1} L ${x1} ${midY} L ${x2} ${midY} L ${x2} ${y2}`;
    parts.push(`<path d="${path}" fill="none" stroke="${theme.edge}" stroke-width="1.6" marker-end="url(#dc-arrow)"/>`);
    if (edge.label) {
      const lx = x1 === x2 ? x1 + 8 : (x1 + x2) / 2;
      const ly = midY - 6;
      parts.push(`<text x="${lx}" y="${ly}" text-anchor="middle" font-family="${FONT}" font-size="12" fill="${theme.edgeLabel}">${escapeXml(edge.label)}</text>`);
    }
  }

  for (const p of placed) {
    const cx = p.x + p.w / 2;
    const cy = p.y + p.h / 2;
    const shape = shapeElement(p, theme);
    parts.push(shape);
    parts.push(`<text x="${cx}" y="${cy + 5}" text-anchor="middle" font-family="${FONT}" font-size="14" fill="${theme.text}">${escapeXml(p.node.label)}</text>`);
  }

  parts.push('</svg>');
  return { svg: parts.join('\n'), width, height, nodes: placed.length };
}

function shapeElement(p: Placed, theme: SvgTheme): string {
  const shape: NodeShape = p.node.shape;
  const common = `stroke-width="1.6"`;
  if (shape === 'diamond') {
    const cx = p.x + p.w / 2;
    const cy = p.y + p.h / 2;
    const points = [
      `${cx},${p.y}`,
      `${p.x + p.w},${cy}`,
      `${cx},${p.y + p.h}`,
      `${p.x},${cy}`,
    ].join(' ');
    return `<polygon points="${points}" fill="${theme.decisionFill}" stroke="${theme.decisionStroke}" ${common}/>`;
  }
  if (shape === 'circle') {
    return `<ellipse cx="${p.x + p.w / 2}" cy="${p.y + p.h / 2}" rx="${p.w / 2}" ry="${p.h / 2}" fill="${theme.roundFill}" stroke="${theme.roundStroke}" ${common}/>`;
  }
  if (shape === 'round' || shape === 'stadium') {
    return `<rect x="${p.x}" y="${p.y}" width="${p.w}" height="${p.h}" rx="${p.h / 2}" fill="${theme.roundFill}" stroke="${theme.roundStroke}" ${common}/>`;
  }
  return `<rect x="${p.x}" y="${p.y}" width="${p.w}" height="${p.h}" rx="8" fill="${theme.rectFill}" stroke="${theme.rectStroke}" ${common}/>`;
}
