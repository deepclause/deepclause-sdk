# Images and video in DML

How DeepClause handles media (images now; video later), and the decisions behind
the implementation. Status: **image input, inline terminal display, and image
generation implemented**; video is planned.

## Principles

* **Media are workspace files.** DML carries a *reference*, never the bytes.
  Base64 in clauses would blow up the "small, inspectable" module model and break
  export/history. This is the `script_file/2` convention applied to media.
* **Reference-only memory.** Conversation memory may hold media, but only as
  `media(Path, MimeType)`. Bytes are resolved from the workspace at call time.
* **No vision in judgments.** `choose`/`rate`/`verify`/`probability` stay text
  only.
* **Reuse pi's models and credentials.** Vision input and image generation run on
  the active pi backend; no separate keys.

## Representation

```prolog
media(Path, MimeType).          % workspace-relative path + mime; the reference
```

`read_media(+Path, -Media)` derives the mime from the extension (PNG/JPEG/GIF/
WebP) and fails for other types. `attach_media(+Media)` appends a user memory
message carrying `media: [media(Path, Mime)]`; the model then sees the image.

Host side:

```ts
interface MediaRef { path: string; mimeType: string; }
interface LLMBackendMedia { mimeType: string; data: string; }   // base64, resolved
```

`MemoryMessage.media?: MediaRef[]` (reference-only) and
`LLMBackendMessage.media?: LLMBackendMedia[]` (bytes for the call).

## Images in (vision)

`task/N` runs an agent loop over the conversation memory. Media references in
memory are resolved to base64 (mime allowlist, size cap) and attached as pi
`ImageContent` blocks on the corresponding user message. A text-only model
receives the text and a `[attached ...]` note, not the image block, when
`media.blockImages` is set.

## Images out (generation)

Generation is a **typed task output**:

```prolog
% text-to-image: bind Img to the generated, workspace-relative path
task("a red fox in snow, 512x512", media(Img)).

% image-to-image: attach the input, then generate
read_media("a.png", In), attach_media(In),
task("turn this into a watercolour", media(Out)).
```

A `media(X)` output var short-circuits the text agent loop: the host resolves the
first available pi image model (or the configured `media.imageModel`), calls
`generateImages`, writes the bytes under `media.dir` (default
`.deepclause/media`), and binds `X` to the path. It does **not** display the
image — call `output(media(X))` for that. Same credentials and model catalog as
chat; no separate keys. A task with a `media` output and no image backend fails
with a clear error.

## Rendering and saving

`output/1` presents an image instead of text:

```prolog
output(media("charts/plot.png")).                 % a workspace file (mime inferred)
output(media{svg: Svg}).                          % inline SVG (rasterized via resvg)
output(media{data: Base64, mime: "image/png"}).   % inline bytes
```

The host resolves it to a PNG (resvg for SVG; PNG passthrough; JPEG/WebP/GIF via
an optional `sharp`) and renders it inline on Kitty/Ghostty/WezTerm/iTerm2.
On other terminals it prints an OSC-8 hyperlinked path and (unless `--no-open`)
hands the file to `xdg-open`/`open`. In `--json` the same event is emitted as
`{"type":"media","media":{"path":…,"mimeType":…,"data":"<png base64>"}}`.

In the REPL: `/show <path>` displays an existing image, `/gen <prompt>` generates
one and displays it. The authoring agent has `show_image` and `generate_image`
tools.

`save_image(+Media, +OutPath)` writes a reference to a workspace file
(file copy, or an inline SVG written verbatim); parent directories must exist.

## Video (deferred)

pi has no video content type. The pattern is: sample frames with `ffmpeg` (via
`bash`), treat each frame as an image, optionally transcribe audio, and assemble
output with `ffmpeg`. No core video type.

## Guardrails

* Workspace path isolation (same resolver as scripts).
* MIME allowlist (PNG/JPEG/GIF/WebP) and a byte-size cap; no remote URLs.
* Media is recorded by `(path, mime)` only; never bytes, in facts/history.
* Text-only fallback fails clearly rather than silently dropping the image.

## Config (planned)

The MVP uses constants (PNG/JPEG/GIF/WebP allowlist, 10 MiB cap in
`src/system/runtime/media.ts`). A `media` config section is planned:

```json
"media": { "dir": ".deepclause/media", "maxBytes": 10485760, "blockImages": false }
```
