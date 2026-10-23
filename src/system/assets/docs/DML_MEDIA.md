# Media and HTML

Media is a workspace reference, not image bytes in DML. Analysis, display and
generation are DIFFERENT capabilities. Check the active model/host before promising
vision or image generation. Videos/audio are not generic DML image inputs.

## Vision input

```prolog
describe_image(Path, Text) :-
    read_media(Path, Image),
    attach_media(Image),
    with_tools([], task("Describe only the attached image.", string(Text))).
```

`read_media(Path, Media)` derives a reference `media(Path, Mime)`; it does not
load bytes into Prolog. `attach_media(Media)` adds a reference to harness memory;
the next vision-capable task resolves the bytes for the model. A fresh `prompt/N`
does NOT read that memory, so attaching then prompting loses the image.
Pass media paths explicitly into nested tools; their task memory starts fresh.

Use workspace images, not remote URLs/base64 embedded in clauses. PNG/JPEG/GIF/
WebP are the portable vision formats; SVG display and actual model input support
are host-dependent. A text-only model does not magically see image contents;
report the limitation or use an explicitly available conversion/vision capability.
Semantic judgments (`choose/rate/verify/probability`) are text-only.

## Display and save

`output(media("charts/plot.png"))` displays an existing image.
`output(media{svg: Svg})` displays inline SVG where the host supports it.
The authoring host `show_image(path)` displays a workspace image to the person;
it is not a call asking a model to analyse it.

`save_image(Media, OutPath)` copies a referenced image or writes inline SVG.
Create parent directories first. Display does not persist a file by itself, and
saving does not display it; use the appropriate separate operation.

## Image generation

`task("Description", media(ImagePath))` selects an image-generation path rather
than a normal text agent turn. It requires a CONFIGURED image backend, otherwise
the runtime reports generation unavailable. Selecting a text/vision model via
`with_model` does not install an image generator. When configured, the output is
a workspace path; display it separately with `output(media(ImagePath))`.

The current browser session embedding does not configure an image backend by
default. Prefer deterministic SVG or a supported Python image workflow if it
fits the request, but do not mislabel that as model image generation.

## HTML miniapps

Browser hosts can display `output(html("miniapp/index.html"))`; the file must
exist. Write the complete HTML file once, with inline assets or allowed relative
assets. The renderer sandboxes scripts and does not grant app DOM/credential access.
`window.dc.call('double(21, X)')` calls a bounded goal in the rendering owner.
The promise returns `{ok, output, bindings, error?}`. Keep called predicates bounded
and deterministic. A reference to a predicate is not a replacement for defining it.

The browser miniapp bridge also supports `readFile`, `readBytes`, `list`, `stat`
and `fetch` for confined workspace paths; byte reads are capped. These are browser
renderer APIs, not DML host tool names. A plain HTML answer fence is not a trusted
workspace page. Do not assume these APIs exist in terminal or other embeddings.
