# mred-format-viewer

A terminal UI viewer for DrRacket's WXME binary file format.

WXME is the format DrRacket uses to save files that contain non-text elements like images, comment boxes, and embedded editors. These files can't be read with standard text tools. This viewer decodes WXME files and displays them in a scrollable terminal interface, showing text content with annotated placeholders for non-text elements. You can expand any placeholder to see structural details about the embedded snip.

## Installation

```sh
raco pkg install mred-format-viewer
```

Requires the `kettle-lib` and `wxme-lib` packages (installed automatically as dependencies).

## Usage

```sh
racket -l mred-format-viewer -- <file.rkt>
```

Or if installed as a package:

```sh
racket -l mred-format-viewer -- path/to/file.wxme
```

## Key Bindings

| Key | Action |
|-----|--------|
| `j` / Down | Move cursor down |
| `k` / Up | Move cursor up |
| `Space` / PgDn | Page down |
| `b` / PgUp | Page up |
| `g` | Go to top |
| `G` | Go to bottom |
| `h` / Left | Scroll left |
| `l` / Right | Scroll right |
| `Enter` | Toggle structural detail for snip under cursor |
| `Tab` | Jump to next snip placeholder |
| `Shift-Tab` | Jump to previous snip placeholder |
| `q` | Quit |

## Display

- **Text content** is shown as-is
- **Non-text snips** (images, nested editors, etc.) appear as cyan placeholders like `[image (378 bytes)]` or `[nested editor]`
- **Structural details** can be expanded on any snip placeholder by pressing Enter, showing metadata like image dimensions, data size, and content previews
- The header bar shows the filename, line count, snip count, and file size
- The footer bar shows scroll position and key binding hints

## Running Tests

```sh
raco test -y tests/
```

## License

MIT
