# rstudioSvgViewer

`rstudioSvgViewer` is an R package that adds an **RStudio Addin** for viewing SVG files in a Shiny gadget.

## What It Does

- Opens SVG files directly inside RStudio.
- Uses the active editor file if it is an `.svg`.
- Falls back to a file picker when no active SVG is available.
- Provides simple viewer controls: zoom in, zoom out, fit, and reset.

## Requirements

- R >= 4.1
- RStudio (Desktop or Server) for Addins UI
- Internet access the first time you install from GitHub

## Installation (from GitHub)

After you upload this project to GitHub, install it with:

```r
install.packages("remotes")
remotes::install_github("jesgj/rstudio_svg_viewer")
```
If you publish releases/tags, users can install a specific version:

```r
remotes::install_github("YOUR_GITHUB_USERNAME/rstudio_svg_viewer@v0.1.0")
```

## First Run

1. Restart R (recommended after install).
2. Open RStudio.
3. Go to **Addins** -> **SVG Viewer**.

## How to Use

1. Open an `.svg` file in RStudio (optional but recommended).
2. Launch **Addins** -> **SVG Viewer**.
3. If an active `.svg` is open, it is loaded automatically.
4. Otherwise, choose an SVG file from the file picker.
5. Use controls in the gadget title bar:
   - `+` zoom in
   - `-` zoom out
   - `Fit` fit image to viewport
   - `Reset` reset to initial view

## Programmatic Launch

You can also launch the addin from code:

```r
rstudioSvgViewer::launch_svg_viewer()
```

Or provide a file directly:

```r
rstudioSvgViewer::launch_svg_viewer("path/to/file.svg")
```

## Troubleshooting

- Addin does not appear in menu:
  - Restart RStudio.
  - Reinstall the package.
- Error about `rstudioapi` or non-interactive session:
  - Run the addin inside RStudio, not plain `R` in a terminal.
- Install fails with compilation errors on Linux:
  - Install system build tools (`build-essential`, compilers, headers), then retry.
- Install fails due library permissions:
  - Install to a user-writable library via `.libPaths()` or run R with proper permissions.

## Uninstall

```r
remove.packages("rstudioSvgViewer")
```
