# rstudioSvgViewer

`rstudioSvgViewer` is an R package that provides an RStudio addin to preview SVG files in a Shiny gadget.

## Install

```r
# install.packages("remotes")
remotes::install_github("<org>/rstudio_svg_viewer")
```

## Use the addin

1. Open RStudio.
2. Go to **Addins** -> **SVG Viewer**.
3. If the active editor file is an `.svg`, it opens directly.
4. Otherwise, select an SVG file in the picker.

## Programmatic launch

```r
rstudioSvgViewer::launch_svg_viewer()
```
