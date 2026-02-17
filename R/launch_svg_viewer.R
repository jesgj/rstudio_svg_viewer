#' Launch the SVG viewer addin
#'
#' Opens a Shiny gadget that renders an SVG file inside RStudio.
#' If the active editor document is an SVG, it is used automatically.
#' Otherwise, a file picker is shown.
#'
#' @param file Optional path to an SVG file. If supplied, the picker is skipped.
#' @return Invisibly returns `TRUE` when closed with Done and `FALSE` when cancelled.
#' @export
launch_svg_viewer <- function(file = NULL) {
  if (!interactive()) {
    svg_viewer_abort("launch_svg_viewer() must be run in an interactive session.")
  }

  path <- tryCatch(
    {
      if (is.null(file)) {
        resolve_svg_path()
      } else {
        normalize_and_validate_svg_path(file)
      }
    },
    svg_viewer_error = function(err) {
      show_svg_viewer_notification(conditionMessage(err))
      return(NULL)
    }
  )

  if (is.null(path)) {
    return(invisible(FALSE))
  }

  svg_text <- tryCatch(
    read_svg_text(path),
    svg_viewer_error = function(err) {
      show_svg_viewer_notification(conditionMessage(err))
      return(NULL)
    }
  )

  if (is.null(svg_text)) {
    return(invisible(FALSE))
  }

  ui <- build_svg_gadget_ui(svg_text = svg_text, file_label = basename(path))
  server <- run_svg_gadget_server()

  viewer <- build_dialog_viewer(file_label = basename(path))

  run_gadget_compat(ui = ui, server = server, viewer = viewer)
}

build_dialog_viewer <- function(file_label, dialog_viewer_fn = shiny::dialogViewer) {
  title_text <- paste0("SVG Viewer - ", file_label)
  formal_names <- names(formals(dialog_viewer_fn))
  if (is.null(formal_names)) {
    formal_names <- character(0)
  }

  args <- list(width = 980, height = 760)
  if ("title" %in% formal_names) {
    args$title <- title_text
  } else if ("dialogName" %in% formal_names) {
    args$dialogName <- title_text
  }

  do.call(dialog_viewer_fn, args)
}

run_gadget_compat <- function(ui, server, viewer, run_gadget_fn = shiny::runGadget) {
  formal_names <- names(formals(run_gadget_fn))
  if (is.null(formal_names)) {
    formal_names <- character(0)
  }

  args <- list()
  if ("ui" %in% formal_names) {
    args$ui <- ui
  } else if ("app" %in% formal_names) {
    args$app <- ui
  } else {
    args[[1]] <- ui
  }

  if ("server" %in% formal_names) {
    args$server <- server
  } else {
    args[[length(args) + 1]] <- server
  }

  if ("viewer" %in% formal_names) {
    args$viewer <- viewer
  } else {
    args[[length(args) + 1]] <- viewer
  }

  do.call(run_gadget_fn, args)
}

run_svg_gadget_server <- function() {
  function(input, output, session) {
    shiny::observeEvent(input$zoom_in, {
      session$sendCustomMessage("svg_viewer_action", list(action = "zoom_in"))
    })

    shiny::observeEvent(input$zoom_out, {
      session$sendCustomMessage("svg_viewer_action", list(action = "zoom_out"))
    })

    shiny::observeEvent(input$fit_view, {
      session$sendCustomMessage("svg_viewer_action", list(action = "fit"))
    })

    shiny::observeEvent(input$reset_view, {
      session$sendCustomMessage("svg_viewer_action", list(action = "reset"))
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(TRUE))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(invisible(FALSE))
    })
  }
}

build_svg_gadget_ui <- function(svg_text, file_label) {
  miniUI::miniPage(
    miniUI::gadgetTitleBar(
      title = paste0("SVG Viewer: ", file_label),
      left = miniUI::miniTitleBarButton("fit_view", "Fit"),
      right = shiny::tagList(
        miniUI::miniTitleBarButton("zoom_out", "-"),
        miniUI::miniTitleBarButton("zoom_in", "+"),
        miniUI::miniTitleBarButton("reset_view", "Reset")
      )
    ),
    miniUI::miniContentPanel(
      htmltools::tags$div(
        id = "svg_viewer_root",
        htmltools::tags$div(
          id = "svg_viewport",
          htmltools::tags$div(id = "svg_stage", htmltools::HTML(svg_text))
        )
      ),
      htmltools::tags$style(htmltools::HTML(svg_viewer_css())),
      htmltools::tags$script(htmltools::HTML(svg_viewer_js()))
    )
  )
}

resolve_svg_path <- function(active_doc_context = NULL, picker = NULL) {
  context <- active_doc_context
  if (is.null(context)) {
    context <- get_active_document_context()
  }

  candidate <- ""
  if (!is.null(context$path) && is.character(context$path) && nzchar(context$path)) {
    if (is_svg_extension(context$path)) {
      candidate <- context$path
    }
  }

  if (!nzchar(candidate)) {
    picker_fn <- picker
    if (is.null(picker_fn)) {
      picker_fn <- function() {
        rstudioapi::selectFile(caption = "Select an SVG file", existing = TRUE)
      }
    }

    chosen <- picker_fn()
    if (length(chosen) == 0 || is.na(chosen[[1]]) || !nzchar(chosen[[1]])) {
      svg_viewer_abort("No SVG file selected.")
    }
    candidate <- chosen[[1]]
  }

  normalize_and_validate_svg_path(candidate)
}

get_active_document_context <- function() {
  if (!rstudioapi::isAvailable()) {
    return(list(path = ""))
  }

  tryCatch(
    rstudioapi::getActiveDocumentContext(),
    error = function(...) {
      list(path = "")
    }
  )
}

normalize_and_validate_svg_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    svg_viewer_abort("A valid file path is required.")
  }

  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (!is_svg_extension(normalized_path)) {
    svg_viewer_abort("Selected file must have a .svg extension.")
  }

  if (!file.exists(normalized_path)) {
    svg_viewer_abort("Selected SVG file does not exist.")
  }

  if (file.access(normalized_path, mode = 4) != 0) {
    svg_viewer_abort("Selected SVG file is not readable.")
  }

  normalized_path
}

is_svg_extension <- function(path) {
  tolower(tools::file_ext(path)) == "svg"
}

read_svg_text <- function(path) {
  valid_path <- normalize_and_validate_svg_path(path)

  lines <- tryCatch(
    readLines(valid_path, warn = FALSE, encoding = "UTF-8"),
    error = function(...) {
      svg_viewer_abort("Failed to read the SVG file.")
    }
  )

  svg_text <- paste(lines, collapse = "\n")
  if (!nzchar(svg_text)) {
    svg_viewer_abort("Selected SVG file is empty.")
  }

  if (!grepl("<svg\\b", svg_text, ignore.case = TRUE, perl = TRUE)) {
    svg_viewer_abort("Selected file does not contain a valid <svg> root element.")
  }

  enc2utf8(svg_text)
}

show_svg_viewer_notification <- function(text) {
  tryCatch(
    shiny::showNotification(text, type = "error", duration = 8),
    error = function(...) {
      base::message(text)
      invisible(NULL)
    }
  )
}

svg_viewer_abort <- function(message) {
  stop(structure(list(message = message), class = c("svg_viewer_error", "error", "condition")))
}

svg_viewer_css <- function() {
  paste(
    "html, body, #svg_viewer_root {",
    "height: 100%;",
    "margin: 0;",
    "background: #f6f7f9;",
    "}",
    "#svg_viewport {",
    "position: relative;",
    "height: 100%;",
    "overflow: hidden;",
    "border: 1px solid #d7dde5;",
    "background: linear-gradient(45deg, #ffffff 25%, #f0f3f8 25%, #f0f3f8 50%, #ffffff 50%, #ffffff 75%, #f0f3f8 75%, #f0f3f8 100%);",
    "background-size: 24px 24px;",
    "cursor: grab;",
    "user-select: none;",
    "touch-action: none;",
    "}",
    "#svg_stage {",
    "position: absolute;",
    "inset: 0;",
    "overflow: visible;",
    "}",
    "#svg_stage svg {",
    "position: absolute;",
    "left: 0;",
    "top: 0;",
    "max-width: none;",
    "max-height: none;",
    "transform-origin: 0 0;",
    "}",
    sep = "\n"
  )
}

svg_viewer_js <- function() {
  paste(
    "(function() {",
    "  var state = {",
    "    scale: 1,",
    "    tx: 0,",
    "    ty: 0,",
    "    initScale: 1,",
    "    initTx: 0,",
    "    initTy: 0,",
    "    ready: false,",
    "    dragging: false,",
    "    dragOriginX: 0,",
    "    dragOriginY: 0,",
    "    dragStartTx: 0,",
    "    dragStartTy: 0",
    "  };",
    "",
    "  function getContainer() { return document.getElementById('svg_viewport'); }",
    "  function getSvg() {",
    "    var stage = document.getElementById('svg_stage');",
    "    if (!stage) return null;",
    "    return stage.querySelector('svg');",
    "  }",
    "",
    "  function parseLength(rawValue) {",
    "    if (rawValue === null || rawValue === undefined) return 0;",
    "    var value = String(rawValue).trim();",
    "    if (!value || /%$/.test(value)) return 0;",
    "    var numeric = Number.parseFloat(value);",
    "    if (!isFinite(numeric) || numeric <= 0) return 0;",
    "    return numeric;",
    "  }",
    "",
    "  function applyTransform() {",
    "    var svg = getSvg();",
    "    if (!svg) return;",
    "    svg.style.transform = 'translate(' + state.tx + 'px,' + state.ty + 'px) scale(' + state.scale + ')';",
    "  }",
    "",
    "  function getSvgSize(svg) {",
    "    var width = 0;",
    "    var height = 0;",
    "    var previousTransform = svg.style.transform;",
    "    svg.style.transform = '';",
    "",
    "    try {",
    "      if (svg.viewBox && svg.viewBox.baseVal && svg.viewBox.baseVal.width > 0 && svg.viewBox.baseVal.height > 0) {",
    "        width = svg.viewBox.baseVal.width;",
    "        height = svg.viewBox.baseVal.height;",
    "      }",
    "",
    "      if (!(width > 0 && height > 0)) {",
    "        width = parseLength(svg.getAttribute('width'));",
    "        height = parseLength(svg.getAttribute('height'));",
    "      }",
    "",
    "      if (!(width > 0 && height > 0) && svg.width && svg.height) {",
    "        var baseWidth = Number(svg.width.baseVal.value);",
    "        var baseHeight = Number(svg.height.baseVal.value);",
    "        if (isFinite(baseWidth) && baseWidth > 0 && isFinite(baseHeight) && baseHeight > 0) {",
    "          width = baseWidth;",
    "          height = baseHeight;",
    "        }",
    "      }",
    "",
    "      if (!(width > 0 && height > 0)) {",
    "        var box = svg.getBBox();",
    "        if (box && box.width > 0 && box.height > 0) {",
    "          width = box.width;",
    "          height = box.height;",
    "        }",
    "      }",
    "",
    "      if (!(width > 0 && height > 0)) {",
    "        var rect = svg.getBoundingClientRect();",
    "        if (rect && rect.width > 0 && rect.height > 0) {",
    "          width = rect.width;",
    "          height = rect.height;",
    "        }",
    "      }",
    "    } catch (err) {",
    "      width = 600;",
    "      height = 400;",
    "    } finally {",
    "      svg.style.transform = previousTransform;",
    "    }",
    "",
    "    if (!isFinite(width) || width <= 0) width = 600;",
    "    if (!isFinite(height) || height <= 0) height = 400;",
    "",
    "    return { width: width, height: height };",
    "  }",
    "",
    "  function fitToView(storeAsInitial) {",
    "    var container = getContainer();",
    "    var svg = getSvg();",
    "    if (!container || !svg) return;",
    "",
    "    var size = getSvgSize(svg);",
    "    var pad = 24;",
    "    var cw = container.clientWidth;",
    "    var ch = container.clientHeight;",
    "    if (cw <= 0 || ch <= 0) return;",
    "",
    "    var scale = Math.min((cw - pad) / size.width, (ch - pad) / size.height);",
    "    if (!isFinite(scale) || scale <= 0) scale = 1;",
    "",
    "    state.scale = scale;",
    "    state.tx = (cw - size.width * scale) / 2;",
    "    state.ty = (ch - size.height * scale) / 2;",
    "",
    "    if (storeAsInitial) {",
    "      state.initScale = state.scale;",
    "      state.initTx = state.tx;",
    "      state.initTy = state.ty;",
    "      state.ready = true;",
    "    }",
    "",
    "    applyTransform();",
    "  }",
    "",
    "  function zoomAt(multiplier, cx, cy) {",
    "    var container = getContainer();",
    "    var svg = getSvg();",
    "    if (!container || !svg || !state.ready) return;",
    "",
    "    var oldScale = state.scale;",
    "    var newScale = oldScale * multiplier;",
    "    newScale = Math.max(0.05, Math.min(newScale, 20));",
    "",
    "    state.tx = cx - (cx - state.tx) * (newScale / oldScale);",
    "    state.ty = cy - (cy - state.ty) * (newScale / oldScale);",
    "    state.scale = newScale;",
    "    applyTransform();",
    "  }",
    "",
    "  function zoom(multiplier) {",
    "    var container = getContainer();",
    "    if (!container || !state.ready) return;",
    "    zoomAt(multiplier, container.clientWidth / 2, container.clientHeight / 2);",
    "  }",
    "",
    "  function resetView() {",
    "    if (!state.ready) return;",
    "    state.scale = state.initScale;",
    "    state.tx = state.initTx;",
    "    state.ty = state.initTy;",
    "    applyTransform();",
    "  }",
    "",
    "  function startDrag(event) {",
    "    var container = getContainer();",
    "    if (!container || !state.ready) return;",
    "    if (event.button !== undefined && event.button !== 0) return;",
    "    state.dragging = true;",
    "    state.dragOriginX = event.clientX;",
    "    state.dragOriginY = event.clientY;",
    "    state.dragStartTx = state.tx;",
    "    state.dragStartTy = state.ty;",
    "    container.style.cursor = 'grabbing';",
    "    if (container.setPointerCapture && event.pointerId !== undefined) {",
    "      try { container.setPointerCapture(event.pointerId); } catch (err) {}",
    "    }",
    "    event.preventDefault();",
    "  }",
    "",
    "  function dragMove(event) {",
    "    if (!state.dragging) return;",
    "    state.tx = state.dragStartTx + (event.clientX - state.dragOriginX);",
    "    state.ty = state.dragStartTy + (event.clientY - state.dragOriginY);",
    "    applyTransform();",
    "    event.preventDefault();",
    "  }",
    "",
    "  function stopDrag(event) {",
    "    var container = getContainer();",
    "    if (!container) return;",
    "    if (state.dragging && container.releasePointerCapture && event && event.pointerId !== undefined) {",
    "      try { container.releasePointerCapture(event.pointerId); } catch (err) {}",
    "    }",
    "    state.dragging = false;",
    "    container.style.cursor = 'grab';",
    "  }",
    "",
    "  function onWheel(event) {",
    "    var container = getContainer();",
    "    if (!container || !state.ready) return;",
    "    var rect = container.getBoundingClientRect();",
    "    var cx = event.clientX - rect.left;",
    "    var cy = event.clientY - rect.top;",
    "    var multiplier = event.deltaY < 0 ? 1.1 : (1 / 1.1);",
    "    zoomAt(multiplier, cx, cy);",
    "    event.preventDefault();",
    "  }",
    "",
    "  function bindInteractions() {",
    "    var container = getContainer();",
    "    if (!container || container.__svgViewerBound) return;",
    "    container.addEventListener('pointerdown', startDrag);",
    "    container.addEventListener('pointermove', dragMove);",
    "    container.addEventListener('pointerup', stopDrag);",
    "    container.addEventListener('pointercancel', stopDrag);",
    "    container.addEventListener('wheel', onWheel, { passive: false });",
    "    container.__svgViewerBound = true;",
    "  }",
    "",
    "  function initialize() {",
    "    bindInteractions();",
    "    fitToView(true);",
    "  }",
    "",
    "  window.addEventListener('resize', function() { fitToView(false); });",
    "",
    "  if (window.Shiny && window.Shiny.addCustomMessageHandler) {",
    "    window.Shiny.addCustomMessageHandler('svg_viewer_action', function(msg) {",
    "      if (!msg || !msg.action) return;",
    "      if (msg.action === 'zoom_in') zoom(1.2);",
    "      if (msg.action === 'zoom_out') zoom(1 / 1.2);",
    "      if (msg.action === 'fit') fitToView(false);",
    "      if (msg.action === 'reset') resetView();",
    "    });",
    "  }",
    "",
    "  if (document.readyState === 'loading') {",
    "    document.addEventListener('DOMContentLoaded', initialize);",
    "  } else {",
    "    initialize();",
    "  }",
    "})();",
    sep = "\n"
  )
}
