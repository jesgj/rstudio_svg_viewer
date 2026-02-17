test_that("resolve_svg_path uses active SVG document path", {
  svg_path <- tempfile(fileext = ".svg")
  writeLines("<svg></svg>", svg_path)

  out <- rstudioSvgViewer:::resolve_svg_path(
    active_doc_context = list(path = svg_path),
    picker = function() stop("picker should not be called")
  )

  expect_equal(out, normalizePath(svg_path, winslash = "/", mustWork = FALSE))
})

test_that("resolve_svg_path falls back to picker when active file is not svg", {
  svg_path <- tempfile(fileext = ".svg")
  writeLines("<svg></svg>", svg_path)

  out <- rstudioSvgViewer:::resolve_svg_path(
    active_doc_context = list(path = tempfile(fileext = ".txt")),
    picker = function() svg_path
  )

  expect_equal(out, normalizePath(svg_path, winslash = "/", mustWork = FALSE))
})

test_that("resolve_svg_path errors when no file is selected", {
  expect_error(
    rstudioSvgViewer:::resolve_svg_path(
      active_doc_context = list(path = ""),
      picker = function() ""
    ),
    class = "svg_viewer_error",
    regexp = "No SVG file selected"
  )
})

test_that("normalize_and_validate_svg_path rejects non-svg extension", {
  txt_path <- tempfile(fileext = ".txt")
  writeLines("plain text", txt_path)

  expect_error(
    rstudioSvgViewer:::normalize_and_validate_svg_path(txt_path),
    class = "svg_viewer_error",
    regexp = "must have a .svg extension"
  )
})

test_that("normalize_and_validate_svg_path rejects missing files", {
  missing_path <- file.path(tempdir(), "missing-file.svg")

  expect_error(
    rstudioSvgViewer:::normalize_and_validate_svg_path(missing_path),
    class = "svg_viewer_error",
    regexp = "does not exist"
  )
})

test_that("read_svg_text rejects files without svg root", {
  bad_svg <- tempfile(fileext = ".svg")
  writeLines("<html><body>not svg</body></html>", bad_svg)

  expect_error(
    rstudioSvgViewer:::read_svg_text(bad_svg),
    class = "svg_viewer_error",
    regexp = "does not contain a valid <svg> root element"
  )
})

test_that("read_svg_text returns file contents", {
  good_svg <- tempfile(fileext = ".svg")
  writeLines(c("<svg>", "  <rect width='10' height='10' />", "</svg>"), good_svg)

  out <- rstudioSvgViewer:::read_svg_text(good_svg)
  expect_true(grepl("<svg>", out, fixed = TRUE))
})

test_that("build_svg_gadget_ui includes expected control ids", {
  ui <- rstudioSvgViewer:::build_svg_gadget_ui("<svg></svg>", "demo.svg")
  html <- as.character(ui)

  expect_true(grepl("zoom_in", html, fixed = TRUE))
  expect_true(grepl("zoom_out", html, fixed = TRUE))
  expect_true(grepl("fit_view", html, fixed = TRUE))
  expect_true(grepl("reset_view", html, fixed = TRUE))
  expect_true(grepl("svg_viewport", html, fixed = TRUE))
})

test_that("build_dialog_viewer uses title when available", {
  captured <- NULL
  fake_dialog_viewer <- function(title = NULL, width, height) {
    captured <<- list(title = title, width = width, height = height)
    function(...) NULL
  }

  out <- rstudioSvgViewer:::build_dialog_viewer(
    file_label = "demo.svg",
    dialog_viewer_fn = fake_dialog_viewer
  )

  expect_true(is.function(out))
  expect_equal(captured$title, "SVG Viewer - demo.svg")
  expect_equal(captured$width, 980)
  expect_equal(captured$height, 760)
})

test_that("build_dialog_viewer falls back to dialogName", {
  captured <- NULL
  fake_dialog_viewer <- function(dialogName = NULL, width, height) {
    captured <<- list(dialogName = dialogName, width = width, height = height)
    function(...) NULL
  }

  out <- rstudioSvgViewer:::build_dialog_viewer(
    file_label = "legacy.svg",
    dialog_viewer_fn = fake_dialog_viewer
  )

  expect_true(is.function(out))
  expect_equal(captured$dialogName, "SVG Viewer - legacy.svg")
  expect_equal(captured$width, 980)
  expect_equal(captured$height, 760)
})

test_that("build_dialog_viewer works without title-like formals", {
  captured <- NULL
  fake_dialog_viewer <- function(width, height) {
    captured <<- list(width = width, height = height)
    function(...) NULL
  }

  out <- rstudioSvgViewer:::build_dialog_viewer(
    file_label = "basic.svg",
    dialog_viewer_fn = fake_dialog_viewer
  )

  expect_true(is.function(out))
  expect_equal(captured$width, 980)
  expect_equal(captured$height, 760)
})

test_that("run_gadget_compat uses ui when available", {
  captured <- NULL
  fake_run_gadget <- function(ui, server = NULL, viewer = NULL) {
    captured <<- list(ui = ui, server = server, viewer = viewer)
    invisible(TRUE)
  }

  ui_obj <- structure(list(), class = "fake_ui")
  server_obj <- function(input, output, session) NULL
  viewer_obj <- function(url) url

  out <- rstudioSvgViewer:::run_gadget_compat(
    ui = ui_obj,
    server = server_obj,
    viewer = viewer_obj,
    run_gadget_fn = fake_run_gadget
  )

  expect_true(isTRUE(out))
  expect_identical(captured$ui, ui_obj)
  expect_identical(captured$server, server_obj)
  expect_identical(captured$viewer, viewer_obj)
})

test_that("run_gadget_compat falls back to app argument", {
  captured <- NULL
  fake_run_gadget <- function(app, server = NULL, viewer = NULL) {
    captured <<- list(app = app, server = server, viewer = viewer)
    "ok"
  }

  ui_obj <- structure(list(), class = "fake_ui")
  server_obj <- function(input, output, session) NULL
  viewer_obj <- function(url) url

  out <- rstudioSvgViewer:::run_gadget_compat(
    ui = ui_obj,
    server = server_obj,
    viewer = viewer_obj,
    run_gadget_fn = fake_run_gadget
  )

  expect_equal(out, "ok")
  expect_identical(captured$app, ui_obj)
  expect_identical(captured$server, server_obj)
  expect_identical(captured$viewer, viewer_obj)
})

test_that("run_gadget_compat works with positional-only signature", {
  captured <- NULL
  fake_run_gadget <- function(first, second, third) {
    captured <<- list(first = first, second = second, third = third)
    123
  }

  ui_obj <- structure(list(), class = "fake_ui")
  server_obj <- function(input, output, session) NULL
  viewer_obj <- function(url) url

  out <- rstudioSvgViewer:::run_gadget_compat(
    ui = ui_obj,
    server = server_obj,
    viewer = viewer_obj,
    run_gadget_fn = fake_run_gadget
  )

  expect_equal(out, 123)
  expect_identical(captured$first, ui_obj)
  expect_identical(captured$second, server_obj)
  expect_identical(captured$third, viewer_obj)
})

test_that("svg_viewer_js contains pan and wheel interaction hooks", {
  js <- rstudioSvgViewer:::svg_viewer_js()

  expect_true(grepl("pointerdown", js, fixed = TRUE))
  expect_true(grepl("pointermove", js, fixed = TRUE))
  expect_true(grepl("wheel", js, fixed = TRUE))
  expect_true(grepl("zoomAt", js, fixed = TRUE))
})
