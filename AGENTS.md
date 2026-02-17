# Repository Guidelines

## Project Structure & Module Organization
- Core addin/runtime code lives in `R/launch_svg_viewer.R`.
- RStudio addin registration is in `inst/rstudio/addins.dcf`.
- Tests are in `tests/testthat/` with entrypoint `tests/testthat.R`.
- User-facing docs are in `README.md` and function docs in `man/`.
- CI checks run from `.github/workflows/R-CMD-check.yaml`.

## Build, Test, and Development Commands
- `R CMD build .`
  - Builds the package tarball (e.g., `rstudioSvgViewer_0.0.0.9000.tar.gz`).
- `R CMD check --no-manual rstudioSvgViewer_0.0.0.9000.tar.gz`
  - Runs package checks, including `testthat` tests.
- `R -q -e "testthat::test_dir('tests/testthat')"`
  - Fast local test run during development.
- `R -q -e "install.packages('rstudioSvgViewer_0.0.0.9000.tar.gz', repos=NULL, type='source')"`
  - Installs local build for RStudio addin verification.

## Coding Style & Naming Conventions
- Use base R style with 2-space indentation and clear, small helper functions.
- Use `snake_case` for function names (`launch_svg_viewer`, `run_gadget_compat`).
- Keep compatibility wrappers explicit for Shiny/RStudio API differences.
- Keep generated CSS/JS in dedicated helpers (`svg_viewer_css()`, `svg_viewer_js()`).

## Testing Guidelines
- Framework: `testthat` (edition 3, configured in `DESCRIPTION`).
- Test files follow `test-*.R` naming in `tests/testthat/`.
- Add regression tests for every bug fix, especially compatibility issues.
- Validate both helper behavior and generated UI/JS markers when practical.

## Commit & Pull Request Guidelines
- Follow Conventional Commit style used in history: `feat:`, `fix:`, `docs:`, `test:`, `chore:`.
- Keep one logical change per commit (feature, fix, or docs update).
- PRs should include:
  - concise summary of behavior changes,
  - commands run (`R CMD build`, `R CMD check`),
  - test results,
  - screenshots/GIFs for visible addin UI changes.

## Compatibility Notes
- This project supports mixed Shiny signatures (`dialogViewer`, `runGadget`).
- When changing viewer launch logic, preserve fallback behavior for legacy environments.
