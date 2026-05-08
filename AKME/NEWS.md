# AKME 0.2.0

Complete tidyverse modernization. The public API (function names and
parameter names) is preserved.

## Breaking changes

- `plot_wKM()` and `G_rho_hist()` now **return** a `ggplot` object
  rather than drawing as a side effect. Top-level callers (the R
  console, vignettes, knitr chunks) still auto-print, but scripts
  that previously relied on the side effect alone need to wrap the
  call in `print()`.
- The package now requires R `>= 4.1.0` (was `>= 3.1.1`).

## Internals

- Dropped `plyr` from `Imports`. `plyr::ddply()` and `plyr::d_ply()`
  call sites in `group_km()`, `grp_quantiles()`, and `plot_wKM()` now
  use `dplyr::group_modify()` / `dplyr::group_walk()`.
- Dropped `magrittr` from `Imports`. All pipes in package source are
  the base `|>`.
- Replaced deprecated `tidyr::gather()` and `tidyr::spread()` with
  `tidyr::pivot_longer()` and `tidyr::pivot_wider()`.
- `Surv_weighted()` returns a tibble; column rename uses
  `dplyr::rename(... = 1)` instead of `names<-`.
- `Surv_wconf()` uses `dplyr::if_else()`, `tidyr::replace_na()`, and
  `pmin()`/`pmax()` instead of nested `ifelse()` for NA / Inf / clamp
  handling.
- `Surv_quantile()` uses `purrr::map_int()` instead of `sapply()`,
  validates `type` with `rlang::arg_match()`, and emits warnings via
  `cli::cli_warn()`.
- `G_rho()` uses `dplyr::full_join(..., suffix = ...)` instead of
  base `merge()`, and `tidyr::replace_na()` instead of nested
  `ifelse()`.
- `log_rank()` uses `purrr::map_dbl()` instead of `replicate()` and
  validates `method` and `alternative` with `rlang::arg_match()`.
  A latent argument-name typo (`comp_dat = comp` against the formal
  `comp_data`) at the `G_rho()` call site is now fixed.
- `check_input()` uses `cli::cli_abort()` for structured errors and
  `is.numeric()` / `inherits()` instead of `class(x)[1]`.
- All non-package internal column references use the `.data`
  pronoun (via `@importFrom rlang .data`) to silence
  `R CMD check` NOTEs.

## Plotting rewrites

- `plot_wKM()` is now a ggplot2 function: `geom_step()`,
  `scale_colour_manual()` keyed off `ggthemes::gdocs_pal()`, and
  `scale_x_log10()` with `scales::label_log()`. Validates positive
  concentrations under `log_scale = TRUE` via `cli::cli_abort()`.
- `G_rho_hist()` is now a ggplot2 function: `geom_histogram()`
  + `geom_vline()` + `annotate()` for the P-value label.

## Documentation and tooling

- Re-authored DESCRIPTION with `Authors@R = person(...)`,
  `Encoding: UTF-8`, `Roxygen: list(markdown = TRUE)`,
  `RoxygenNote: 7.3.2`, `Config/testthat/edition: 3`, and
  proper `URL` / `BugReports`.
- New `tests/testthat/` suite covering the public API.
- Replaced the legacy MWE script and CSV-dependent vignette with a
  self-contained `vignettes/akme.Rmd`.
- Added a `pkgdown` site config.

# AKME 0.1

- Initial implementation.
