#' Plot weighted Kaplan-Meier survival curves
#'
#' Returns a ggplot object showing one stepped survival curve per level
#' of `Dataset`. The function is a thin wrapper around `ggplot2`.
#'
#' Note: this function returns the plot rather than drawing it as a side
#' effect. Top-level callers (the R console, vignettes, knitr chunks)
#' will auto-print the result; scripts that previously relied on the
#' side effect must now wrap the call in [print()].
#'
#' @param grouped_km Output of [group_km()] (or any tibble with columns
#'   `Concentration`, `S`, and `Dataset`).
#' @param log_scale Logical. Use a base-10 log x-axis (default) or
#'   linear.
#' @param n_ticks Approximate number of x-axis breaks.
#' @param tick_step Retained for backward compatibility; not used.
#' @param units Concentration units to display in the x-axis label.
#' @return A `ggplot` object.
#' @export
plot_wKM <- function(grouped_km,
                     log_scale = TRUE,
                     n_ticks = 5,
                     tick_step = 1,
                     units = "ppb") {
  if (log_scale && any(grouped_km$Concentration <= 0)) {
    cli::cli_abort(
      "All concentrations must be positive when {.code log_scale = TRUE}."
    )
  }

  n_colors <- dplyr::n_distinct(grouped_km$Dataset)
  pal <- ggthemes::gdocs_pal()(n_colors)

  p <- ggplot2::ggplot(
    grouped_km,
    ggplot2::aes(
      x = .data$Concentration,
      y = .data$S,
      colour = factor(.data$Dataset)
    )
  ) +
    ggplot2::geom_step(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = pal, name = "Dataset") +
    ggplot2::ylim(0, 1) +
    ggplot2::ylab(expression(widehat(F[x])(x))) +
    ggplot2::xlab(paste0("Concentration, ", units)) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "right")

  if (log_scale) {
    p <- p + ggplot2::scale_x_log10(
      breaks = scales::breaks_log(n = n_ticks),
      labels = scales::label_log()
    )
  } else {
    p <- p + ggplot2::scale_x_continuous(n.breaks = n_ticks)
  }

  p
}
