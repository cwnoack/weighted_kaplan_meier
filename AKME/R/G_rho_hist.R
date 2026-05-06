#' Plot the null distribution from a log-rank test
#'
#' Takes the output from [log_rank()] and returns a ggplot histogram of
#' the simulated `G_rho` values, with the observed test statistic
#' overlaid as a red dashed reference line and the P-value annotated.
#'
#' Returns the plot object rather than drawing it as a side effect; see
#' the note in [plot_wKM()].
#'
#' @param log_rank_output A list returned by [log_rank()].
#' @return A `ggplot` object.
#' @export
G_rho_hist <- function(log_rank_output) {
  G_test <- log_rank_output$G_test
  p_val  <- log_rank_output$p.val
  rho    <- log_rank_output$rho

  df <- tibble::tibble(boot_G = log_rank_output$boot_G)
  hjust <- if (G_test > 0) 1.05 else -0.05
  label <- sprintf("italic(P) == %.3f", p_val)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$boot_G)) +
    ggplot2::geom_histogram(bins = 21, fill = "grey80", colour = "grey40") +
    ggplot2::geom_vline(
      xintercept = G_test,
      colour = "red", linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = G_test, y = Inf,
      label = label, parse = TRUE,
      hjust = hjust, vjust = 1.5, colour = "red"
    ) +
    ggplot2::labs(
      title = bquote(rho == .(rho)),
      x = expression(G[rho]^"*"),
      y = "Count"
    ) +
    ggplot2::theme_classic(base_size = 12)
}
