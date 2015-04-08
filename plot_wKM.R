plot_wKM <- function(grouped_km, log_scale = T, n_ticks = 5,
                     tick_step = 1, units = 'ppb'){
  par(list(oma = c(0,0,0,0),
           mar = c(4.5,4.5,1,1)))
  if(log_scale){
    stopifnot(all(grouped_km$Concentration > 0))
    C <- log10(grouped_km$Concentration)
    rng <- c(floor(min(C)), ceiling(max(C)))
    x_limits <- 10^rng
    ticks <- signif(seq(rng[1],rng[2], length.out = n_ticks), 2)
    tick_labs <- sapply(ticks, function(i) as.expression(bquote(10^.(i))))
    ticks <- 10^ticks
    log_val <- 'x'
    
  } else {
    C <- grouped_km$Concentration
    rng <- c(floor(min(C)), ceiling(max(C)))
    x_limits <- rng
    ticks <- signif(seq(rng[1],rng[2], length.out = n_ticks), 2)
    tick_labs <- ticks
    log_val <- ''
  }
  
  plot(1,1, log = log_val, type = 'n', bty = 'n',
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xlim = x_limits,
       ylim = c(0,1))
  axis(1, at = ticks, labels = tick_labs, lwd = 2, tck = -0.03)
  mtext(paste('Concentration, ',units, sep = ''), 1, line = 2.2)
  axis(2, las = 1, lwd = 2, tck = -0.03, line = -0.5)
  mtext(expression(paste("Cumulative fraction of samples, ",
                         widehat(F[x])(x), sep = '')), side = 2, line = 2.2)
  
  n_colors <- length(unique(grouped_km$Dataset))
  color_df <- data.frame(Dataset = unique(grouped_km$Dataset),
                         line_color = gdocs_pal()(n_colors))
  grouped_plot_data <- left_join(grouped_km, color_df, by = "Dataset")
  d_ply(grouped_plot_data, .(Dataset),
        function(df) {lines(df$Concentration, df$S, lwd = 1.5,
                            type = 's', col = unique(df$line_color))})
  
  legend(x = ticks[1],y= 1,legend = color_df$Dataset, lty = 1, lwd = 1.5, title = 'Dataset',
         col = color_df$line_color, bty = 'n')
  
}
