G_rho_hist <- function(log_rank_output){
  
  par(list(oma = c(0,0,0,0),
           mar = c(4.5,4.5,2,1)))
  
  G_test <- log_rank_output$G_test
  boot_G <- log_rank_output$boot_G
  p.val <- log_rank_output$p.val
  i <- log_rank_output$rho
  
  get_lims <- function(test_val = G_test,
                       sim_vals = boot_G){
    combined <- c(test_val, sim_vals)
    limits <- c(floor(min(combined)), ceiling(max(combined)))
    return(limits)
  }
  
  xlims <- get_lims()
  
  H <- hist(boot_G, breaks = 21, plot = F)
  plot(H, main = as.expression(bquote(rho~"=" ~.(i))), xlab = expression(paste(G[rho]^"*")), xlim = xlims)
  abline(v = G_test, col = 'red', lty = 2, lwd = 2)
  
  if(G_test > 0){
    text(x = G_test, y = 0.7*max(H$counts),
         labels = expression(G[rho]^"test"), pos = 2, col = 'red')
    text(x = G_test, y = 0.55*max(H$counts),
         labels = sprintf('P = %1.3f',p.val), pos = 2, col = 'red')
  } else {
    text(x = G_test, y = 0.7*max(H$counts),
         labels = expression(G[rho]^"test"), pos = 4, col = 'red')
    text(x = G_test, y = 0.55*max(H$counts),
         labels = sprintf('P = %1.3f',p.val), pos = 4, col = 'red')     
  }
  
}
