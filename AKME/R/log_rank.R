log_rank <- function(grouped_data, comp_group = NULL, rho = 1,
                     method = 'perm', boots = 1000, alternative = 'two.sided'){
  if(is.null(comp_group)){
    
    grps <- levels(factor(grouped_data$Dataset))
    ref <- filter(grouped_data, Dataset == grps[1]) %>% select(-Dataset)
    comp <- filter(grouped_data, Dataset == grps[2]) %>% select(-Dataset)
    
  } else {
    
    ref <- filter(grouped_data, Dataset != comp_group) %>% select(-Dataset)
    comp <- filter(grouped_data, Dataset == comp_group) %>% select(-Dataset)
    
  }
  
  test_G <- G_rho(ref_data = ref, comp_dat = comp, rho = rho)
  sim_G <- replicate(boots, sim_G_rho(grouped_data, rho, method))
  
  switch(alternative,
         two.sided = {p.val <- sum(abs(sim_G) >= abs(test_G))/boots},
         greater = {p.val <- sum(sim_G >= test_G)/boots},
         less = {p.val <- sum(sim_G <= test_G)/boots}
         )
  
  output <- list(G_test = test_G, boot_G = sim_G, p.val = p.val, rho = rho)
  
  return(output)
  
}
