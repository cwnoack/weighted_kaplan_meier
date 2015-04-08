sim_G_rho <- function(grouped_data, rho = 1, method = 'perm'){
  grouped_data$Dataset <- factor(grouped_data$Dataset)
  data_levels <- levels(factor(grouped_data$Dataset))
  
  p <- prop.table(table(grouped_data$Dataset))
  
  switch(method,
         sim = {new_labs <- sample(data_levels,
                                   size = nrow(grouped_data),
                                   replace = T, prob = p)},
         boot = {new_labs <- sample(grouped_data$Dataset, replace = T)},
         perm = {new_labs <- sample(grouped_data$Dataset, replace = F)}
         )
  
  grouped_data$Dataset <- new_labs
  
  G1 <- filter(grouped_data, Dataset == data_levels[1]) %>% select(-Dataset)
  G2 <- filter(grouped_data, Dataset == data_levels[2]) %>% select(-Dataset)
  
  G_sim <- G_rho(G1, G2, rho)
  
  return(G_sim)
}
