source('run_at_startup_to_load.R')

# Group means
means <- c(1,1.5,5)

# Samples in each group
N <- 50
f_cen <- 0.3

# Generate random data
set.seed(8675309)
R <- data.frame(sapply(means, function(mu) rlnorm(N, mu, 1)))
colnames(R) <- c('Control','High','Highest')

# mutate adds censoring and site ID (sampled with uneven probability)
R.g <- gather(R, Dataset,Concentration,Control:Highest) %>%
  mutate(Censored = rbinom(nlevels(Dataset)*N,1,f_cen),
         Site = sample(LETTERS[1:10], nlevels(Dataset)*N, replace = T,
                       prob = 1:10/sum(1:10)))


CvsH <- filter(R.g, Dataset != 'Highest')
CvsH_test <- log_rank(CvsH, comp_group = 'High',
                     rho = 0, method = 'sim',
                     boots = 100, alternative = 'greater')
plot_wKM(group_km(grouped_data = CvsH), log_scale = T)
