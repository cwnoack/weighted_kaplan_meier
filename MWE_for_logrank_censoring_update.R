source('run_at_startup_to_load.R')

# Group means
means <- c(1,1.5,2.5)

# Samples in each group
N <- 100

# Generate random data
set.seed(8675309)
R <- data.frame(sapply(means, function(mu) rlnorm(N, mu, 1)))
colnames(R) <- c('Control','High','Highest')

# mutate adds censoring and site ID (sampled with uneven probability)
R.g <- gather(R, Dataset,Concentration,Control:Highest) %>%
  mutate(DL = sample(c(1, 10), size = nlevels(Dataset)*N,
                     replace = T, prob = c(0.6,0.4)),
         Censored = ifelse(Concentration < DL, 1, 0),
         Concentration = ifelse(Censored == 1, DL, Concentration),
         Site = sample(LETTERS[1:10], nlevels(Dataset)*N, replace = T,
                       prob = 1:10/sum(1:10))) %>%
  select(Dataset, Concentration, Censored, Site)

CvsH <- filter(R.g, Dataset != 'High')
CvsH_test <- log_rank(CvsH, comp_group = 'Highest',
                     rho = 1, method = 'sim',
                     boots = 500, alternative = 'greater')
grp_KM <- group_km(grouped_data = CvsH)

plot_wKM(grp_KM, log_scale = T)

G_rho_hist(CvsH_test)

quantile_comp <- ddply(CvsH, .(Dataset),
                       function(df) Surv_quantile(select(df, -Dataset),
                                                  type = 'interp')
                       ) %>%
  spread(Dataset, Xh) %>% arrange(desc(Percentile))

print(quantile_comp)
