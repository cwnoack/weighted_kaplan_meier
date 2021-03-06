# Source this file at Rstudio start up to load everything
# Hopefully this will be unecessary once I have the package built.
all_files <- list.files()
all_files <- all_files[!(all_files %in% c('run_at_startup_to_load.R',
                                          'MWE_for_logrank.R',
                                          'MWE_for_logrank_censoring_update.R'))]
files_to_run <- grep(x = all_files, pattern = '.*\\.R$')
sapply(files_to_run, function(i) source(all_files[i]))
load_dependencies()
