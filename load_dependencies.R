# This function will install any packages necessary for analysis and then load them
load_dependencies <- function(pkgs = list('plyr','dplyr','tidyr',
                                          'ggplot2','ggthemes')){
  is.installed <- function(pkg){
    if(!(require(pkg, character.only = T, quietly = T, warn.conflicts = F))){
      install.packages(pkg, verbose = F)
      library(pkg, quietly = T, verbose = F, warn.conflicts = F)
    }
  }
  
  lapply(pkgs, is.installed)
  pkg_string <- paste(unlist(pkgs), sep = '', collapse = ', ')
  cat('The following packages have been loaded: ',pkg_string,'.\n',
      sep = '')
}
