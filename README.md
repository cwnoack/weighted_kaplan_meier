# weighted_kaplan_meier

This repository contains the **AKME** R package: an Adjusted (weighted)
Kaplan-Meier estimator of the survival function for right-censored
concentration data, plus a weighted log-rank test for comparing two groups.
The methods follow Xie & Liu (2005) and Singh et al. (2014).

The package is in [`AKME/`](AKME/).

## Installation

From a local clone:

```r
# install.packages("pak")
pak::pkg_install("local::./AKME")
```

Or directly from GitHub:

```r
pak::pkg_install("cwnoack/weighted_kaplan_meier/AKME")
```

## Getting started

```r
library(AKME)
vignette("akme", package = "AKME")
```

The vignette walks through the full workflow: generating censored,
multi-site data, running `Surv_weighted()`, comparing groups with
`log_rank()`, and plotting with `plot_wKM()` and `G_rho_hist()`.
