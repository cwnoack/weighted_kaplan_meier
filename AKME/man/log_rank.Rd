% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/log_rank.R
\name{log_rank}
\alias{log_rank}
\title{Log-rank test}
\usage{
log_rank(grouped_data, comp_group = NULL, rho = 1, method = "perm",
  boots = 1000, alternative = "two.sided")
}
\arguments{
\item{grouped_data}{data.frame with four columns: (1) measured concentration,
(2) flag for nondetects [BDL = 1],
 (3) unique site identifier,
  (4) dataset identifier.}

\item{comp_group}{identifier of the "comparison" group for the test. Defaults to
the second group if no value is supplied.}

\item{rho}{Positive, real number input. This is the exponential argument of
the weighting function as in Singh et al. (2014)}

\item{method}{method for randomization of groupings for permutation test.
Either `perm` for resampling without replacement, `boot` for resampling with
replacement, or `sim` for simulation from a binomial distribution.}

\item{boots}{Number of randomizations to be performed.}

\item{alternative}{Specifies form of test. Either 'two.sided', 'greater',
or 'less' referring to the comparison group of `comp_group`}
}
\value{
output A list with elements containing the true test statistic (G_test),
the randomized test statistics (boot_G), the p.value of the resulting test
(p.value), and the value of rho used for the test (rho).
}
\description{
Weighted or unweighted log-rank test of differences between two groups using
the methodologies of Xie and Liu (2005) and Singh et al. (2014). This is a
non-parametric test that compares a true test statistics to a number of
randomized statistics (randomized by simulation or one of two resampling
strategies). A P-value is calculated based on the extremity of the true statistic
compared to the randomized statistics.
}

