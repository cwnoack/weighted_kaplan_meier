% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/Surv_weighted.R
\name{Surv_weighted}
\alias{Surv_weighted}
\title{Adjusted (weighted) Kaplan-Meier Estimator of the survival function for right censored concentration data}
\usage{
Surv_weighted(censored_data)
}
\arguments{
\item{censored_data}{data.frame with three columns: (1) measured concentration, (2) flag for nondetects [BDL = 1], (3) unique site identifier.}
}
\value{
observed A data.frame containing the observed concentrations, the
  weighted "at-risk" and "events" at each concentration, and the survival
  estimator along with some intermediate data.
}
\description{
Calculates the AKME of the survival function for right censored data and
returns the estimator (S) along with weighted "at risk" and "event" values at
each observed (uncensored) concentration. Uses methods from Xie and Liu (2005).
}

