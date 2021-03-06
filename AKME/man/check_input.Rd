% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/check_input.R
\name{check_input}
\alias{check_input}
\title{Check inputs to Kaplan-Meier estimator functions}
\usage{
check_input(censored_data)
}
\arguments{
\item{censored_data}{a data.frame of three columns.}
}
\description{
For the Kaplan-Meier code, the data need to be formatted in a data.frame The
columns need to hold (1) the measured concentration [numeric], (2) a detection
flag [numeric/factor/logical/integer], and (3) a site identifier
[factor/character/integer/numeric].
}
\examples{
data_good <- data.frame(Concentration = rlnorm(100,1,2),
 Censored = rbinom(100,1,0.2),
  Site = sample(letters[1:3],100,T))
check_input(data_good)
}

