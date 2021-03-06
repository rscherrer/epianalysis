% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_multiple_likelihood_scapes.R
\name{plot_multiple_likelihood_scapes}
\alias{plot_multiple_likelihood_scapes}
\title{Multiple speciation scapes}
\usage{
plot_multiple_likelihood_scapes(data, paramspace, dim, threshold, n,
  factor, ...)
}
\arguments{
\item{data}{A data frame containing trajectories in speciation cube for all simulations (in rows).}

\item{paramspace}{The names of the two parameters to plot against.}

\item{dim}{The variable to use as a criterion for complete speciation.}

\item{threshold}{The threshold above which the value of the criterion variable means complete speciation}

\item{n}{How many of the last time points to average?}

\item{factor}{The factor used to split the plots}

\item{...}{Extra arguments to be passed to \code{plot_likelihood_scape}.}
}
\description{
This is a function to produce multiple heatmaps from a single dataset, by levels of a factor.
}
