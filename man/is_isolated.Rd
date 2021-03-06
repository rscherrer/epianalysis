% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_isolated.R
\name{is_isolated}
\alias{is_isolated}
\title{Find simulations that have reached a threshold along one dimension}
\usage{
is_isolated(trajectories, dim, th)
}
\arguments{
\item{trajectories}{A data frame containing the speciation cube coordinates through time of the simulations.}

\item{dim}{Along what dimension to check? Either of "spatial", "ecological" or "mating".}

\item{th}{Threshold value for the coordinate at the final time step. Above this value, returns a TRUE, otherwise a FALSE.}
}
\value{
A boolean vector indicating which simulations did reach at least the specified threshold value along the specified dimension of the speciation cube.
}
\description{
Find simulations that have reached a threshold along one dimension
}
