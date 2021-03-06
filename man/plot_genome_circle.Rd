% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_genome_circle.R
\name{plot_genome_circle}
\alias{plot_genome_circle}
\title{Plot circular genome scan}
\usage{
plot_genome_circle(path = ".", vars = c("Fst", "varP"), time = 5e+05,
  tracks = seq_along(vars), cols = palette()[seq_along(vars)],
  legend = T)
}
\arguments{
\item{path}{Where the data for the given simulation are to be found.}

\item{vars}{The variables to plot along the genome.}

\item{time}{What time point?}

\item{tracks}{A vector of integer indices indicating what track (i.e. line from the outside inwards) each variable should appear on. Should be of the same length as \code{vars}. To overlay variables on the same track, assign to them the same index.}

\item{cols}{Colors. Should be as long as \code{vars}. If not specified, default palette colors are taken.}

\item{legend}{Do we plot the legend?}
}
\description{
This functions plots variables across the genome in a circular plot.
}
