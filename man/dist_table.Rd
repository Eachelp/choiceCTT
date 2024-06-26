% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_table.R
\name{dist_table}
\alias{dist_table}
\title{Calculate distribution of categories per item and quantile grouping}
\usage{
dist_table(data, score = NULL, cutting = 4, digit = 3, missing = "omit")
}
\arguments{
\item{data}{A data frame with each column representing an item with responses.}

\item{cutting}{The number of quantile groups to create.}

\item{categories}{A vector of the categories to consider for frequency calculation.}
}
\value{
A list containing distribution tables for each item and a summary.
}
\description{
This function calculates the frequency distribution of response categories
for each item in the data and also groups the total scores into quantiles.
It returns a list containing the distribution tables for each item and a summary table.
}
