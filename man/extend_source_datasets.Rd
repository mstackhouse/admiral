% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_param_tte.R
\name{extend_source_datasets}
\alias{extend_source_datasets}
\title{Add By Groups to all Datasets if Necessary}
\usage{
extend_source_datasets(source_datasets, by_vars)
}
\arguments{
\item{source_datasets}{Source datasets

A named list of datasets is expected. Each dataset must contain either all
by variables or none of the by variables.}

\item{by_vars}{By variables}
}
\value{
The list of extended source datasets
}
\description{
The function ensures that the by variables are contained in all source
datasets.
}
\details{
\enumerate{
\item The by groups are determined as the union of the by groups occuring in
the source datasets.
\item For all source datasets which do not contain the by variables the source
dataset is replaced by the cartesian product of the source dataset and the
by groups.
}
}
\author{
Stefan Bundfuss
}
\keyword{dev_utility}
