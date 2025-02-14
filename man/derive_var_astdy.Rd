% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_var_astdy.R
\name{derive_var_astdy}
\alias{derive_var_astdy}
\title{Derive Analysis Start Relative Day}
\usage{
derive_var_astdy(dataset, reference_date = TRTSDT, date = ASTDT)
}
\arguments{
\item{dataset}{Input dataset

The columns specified by the \code{reference_date} and the \code{date} parameter are
expected.}

\item{reference_date}{The start date column, e.g., date of first treatment

A date or date-time object column is expected.

The default is \code{TRTSDT}.}

\item{date}{The end date column for which the study day should be derived

A date or date-time object column is expected.

The default is \code{ASTDT}}
}
\value{
The input dataset with \code{ASTDY} column added
}
\description{
Adds the analysis start relative day (\code{ASTDY}) to the dataset, i.e., study
day of analysis start date.
}
\details{
The study day is derived as number of days from the start date
to the end date. If it is nonnegative, one is added. I.e., the study day of the
start date is 1.
}
\examples{
data <- tibble::tribble(
  ~TRTSDT, ~ASTDT,
  lubridate::ymd("2020-01-01"), lubridate::ymd("2020-02-24")
)

derive_var_astdy(data)
}
\author{
Stefan Bundfuss
}
\keyword{bds}
\keyword{derivation}
\keyword{occds}
\keyword{timing}
