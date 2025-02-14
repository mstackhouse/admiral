% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert_varval_list}
\alias{assert_varval_list}
\title{Is an Argument a Variable-Value List?}
\usage{
assert_varval_list(
  arg,
  required_elements = NULL,
  accept_expr = FALSE,
  optional = FALSE
)
}
\arguments{
\item{arg}{A function argument to be checked}

\item{required_elements}{A \code{character} vector of names that must be present in \code{arg}}

\item{accept_expr}{Should expressions on the right hand side be accepted?}

\item{optional}{Is the checked parameter optional? If set to \code{FALSE} and \code{arg}
is \code{NULL} then an error is thrown.}
}
\description{
Checks if the argument is a list of \code{quosures} where the expressions are
variable-value pairs. The value can be a symbol, a string, a numeric, or
\code{NA}. More general expression are not allowed.
}
\examples{
example_fun <- function(vars) {
  assert_varval_list(vars)
}
example_fun(vars(DTHDOM = "AE", DTHSEQ = AESEQ))

try(example_fun(vars("AE", DTSEQ = AESEQ)))
}
\author{
Stefan Bundfuss, Thomas Neitmann
}
\keyword{assertion}
