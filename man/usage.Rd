\name{usage}
\alias{usage}
\title{Set the Script Expected Parameters}
\usage{
  usage(...)
}
\arguments{
  \item{usage}{Either a single list of arguments, or a
  character string.}
}
\description{
  This function checks the passed parameters against what
  is expected for a particular script. See examples for
  usage.
}
\examples{
expected_args <- list(
  input="Location of input file.",
  output="Location of output file."
)
usage(expected_args)
}

