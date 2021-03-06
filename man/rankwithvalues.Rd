% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rankwithvalues.R
\name{rankwithvalues}
\alias{rankwithvalues}
\title{Rank With Values}
\usage{
rankwithvalues(x, a)
}
\arguments{
\item{x}{Observed value in the raw data table}

\item{a}{First two columns of the ranking subtable}
}
\value{
ranking value to be assigned to this observed data value
}
\description{
This function draws the appropriate ranking value from the appropriate ranking subtable
based on the value found in the raw data table (for variables that have discrete observations,
i.e. variable, type, and severity)
}
\keyword{rank}
\keyword{value}
