% Generated by roxygen2 (4.0.2): do not edit by hand
\name{Maybe}
\alias{Maybe}
\title{Zero or one of the specified pattern}
\usage{
Maybe(Regularity_object, ...)
}
\arguments{
\item{Regularity_object}{created from Regularity}

\item{...}{can be either a regex pattern or count and pattern}
}
\value{
a Regularity object
}
\description{
Zero or one of the specified pattern
}
\examples{
Regularity() \%>\% StartWith(3, 'digits') \%>\% Maybe('digit')
}

