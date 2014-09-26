\name{Regr-package}
\alias{Regr-package}
\alias{Regr}
\docType{package}
\title{
Regr
}
\description{
Regr is a friendly regex builder inspired by Ruby's Regularity
    library that is designed to work with R's magrittr. The library is based
    around using simple verbs (StartsWith, Then, OneOrMore) to construct regex
    strings. regr also recognises the identifiers digit, lowercase, uppercase,
    letter, alphanumeric, whitespace, space, tab as singular or plural, e.g.
    digit and digits can both be used
}
\details{
\tabular{ll}{
Package: \tab regr\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-23\cr
License: \tab GPL-3\cr
}
\code{regr() \%>\% 
StartWith(3, '$') \%>\% 
Append('-') \%>\% 
Then(2, 'uppercase') \%>\%
Maybe('digit') \%>\%
Between(c(1, 2), 'a') \%>\%
OneOf(c('c', 'd', 'e')) \%>\%
ZeroOrMore('k') \%>\%
AtLeast(5, 'j') \%>\%
EndWith(4, 'alphanumeric')}
}
\author{
Martin Eastwood
}

