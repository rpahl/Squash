% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pair.R
\docType{class}
\name{TypedPair-class}
\alias{TypedPair}
\alias{TypedPair-class}
\title{A typed pair container similar to C++ std::pair extending the Pair class}
\description{
A typed pair container similar to C++ std::pair extending the Pair class
}
\section{Fields}{

\describe{
\item{\code{T1}}{(character) string specifying the type of the first element}

\item{\code{T2}}{(character) string specifying the type of the second element}
}}
\note{
Date of last modification: 09.09.2015

Change history: none
}
\examples{
x0 <- TypedPair(T1 = "character", first = "x", T2 = "numeric", second = 0)
Analog: Short (implicit) construction
x0 <- TypedPair("character", "x", "numeric", 0)
x0$first # "x"
x0$second # "0"
\dontrun{
x0$setFirst(1) # Throws "bad type - expected character but got numeric"
}
x0$setFirst("z") # ok
x0$first # "z"

y1 <- TypedPair("character", "y", "numeric", 1)
x0$swap(y1)
x0$first # "y"
x0$second # "1"

}
\author{
Roman Pahl
}

