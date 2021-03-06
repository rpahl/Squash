% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pair.R
\docType{class}
\name{Pair-class}
\alias{Pair}
\alias{Pair-class}
\title{A pair container similar to C++ std::pair}
\description{
A pair container similar to C++ std::pair
}
\section{Fields}{

\describe{
\item{\code{first}}{(ANY), the first value of the pair}

\item{\code{second}}{(ANY), the second value of the pair}
}}
\section{Methods}{

\describe{
\item{\code{swap(pr)}}{Exchanges the contents of two pairs}
}}
\note{
Date of last modification: 09.09.2015

Change history: none
}
\examples{
p1 <- Pair("foo", 1)
p2 <- Pair(2, "bar")
p1$first # "foo"
p1$swap(p2)
p1$first # 2

}
\author{
Roman Pahl
}

