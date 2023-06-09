% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgwrc.R
\name{Find_Mode}
\alias{Find_Mode}
\title{Find Mode Function}
\usage{
Find_Mode(x, near_g, clusters)
}
\arguments{
\item{x}{input vector of element distinces to some origin element}

\item{near_g}{choose the maximum number of elements from x for the Mode function}

\item{clusters}{x's associated cluster column}
}
\value{
Mode of the vector
}
\description{
Finds the most occurred group for some given 'clusters' given some number of elements
'near_g' within some vector 'x'.
}
\examples{
df <- data.frame(a = c(0,1,1,2,3,4,5), b = c("0","0","0","1","1","1","1"))
Find_Mode(df$a, near_g = 3,clusters = df$b)
}
