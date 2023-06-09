% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgwrc.R
\name{near.group.fast.cleanup}
\alias{near.group.fast.cleanup}
\title{Near Group Cleanup Function}
\usage{
near.group.fast.cleanup(data1, data2clust, near_g)
}
\arguments{
\item{data1}{ID and coords}

\item{data2clust}{ID and clusters}

\item{near_g}{the number of nearest elements to use for the grouping process}
}
\value{
Mode of the vector
}
\description{
Assigns elements a group based on the group Mode of some n number of nearest elements.
Uses matrix operations for fast performance.
}
\examples{
FolioID = c('1','2','3','4','5','6','7')
x = c(1,2,4,5,1,3,1)
y = c(3,5,6,3,1,5,2)
clust_1 = c(6,2,2,6,6,2,6)
df = data.frame(FolioID,x,y)
df2 = data.frame(FolioID,clust_1)
near.group.fast.cleanup(df,df2,near_g = 3) #pick odd number for mode function
plot(df$x,df$y,col= as.factor(df2$clust_1))
plot(df$x,df$y,col=as.factor(cleanclust$clust_1) )
}
