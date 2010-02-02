\name{ISU.Stat511-package}
\alias{ISU.Stat511-package}
\alias{ISU.Stat511}
\docType{package}
\title{
Stat 511: Statistical Methods
}
\description{
A collection of R functions and datasets used in the course Stat 511 in the Department of Statistics, Iowa State University.
}
\details{
\tabular{ll}{
Package: \tab ISU.Stat511\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2010-01-31\cr
License: \tab GPL-2 | GPL-3\cr
LazyLoad: \tab yes\cr
}
%% An overview of how to use the package, including the most important functions
}
\author{
Dan Nettleton, Yihui Xie

Maintainer: Yihui Xie <xie@yihui.name>
}
\references{
%% Literature or other references for background information
}
\keyword{ package }
\seealso{
%%
}
\examples{
library(ISU.Stat511)
data(bread)
bread.lm = lm(volume ~ fat * surfactant, data = bread)
summary(bread.lm)
}
