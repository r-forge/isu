\name{Ftest.lm}
\alias{Ftest.lm}
\title{
Perform an F test on C * beta = d for a linear model
}
\description{
Perform an F test on C * beta = d for an \code{lm} object, and calculate confidence intervals corresponding to each row of the C matrix.
}
\usage{
Ftest.lm(object, Cmat, d = 0, conf.level = 0.95)
}
\arguments{
  \item{object}{
an \code{lm} object; typically from \code{\link[stats]{lm}}
}
  \item{Cmat}{
the C matrix (to form linear combinations of beta's)
}
  \item{d}{
the \code{d} vector in C * beta = d
}
  \item{conf.level}{
confidence level
}
}
\details{
%% ...
}
\value{
A list with class `\code{htest}' containing the following components:
  \item{statistic}{the F statistic}
  \item{parameter}{the degrees of freedom for the numerator and denominator}
  \item{p.value}{P value of the test}
  \item{conf.int}{confidence interval(s) for row(s) of C}
  \item{method}{the name of the test}
  \item{data.name}{the object name used in the test}
}
\references{
Lecture notes ``Inference under the Normal Theory Gauss-Markov Model'' p.25--34: \url{http://www.public.iastate.edu/~dnett/S511/08NormalTheoryInference.pdf}
}
\author{
Dan Nettleton (with slight modifications by Yihui Xie)
}
\note{
The confidence intervals is a \code{matrix} object. When there are multiple rows in the \code{C} matrix, a confidence interval is computed for each \code{c*beta} where \code{c} is a row of \code{C}. There are no multiple comparison ajustments by default. To obtain these confidence intervals, we just need to extract the \code{conf.int} component from the list returned (e.g. \code{x$conf.int}); see the examples below.
}

\seealso{
\code{\link[stats]{lm}}, \code{\link[gmodels]{glh.test}} (in the \pkg{gmodels} package)
}
\examples{
data(bread)

bread.lm = lm(volume ~ fat * surfactant, data = bread)
## or lm(volume ~ fat + surfactant + fat:surfactant, data = bread)
summary(bread.lm)

## design matrix
model.matrix(bread.lm)

## estimates of coefficients:
## intercept fat2 surfactantB surfactantC fat2:surfactantB fat2:surfactantC
coef(bread.lm)

## variance matrix of \hat{beta}
vcov(bread.lm)

## F test on a linear combination of beta's:
## - fat2 - fat2:surfactantC = 0
(bread.test = Ftest.lm(bread.lm, c(0, -1, 0, 0, 0, -1)))
## P value
bread.test$p.value
## confidence interval
bread.test$conf.int
}
\keyword{ htest }
\keyword{ regression }
