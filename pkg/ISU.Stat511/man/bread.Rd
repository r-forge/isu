\name{bread}
\alias{bread}
\docType{data}
\title{
Bread Volumn Data
}
\description{
Volume of bread loaves corresponding to two different fats and three different surfactants.
}
\usage{data(bread)}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{volume}}{a numeric vector}
    \item{\code{fat}}{a factor with levels \code{1} \code{2}}
    \item{\code{surfactant}}{a factor with levels \code{A} \code{B} \code{C}}
  }
}
\details{
A food scientist performed the following experiment to study the effects of combining two different fats and three different surfactants on the specific volume of bread loaves. Four batches of dough were made for each of the six combinations of fat and surfactant. Ten loaves of bread were made from each batch of dough and the average volume of the ten loaves was recorded for each batch. Unfortunately, some of the yeast used to make some batches of dough was ineffective and data from the loaves made from those batches had to be removed from the analysis. Fortunately, all six combinations of the levels of fat and surfactant were observed at least once.
}
\source{
Homework 3: \url{http://www.public.iastate.edu/~dnett/S511/hw3.pdf}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(bread)
bread.lm = lm(volume ~ fat * surfactant, data = bread)
## or lm(volume ~ fat + surfactant + fat:surfactant, data = bread)
summary(bread.lm)
## two-way interaction plot
with(bread, interaction.plot(surfactant, fat, volume))
}
\keyword{datasets}
