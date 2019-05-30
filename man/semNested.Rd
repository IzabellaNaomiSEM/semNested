\name{semNested}
\alias{semNested}
\title{semNested}
\description{
This package tests whether two latent models are (likely) nested in each other, equal to each other or not nested in each other. Note: this package is only compatable with Lavaan objects!
}


\usage{
semNested(fit1, fit2, model1, model2)
}


\details{
The objects fit1 and fit2 are Lavaan objects associated with model1 and model2. The objects model1 and model2 are character objects specifying the latent models to be compared.
}


\arguments{
  \item{fit1 }{A Lavaan object containing the cfa model fit or sem model fit of model 1}
  \item{fit2 }{A Lavaan object containing the cfa model fit or sem model fit of model 2}
  \item{model1 }{A character vector specifying model 1}
  \item{model2 }{A character vector specifying model 1}
}


\value{
\item{Conclusion }{Main output regarding nesting}
\item{DF1 }{Degrees of freedom fit1}
\item{DF2 }{Degrees of freedom fit2}
\item{Chisq. statistic model 1 based on model 2}{Chisquare statistic model 1 based on model 2. If value equals zero, likely perfect fit.}
\item{Chisq. statistic model 2 based on model 1}{Chisquare statistic model 2 based on model 1. If value equals zero, likely perfect fit.}
\item{P-value model 1 based on model 2}{If p equals 1, then this is likely a perfect it.}
\item{P-value model 2 based on model 1}{If p equals 1, then this is likely a perfect fit.}
}


\references{
General Lavaan website: http://lavaan.ugent.be/
}


\author{
I.M. Engelmoer (BSc) & N.R.J. van Bergen (BSc)
}


\note{
Note, this package was build as part of a Structural Equatision Modeling course taught by dr. Sacha Epskamp at the University of Amsterdam in 2018-2019. ~~
}


\examples{
## Utilizing the Industrialization And Political Democracy Dataset from the Lavaan package
# Example 1: Model 1 nested in model 2. Note: model 2 is a higher-order model.
library(lavaan)
model1 <- ' latent1  =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8
            latent2  =~ x1 + x2 + x3'

model2 <- ' latent1  =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8
            latent2  =~ x1 + x2 + x3
            latent3  =~ latent1 + latent2'

fit1 <- cfa(model = model1, data = PoliticalDemocracy)
fit2 <- cfa(model = model2, data = PoliticalDemocracy)

semNested(fit1, fit2, model1, model2)

# Example 2: Model 1 nested in model 3. Note: model 1 and fit 1 are already specified above.
model3 <- ' latent1  =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8
            latent2  =~ x1 + x2 + x3 + y8'

fit3 <- cfa(model = model3, data = PoliticalDemocracy)

semNested(fit1, fit3, model1, model3)

# Example 2: Model 4 is equal to model 1. Note: model 1 and fit 1 are already specified above.
model4 <- ' latent1  =~ y1 + y2 + y3 + y4 + y5 + y6 + y7
            latent2  =~ x1 + x2 + x3 + y8'

fit4 <- cfa(model = model4, data = PoliticalDemocracy)

semNested(fit1, fit4, model1, model4)
# }

