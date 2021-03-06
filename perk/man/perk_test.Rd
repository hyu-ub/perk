% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perk.R
\name{perk_test}
\alias{perk_test}
\title{Robust permutation tests of correlation coefficients
Studentized permutation test of Lin's concordance correlation coefficient.}
\usage{
perk_test(
  x,
  y,
  B = 1000,
  r0 = 0,
  method = c("pearson", "spearman", "ccc"),
  alternative = c("two.sided", "less", "greater")
)
}
\arguments{
\item{x}{a \code{numeric} vector}

\item{y}{a \code{numeric} vector}

\item{B}{an \code{interger} number of permutations}

\item{r0}{a \code{numeric} denoting the CCC under the null hypothesis. It should be in the range between -1 and 1. 
This parameter will be ignored for the tests of Pearson or Spearman's correlation coefficient.}

\item{method}{the correlation coefficient to be tested, options include Pearson's correlation coefficient (\code{pearson}), 
Spearman's correlation coefficient (\code{spearman}), and Lin's concordance correlation coefficient (\code{ccc}).}

\item{alternative}{the alternative hypothesis, can be \code{two.sided}, \code{less}, or \code{greater}}
}
\value{
\describe{
\item{\code{estimate}}{the estimated correlation coefficient.}
\item{\code{p.value}}{the p-value from the studentized permutation test.}
\item{\code{method}}{the method for measuring correlation coefficient.}
\item{\code{alternative}}{the alternative hypothesis.}
}
}
\description{
Robust permutation tests of correlation coefficients
Studentized permutation test of Lin's concordance correlation coefficient.
}
\details{
Robust permutation test of Pearson correlation coefficient, Spearman correlation coefficient, and Lin's concordance correlation coefficient.
The test robustly controls type I error under general scenarios
when sample size is small and two variables are dependent but uncorrelated.
Currently, for Pearson and Spearman correlations, the test only supports a zero null hypothesis, but the alternative hypothesis can be 
either one-sided or two-sided. For CCC, the test supports a more general null hypothesis. Currently, the package only supports a one-sided 
alternative hypothesis (greater).
}
\examples{
set.seed(123)
x <- rnorm(20)
y <- rnorm(20)
perk_test(x, y, B = 500, method = "pearson", alternative = "greater")
}
\references{
Lawrence, I., & Lin, K. (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics, 255-268. \cr
\cr
DiCiccio, C. J., & Romano, J. P. (2017). Robust permutation tests for correlation and regression coefficients. Journal of the American Statistical Association, 112(519), 1211-1220.
}
\author{
Han Yu, Alan Hutson
}
