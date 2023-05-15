\name{CExpNB distribution}
\alias{dCExpNB}
\alias{pCExpNB}
\alias{qCExpNB}
\alias{rCExpNB}
\alias{mCExpNB}

\title{Complementary exponential negative binomial distribution
}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponential negative binomial (CExpNB) distribution.
The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline exponential CDF, it is given by
\deqn{
G(x)=1-\exp(-\alpha x);\qquad\alpha>0.
}
By setting G(x) in the above Equation, yields the CDF of the CExpNB distribution.
}


\usage{
dCExpNB(x, alpha, s, lambda, log = FALSE)
pCExpNB(x, alpha, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCExpNB(p, alpha, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCExpNB(n, alpha, s, lambda)
mCExpNB(x, alpha, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) values.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CExpBio distribution.
}
\item{lambda}{
The strictly positive parameter of the negative binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution \eqn{s > 0}.
}
\item{alpha}{
The strictly positive scale parameter of the baseline exponential distribution (\eqn{\alpha > 0}).
}

\item{lower.tail}{
if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}

\item{log}{
if TRUE, probabilities p are given as log(p).
}
\item{log.p}{
if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CExpNB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CExpNB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCExpNB gives the (log) probability function.
pCExpNB gives the (log) distribution function.
qCExpNB gives the quantile function.
rCExpNB generates random values.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.
}

\author{
Muhammad Imran and M.H Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and M.H Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{pCExpGeo}
}
}

\examples{
x<-data_guineapigs
rCExpNB(20,2,2,0.5)
dCExpNB(x,2,2,0.5)
pCExpNB(x,2,3,0.5)
qCExpNB(0.7, 2,3,0.5)
mCExpNB(x,0.02,3.8,0.15, method="B")

}



