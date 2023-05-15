\name{CExpBio distribution}
\alias{dCExpBio}
\alias{pCExpBio}
\alias{qCExpBio}
\alias{rCExpBio}
\alias{mCExpBio}

\title{Complementary exponential binomial distribution
}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponential binomial (CExpBio) distribution.
The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left[1-\lambda(1-G(x))\right]^{m}-(1-\lambda)^{m}}{1-(1-\lambda)^{m}};\qquad\lambda\in\left(0,1\right),\,m\geq1,
}
where G(x) represents the baseline exponential CDF, it is given by
\deqn{
G(x)=1-\exp(-\alpha x);\qquad\alpha>0.
}
By setting G(x) in the above Equation, yields the CDF of the CExpBio distribution.
}


\usage{
dCExpBio(x, alpha, m, lambda, log = FALSE)
pCExpBio(x, alpha, m, lambda, log.p = FALSE, lower.tail = TRUE)
qCExpBio(p, alpha, m, lambda, log.p = FALSE, lower.tail = TRUE)
rCExpBio(n, alpha, m, lambda)
mCExpBio(x, alpha, m, lambda, method="B")
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
The strictly positive parameter of the binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{m}{
The positive parameter of the binomial distribution \eqn{m \geq 1}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CExpBio distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CExpBio distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCExpBio gives the (log) probability function.
pCExpBio gives the (log) distribution function.
qCExpBio gives the quantile function.
rCExpBio generates random values.
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
rCExpBio(20,2,2,0.5)
dCExpBio(x,2,2,0.5)
pCExpBio(x,2,3,0.5)
qCExpBio(0.7, 2,3,0.5)
mCExpBio(x,1.402,2.52,0.04, method="B")
}



