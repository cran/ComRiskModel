\name{CEWBio distribution}
\alias{dCEWBio}
\alias{pCEWBio}
\alias{qCEWBio}
\alias{rCEWBio}
\alias{mCEWBio}
\title{Complementary exponentiated Weibull binomial distribution
}
\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponentiated Weibull binomial (CEWBio) distribution.
The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left[1-\lambda(1-G(x))\right]^{m}-(1-\lambda)^{m}}{1-(1-\lambda)^{m}};\qquad\lambda\in\left(0,1\right),\,m\geq1,
}
where G(x) represents the baseline exponentiated Weibull CDF, it is given by
\deqn{
G(x)=\left(1-\exp(-\alpha x^{\beta})\right)^{\theta};\qquad\alpha,\beta,\theta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CEWBio distribution.
}

\usage{
dCEWBio(x, alpha, beta, theta, m, lambda, log = FALSE)
pCEWBio(x, alpha, beta, theta, m, lambda, log.p = FALSE, lower.tail = TRUE)
qCEWBio(p, alpha, beta, theta, m, lambda, log.p = FALSE, lower.tail = TRUE)
rCEWBio(n, alpha, beta, theta, m, lambda)
mCEWBio(x, alpha, beta, theta, m, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CEWBio distribution.
}
\item{lambda}{
The strictly positive parameter of the binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{m}{
The positive parameter of the binomial distribution \eqn{m \geq 1}.
}
\item{alpha}{
The strictly positive scale parameter of the baseline exponentiated Weibull distribution (\eqn{\alpha > 0}).
}
\item{beta}{
The strictly positive shape parameter of the baseline exponentiated Weibull distribution (\eqn{\beta > 0}).
}
\item{theta}{
The strictly positive shape parameter of the baseline exponentiated Weibull distribution (\eqn{\theta > 0}).
}
\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}

\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{
if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the Bell Burr-12 distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CEWBio distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCEWBio gives the (log) probability function.
pCEWBio gives the (log) distribution function.
qCEWBio gives the quantile function.
rCEWBio generates random values.
mCEWBio gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2013). The exponentiated Weibull distribution: a survey. Statistical Papers, 54, 839-877.
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
dCEWBio(x,1,1,0.2,2,0.2)
pCEWBio(x,2,1,1.2,2,0.2)
qCEWBio(0.7,2,1,1.2,2,0.2)
mCEWBio(x,2.55,0.62,5.72,8.30,0.42, method="B")
}

