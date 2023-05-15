\name{CEWNB distribution}
\alias{dCEWNB}
\alias{pCEWNB}
\alias{qCEWNB}
\alias{rCEWNB}
\alias{mCEWNB}
\title{Complementary exponentiated Weibull negative binomial distribution
}
\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponentiated Weibull negative binomial (CEWNB) distribution.
The CDF of the complementary G negative binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline exponentiated Weibull CDF, it is given by
\deqn{
G(x)=\left(1-\exp(-\alpha x^{\beta})\right)^{\theta};\qquad\alpha,\beta,\theta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CEWNB distribution.
}

\usage{
dCEWNB(x, alpha, beta, theta, s, lambda, log = FALSE)
pCEWNB(x, alpha, beta, theta, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCEWNB(p, alpha, beta, theta, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCEWNB(n, alpha, beta, theta, s, lambda)
mCEWNB(x, alpha, beta, theta, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CEWNB distribution.
}
\item{lambda}{
The strictly positive parameter of the binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution \eqn{s > 0}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CEWNB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CEWNB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}
\value{
dCEWNB gives the (log) probability function.
pCEWNB gives the (log) distribution function.
qCEWNB gives the quantile function.
rCEWNB generates random values.
mCEWNB gives the estimated parameters along with SE and goodness-of-fit measures.
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
\code{\link{pCExpBio}
}
}

\examples{
x<-rCEWNB(20,2,1,1.2,2,0.2)
dCEWNB(x,2,1,1.2,2,0.2)
pCEWNB(x,2,1,1.2,2,0.2)
qCEWNB(0.7,2,1,1.2,2,0.2)
mCEWNB(x,2,1,1.2,2,0.2, method="B")
}

