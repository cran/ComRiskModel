\name{CEWGeo distribution}
\alias{dCEWGeo}
\alias{pCEWGeo}
\alias{qCEWGeo}
\alias{rCEWGeo}
\alias{mCEWGeo}
\title{Complementary exponentiated Weibull geomatric distribution
}
\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponentiated Weibull geomatric (CEWGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline exponentiated Weibull CDF, it is given by
\deqn{
G(x)=\left(1-\exp(-\alpha x^{\beta})\right)^{\theta};\qquad\alpha,\beta,\theta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CEWGeo distribution.
}

\usage{
dCEWGeo(x, alpha, beta, theta, lambda, log = FALSE)
pCEWGeo(x, alpha, beta, theta, lambda, log.p = FALSE, lower.tail = TRUE)
qCEWGeo(p, alpha, beta, theta, lambda, log.p = FALSE, lower.tail = TRUE)
rCEWGeo(n, alpha, beta, theta, lambda)
mCEWGeo(x, alpha, beta, theta, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CEWGeo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CEWGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CEWGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCEWGeo gives the (log) probability function.
pCEWGeo gives the (log) distribution function.
qCEWGeo gives the quantile function.
rCEWGeo generates random values.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Mahmoudi, E., & Shiran, M. (2012). Exponentiated Weibull-geometric distribution and its applications. arXiv preprint arXiv:1206.4008.

Nadarajah, S., Cordeiro, G. M., & Ortega, E. M. (2013). The exponentiated Weibull distribution: a survey. Statistical Papers, 54, 839-877.
}

\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{pCExpGeo}
}
}

\examples{
x<-rCEWGeo(200,2,1,1.2,0.2)
dCEWGeo(x,2,1,1.2,0.2)
pCEWGeo(x,2,1,1.2,0.2)
qCEWGeo(0.7,2,1,1.2,0.2)
mCEWGeo(x,2,1,1.2,0.2, method="B")
}

