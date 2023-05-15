\name{CEEGeo distribution}
\alias{dCEEGeo}
\alias{pCEEGeo}
\alias{qCEEGeo}
\alias{rCEEGeo}
\alias{mCEEGeo}
\title{Complementary exponentiated exponential geomatric distribution
}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponentiated exponential geomatric (CEEGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline exponentiated exponential CDF, it is given by
\deqn{
G(x)=\left(1-\exp(-\alpha x)\right)^{\beta};\qquad\alpha,\beta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CEEGeo distribution.
}



\usage{
dCEEGeo(x, alpha, beta, lambda, log = FALSE)
pCEEGeo(x, alpha, beta, lambda, log.p = FALSE, lower.tail = TRUE)
qCEEGeo(p, alpha, beta, lambda, log.p = FALSE, lower.tail = TRUE)
rCEEGeo(n, alpha, beta, lambda)
mCEEGeo(x, alpha, beta, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CEEGeo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
}
\item{alpha}{
The strictly positive scale parameter of the baseline exponentiated exponential distribution (\eqn{\alpha > 0}).
}
\item{beta}{
The strictly positive shape parameter of the baseline exponentiated exponential distribution (\eqn{\beta > 0}).
}
\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}

\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CEEGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CEEGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}


\value{
dCEEGeo gives the (log) probability function.
pCEEGeo gives the (log) distribution function.
qCEEGeo gives the quantile function.
rCEEGeo generates random values.
mCEEGeo gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Louzada, F., Marchi, V., & Carpenter, J. (2013). The complementary exponentiated exponential geometric lifetime distribution. Journal of Probability and Statistics, 2013.

Nadarajah, S. (2011). The exponentiated exponential distribution: a survey. AStA Advances in Statistical Analysis, 95, 219-251.
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
x<-rCEEGeo(20,2,1,0.1)
dCEEGeo(x,2,1,0.2)
pCEEGeo (x,2,1,0.2)
qCEEGeo (0.7,2,1,0.2)
mCEEGeo(x,0.2,0.1,0.2, method="B")
}

