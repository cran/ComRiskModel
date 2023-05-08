\name{CWGeo distribution}
\alias{dCWGeo}
\alias{pCWGeo}
\alias{qCWGeo}
\alias{rCWGeo}
\alias{mCWGeo}
\title{Complementary Weibull geomatric distribution
}
\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Weibull geomatric (CWGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline Weibull CDF, it is given by
\deqn{
G(x)=1-\exp(-\alpha x^{\beta});\qquad\alpha,\beta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CWGeo distribution.
}

\usage{
dCWGeo(x, alpha, beta, lambda, log = FALSE)
pCWGeo(x, alpha, beta, lambda, log.p = FALSE, lower.tail = TRUE)
qCWGeo(p, alpha, beta, lambda, log.p = FALSE, lower.tail = TRUE)
rCWGeo(n, alpha, beta, lambda)
mCWGeo(x, alpha, beta, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the complementary Weibull geomatric.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
}
\item{alpha}{
The strictly positive scale parameter of the baseline Weibull distribution (\eqn{\alpha > 0}).
}
\item{beta}{
The strictly positive shape parameter of the baseline Weibull distribution (\eqn{\beta > 0}).
}

\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}

\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{if TRUE, probabilities p are given for exp(p).
}

\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CWGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CWGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCWGeo gives the (log) probability function.
pCWGeo gives the (log) distribution function.
qCWGeo gives the quantile function.
rCWGeo generates random values.
mCWGeo gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Tojeiro, C., Louzada, F., Roman, M., & Borges, P. (2014). The complementary Weibull geometric distribution. Journal of Statistical Computation and Simulation, 84(6), 1345-1362.

Hallinan Jr, A. J. (1993). A review of the Weibull distribution. Journal of Quality Technology, 25(2), 85-93.

Rinne, H. (2008). The Weibull distribution: a handbook. CRC press.
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
x<-data_actuarialm
rCWGeo(20,2,1,0.2)
dCWGeo(x,2,1,0.2)
pCWGeo(x,2,1,0.2)
qCWGeo(0.7,2,1,0.2)
mCWGeo(x,0.2,0.5,0.2, method="B")
}

