\name{CBXGeo distribution}
\alias{dCBXGeo}
\alias{pCBXGeo}
\alias{qCBXGeo}
\alias{rCBXGeo}
\alias{mCBXGeo}
\title{Complementary Burr-X geomatric distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Burr-X geomatric (CBXGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline Burr-X CDF, it is given by
\deqn{
G(x)=\left[1-\exp\left(-x^{2}\right)\right]^{a};\qquad a>0.
}
By setting G(x) in the above Equation, yields the CDF of the CBXGeo distribution.
}

\usage{
dCBXGeo(x, a, lambda, log = FALSE)
pCBXGeo(x, a, lambda, log.p = FALSE, lower.tail = TRUE)
qCBXGeo(p, a, lambda, log.p = FALSE, lower.tail = TRUE)
rCBXGeo(n, a, lambda)
mCBXGeo(x, a, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CBXGeo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
}
\item{a}{
The strictly positive shape parameter of the baseline Burr-X distribution (\eqn{a > 0}).
}
\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}
\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CBXGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CBXGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCBXGeo gives the (log) probability function.
pCBXGeo gives the (log) distribution function.
qCBXGeo gives the quantile function.
rCBXGeo generates random values.
mCBXGeo gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes.
Journal of Statistical Distributions and Applications, 3, 1-35.

Kleiber, C., & Kotz, S. (2003). Statistical size distributions in economics and actuarial sciences. John Wiley & Sons.

}

\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{dCB12Geo}
}
}

\examples{
x<-data_guineapigs
dCBXGeo(x,2,0.3)
pCBXGeo(x,2,0.4)
qCBXGeo(0.7,2,0.7)
mCBXGeo(x,0.2,0.3, method="B")
}

