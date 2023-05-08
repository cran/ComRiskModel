\name{CFGeo distribution}
\alias{dCFGeo}
\alias{pCFGeo}
\alias{qCFGeo}
\alias{rCFGeo}
\alias{mCFGeo}
\title{Complementary Fisk geomatric distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Fisk geomatric (CFGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline Fisk CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{a}\right)^{b}\right]^{-1};\qquad a,b>0.
}
By setting G(x) in the above Equation, yields the CDF of the CFGeo distribution.
}


\usage{
dCFGeo(x, a, b, lambda, log = FALSE)
pCFGeo(x, a, b, lambda, log.p = FALSE, lower.tail = TRUE)
qCFGeo(p, a, b, lambda, log.p = FALSE, lower.tail = TRUE)
rCFGeo(n, a, b, lambda)
mCFGeo(x, a, b, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CFGeo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
}
\item{a}{
The strictly positive scale parameter of the baseline Fisk distribution (\eqn{a > 0}).
}
\item{b}{
The strictly positive shape parameter of the baseline Fisk distribution (\eqn{b > 0}).
}

\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}

\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CFGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CFGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}


\value{
dCFGeo gives the (log) probability function.
pCFGeo gives the (log) distribution function.
qCFGeo gives the quantile function.
rCFGeo generates random values.
mCFGeo gives the estimated parameters along with SE and goodness-of-fit measures.
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
\code{\link{pCLGeo}
}
}

\examples{
x<-rCFGeo(20,2,1,0.7)
x
dCFGeo(x,2,1,0.1)
pCFGeo(x,2,1,0.1)
qCFGeo(0.7,2,1,0.1)
mCFGeo(x,0.2,0.1,0.1, method="B")
}

