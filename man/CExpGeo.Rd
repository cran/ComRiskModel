\name{CExpGeo distribution}
\alias{dCExpGeo}
\alias{pCExpGeo}
\alias{qCExpGeo}
\alias{rCExpGeo}
\alias{mCExpGeo}

\title{Complementary exponential geomatric distribution
}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponential geomatric (CExpGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline exponential CDF, it is given by
\deqn{
G(x)=1-\exp(-\alpha x);\qquad\alpha>0.
}
By setting G(x) in the above Equation, yields the CDF of the CExpGeo distribution.
}


\usage{
dCExpGeo(x, alpha, lambda, log = FALSE)
pCExpGeo(x, alpha, lambda, log.p = FALSE, lower.tail = TRUE)
qCExpGeo(p, alpha, lambda, log.p = FALSE, lower.tail = TRUE)
rCExpGeo(n, alpha, lambda)
mCExpGeo(x, alpha, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) values.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CExpGeo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CExpGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CExpGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}
\value{
dCExpGeo gives the (log) probability function.
pCExpGeo gives the (log) distribution function.
qCExpGeo gives the quantile function.
rCExpGeo generates random values.
mCExpGeo gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Louzada, F., Roman, M., & Cancho, V. G. (2011). The complementary exponential geometric distribution: Model, properties, and a comparison with its counterpart. Computational Statistics & Data Analysis, 55(8), 2516-2524.
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
x<-data_guineapigs
rCExpGeo(20,2,0.5)
dCExpGeo(x,2,0.5)
pCExpGeo(x,2,0.5)
qCExpGeo(0.7, 2,0.5)
mCExpGeo(x,2,0.5, method="B")
}



