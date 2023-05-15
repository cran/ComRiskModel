\name{CB12Geo distribution}
\alias{dCB12Geo}
\alias{pCB12Geo}
\alias{qCB12Geo}
\alias{rCB12Geo}
\alias{mCB12Geo}

\title{Complementary Burr-12 geomatric distribution
}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Burr-12 geomatric (CB12Geo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline Burr-12 CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{a}\right)^{b}\right]^{-k};\qquad a,b,k>0.
}
By setting G(x) in the above Equation, yields the CDF of the CB12Geo distribution.
}

\usage{
dCB12Geo(x, a, b, k, lambda, log = FALSE)
pCB12Geo(x, a, b, k, lambda, log.p = FALSE, lower.tail = TRUE)
qCB12Geo(p, a, b, k, lambda, log.p = FALSE, lower.tail = TRUE)
rCB12Geo(n, a, b, k, lambda)
mCB12Geo(x, a, b, k, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) values.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CB12Geo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
}
\item{a}{
The strictly positive scale parameter of the baseline Burr-12 distribution (\eqn{a > 0}).
}
\item{b}{
The strictly positive shape parameter of the baseline Burr-12 distribution (\eqn{b > 0}).
}
\item{k}{
The strictly positive shape parameter of the baseline Burr-12 distribution (\eqn{k > 0}).
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CB12Geo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}


\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CB12Geo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCB12Geo gives the (log) probability function.
pCB12Geo gives the (log) distribution function.
qCB12Geo gives the quantile function.
rCB12Geo generates random values.
mCB12Geo gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Zimmer, W. J., Keats, J. B., & Wang, F. K. (1998). The Burr XII distribution in reliability analysis. Journal of quality technology, 30(4), 386-394.
}
\author{
Muhammad Imran and M.H Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and M.H Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{pCB12Geo}
}
}

\examples{
x<-data_airborne
rCB12Geo(20,2,0.4,1.2,0.2)
dCB12Geo(x,2,1,2,0.3)
pCB12Geo(x,2,1,2,0.3)
qCB12Geo(0.7,2,1,2,0.4)
mCB12Geo(x,1.72,0.2,0.2,0.1, method="B")
}



