\name{CB12Bio distribution}
\alias{dCB12Bio}
\alias{pCB12Bio}
\alias{qCB12Bio}
\alias{rCB12Bio}
\alias{mCB12Bio}
\title{Complementary Burr-12 binomial distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Burr-12 binomial (CB12Bio) distribution.
The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left[1-\lambda(1-G(x))\right]^{m}-(1-\lambda)^{m}}{1-(1-\lambda)^{m}};\qquad\lambda\in\left(0,1\right),\,m\geq1,
}
where G(x) represents the CDF of the baseline Burr-12 distribution, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{a}\right)^{b}\right]^{-k};\qquad a,b,k>0.
}
By setting G(x) in the above Equation, yields the CDF of the CB12Bio distribution.
}

\usage{
dCB12Bio(x, a, b, k, m, lambda, log = FALSE)
pCB12Bio(x, a, b, k, m, lambda, log.p = FALSE, lower.tail = TRUE)
qCB12Bio(p, a, b, k, m, lambda, log.p = FALSE, lower.tail = TRUE)
rCB12Bio(n, a, b, k, m, lambda)
mCB12Bio(x, a, b, k, m, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) values.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CB12Bio distribution.
}
\item{lambda}{
The strictly positive parameter of the binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{m}{
The positive parameter of the binomial distribution \eqn{m \geq 1}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CB12Bio distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}


\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CB12Bio distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCB12Bio gives the (log) probability function.
pCB12Bio gives the (log) distribution function.
qCB12Bio gives the quantile function.
rCB12Bio generates random values.
mCB12Bio gives the estimated parameters along with SE and goodness-of-fit measures.
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
x<-data_guineapigs
rCB12Bio(20,2,0.4,1.2,2,0.7)
dCB12Bio(x,2,1,2,2,0.3)
pCB12Bio(x,2,1,2,2,0.3)
qCB12Bio(0.7,2,1,2,2,0.7)
mCB12Bio(x,0.7,0.1,0.2,0.7,0.7, method="B")
}



