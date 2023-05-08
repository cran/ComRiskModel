\name{CWNB distribution}
\alias{dCWNB}
\alias{pCWNB}
\alias{qCWNB}
\alias{rCWNB}
\alias{mCWNB}
\title{Complementary Weibull negative binomial distribution
}
\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Weibull negative binomial (CWNB) distribution.
The CDF of the complementary G negative binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline Weibull CDF, it is given by
\deqn{
G(x)=1-\exp(-\alpha x^{\beta});\qquad\alpha,\beta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CWNB distribution.
}

\usage{
dCWNB(x, alpha, beta, s, lambda, log = FALSE)
pCWNB(x, alpha, beta, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCWNB(p, alpha, beta, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCWNB(n, alpha, beta, s, lambda)
mCWNB(x, alpha, beta, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CWBio.
}
\item{lambda}{
The strictly positive parameter of the negative binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution \eqn{s > 0}.
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

\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CWNB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CWNB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCWNB gives the (log) probability function.
pCWNB gives the (log) distribution function.
qCWNB gives the quantile function.
rCWNB generates random values.
mCWNB gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

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
rCWNB(20,2,1,2,0.2)
dCWNB(x,2,1,2,0.2)
pCWNB(x,2,1,2,0.2)
qCWNB(0.7,2,1,2,0.2)
mCWNB(x,0.2,0.1,0.2,0.1, method="B")
}

