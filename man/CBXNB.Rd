\name{CBXNB distribution}
\alias{dCBXNB}
\alias{pCBXNB}
\alias{qCBXNB}
\alias{rCBXNB}
\alias{mCBXNB}
\title{Complementary Burr-X negative binomial distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Burr-X negative binomial (CBXNB) distribution.
The CDF of the complementary G negative binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline Burr-X CDF, it is given by
\deqn{
G(x)=\left[1-\exp\left(-x^{2}\right)\right]^{a};\qquad a>0.
}
By setting G(x) in the above Equation, yields the CDF of the CBXNB distribution.
}

\usage{
dCBXNB(x, a, s, lambda, log = FALSE)
pCBXNB(x, a, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCBXNB(p, a, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCBXNB(n, a, s, lambda)
mCBXNB(x, a, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CBXNB distribution.
}
\item{lambda}{
The strictly positive parameter of the negative binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution (\eqn{s > 0}).
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CBXNB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CBXNB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCBXNB gives the (log) probability function.
pCBXNB gives the (log) distribution function.
qCBXNB gives the quantile function.
rCBXNB generates random values.
mCBXNB gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes.
Journal of Statistical Distributions and Applications, 3, 1-35.

Kleiber, C., & Kotz, S. (2003). Statistical size distributions in economics and actuarial sciences. John Wiley & Sons.

}

\author{
Muhammad Imran and M.H Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and M.H Tahir \email{mht@iub.edu.pk}.
}


%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{dCB12Bio}
}
}

\examples{
x<-rCBXNB(500,1.5,1.2,0.8)
dCBXNB(x,2,2,0.3)
pCBXNB(x,2,2,0.4)
qCBXNB(0.7,2,2,0.7)
mCBXNB(x,4,0.2,0.3, method="B")
}

