\name{CFNB distribution}
\alias{dCFNB}
\alias{pCFNB}
\alias{qCFNB}
\alias{rCFNB}
\alias{mCFNB}
\title{Complementary Fisk negative binomial distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Fisk negative binomial (CFNB) distribution.
The CDF of the complementary G negative binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline Fisk CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{a}\right)^{b}\right]^{-1};\qquad a,b>0.
}
By setting G(x) in the above Equation, yields the CDF of the CFNB distribution.
}



\usage{
dCFNB(x, a, b, s, lambda, log = FALSE)
pCFNB(x, a, b, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCFNB(p, a, b, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCFNB(n, a, b, s, lambda)
mCFNB(x, a, b, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CFNB distribution.
}
\item{lambda}{
The strictly positive parameter of the negative binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution \eqn{s > 0}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CFNB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CFNB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}



\value{
dCFNB gives the (log) probability function.
pCFNB gives the (log) distribution function.
qCFNB gives the quantile function.
rCFNB generates random values.
mCFNB gives the estimated parameters along with SE and goodness-of-fit measures.
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
\code{\link{pCLBio}
}
}

\examples{
x<-data_guineapigs
rCFNB(20,2,1,2,0.2)
dCFNB(x,2,1,1,0.3)
pCFNB(x,2,1,1,0.3)
qCFNB(0.7,2,1,1,0.2)
mCFNB(x,0.72,0.7,0.5,0.7, method="B")

}

