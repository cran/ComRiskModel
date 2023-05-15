\name{CFBio distribution}
\alias{dCFBio}
\alias{pCFBio}
\alias{qCFBio}
\alias{rCFBio}
\alias{mCFBio}
\title{Complementary Fisk binomial distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Fisk binomial (CFBio) distribution.
The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left[1-\lambda(1-G(x))\right]^{m}-(1-\lambda)^{m}}{1-(1-\lambda)^{m}};\qquad\lambda\in\left(0,1\right),\,m\geq1,
}
where G(x) represents the baseline Fisk CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{a}\right)^{b}\right]^{-1};\qquad a,b>0.
}
By setting G(x) in the above Equation, yields the CDF of the CFBio distribution.
}



\usage{
dCFBio(x, a, b, m, lambda, log = FALSE)
pCFBio(x, a, b, m, lambda, log.p = FALSE, lower.tail = TRUE)
qCFBio(p, a, b, m, lambda, log.p = FALSE, lower.tail = TRUE)
rCFBio(n, a, b, m, lambda)
mCFBio(x, a, b, m, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CFBio distribution.
}
\item{lambda}{
The strictly positive parameter of the binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{m}{
The positive parameter of the binomial distribution \eqn{m \geq 1}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CFBio distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CFBio distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}


\value{
dCFBio gives the (log) probability function.
pCFBio gives the (log) distribution function.
qCFBio gives the quantile function.
rCFBio generates random values.
mCFBio gives the estimated parameters along with SE and goodness-of-fit measures.
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
\code{\link{pCLGeo}
}
}

\examples{
x<-data_guineapigs
rCFBio(20,2,1,2,0.2)
dCFBio(x,2,1,1,0.3)
pCFBio(x,2,1,1,0.3)
qCFBio(0.7,2,1,1,0.2)
mCFBio(x,0.07,0.102,0.102,0.203, method="B")
}

