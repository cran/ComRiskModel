\name{CLBio distribution}
\alias{dCLBio}
\alias{pCLBio}
\alias{qCLBio}
\alias{rCLBio}
\alias{mCLBio}
\title{Complementary Lomax binomial distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Lomax binomial (CLBio) distribution.
The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left[1-\lambda(1-G(x))\right]^{m}-(1-\lambda)^{m}}{1-(1-\lambda)^{m}};\qquad\lambda\in\left(0,1\right),\,m\geq1,
}
where G(x) represents the baseline Lomax CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{b}\right)\right]^{-q};\qquad b,q>0.
}
By setting G(x) in the above Equation, yields the CDF of the CLBio distribution.
}

\usage{
dCLBio(x, b, q, m, lambda, log = FALSE)
pCLBio(x, b, q, m, lambda, log.p = FALSE, lower.tail = TRUE)
qCLBio(p, b, q, m, lambda, log.p = FALSE, lower.tail = TRUE)
rCLBio(n, b, q, m, lambda)
mCLBio(x, b, q, m, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CLBio distribution.
}
\item{lambda}{
The strictly positive parameter of the binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{m}{
The positive parameter of the binomial distribution \eqn{m \geq 1}.
}
\item{b}{
The strictly positive parameter of the baseline Lomax distribution (\eqn{b > 0}).
}
\item{q}{
The strictly positive shapes parameter of the baseline Lomax distribution (\eqn{q > 0}).
}
\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}

\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CLBio distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CLBio distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCLBio gives the (log) probability function.
pCLBio gives the (log) distribution function.
qCLBio gives the quantile function.
rCLBio generates random values.
mCLBio gives the estimated parameters along with SE and goodness-of-fit measures.
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
\code{\link{pCFGeo}
}
}

\examples{
x<-rCLBio(20,2,1,2,0.7)
dCLBio(x,2,1,2,0.5)
pCLBio(x,2,1,2,0.3)
qCLBio(0.7,2,1,2,0.2)
mCLBio(x,0.2,0.1,0.2,0.5, method="B")
}

