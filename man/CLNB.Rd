\name{CLNB distribution}
\alias{dCLNB}
\alias{pCLNB}
\alias{qCLNB}
\alias{rCLNB}
\alias{mCLNB}
\title{Complementary Lomax negative binomial distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Lomax negative binomial (CLNB) distribution.
The CDF of the complementary G negative binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline Lomax CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{b}\right)\right]^{-q};\qquad b,q>0.
}
By setting G(x) in the above Equation, yields the CDF of the CLNB distribution.
}

\usage{
dCLNB(x, b, q, s, lambda, log = FALSE)
pCLNB(x, b, q, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCLNB(p, b, q, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCLNB(n, b, q, s, lambda)
mCLNB(x, b, q, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CLNB distribution.
}
\item{lambda}{
The strictly positive parameter of the negative binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution \eqn{s > 0}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CLNB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CLNB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}


\value{
dCLNB gives the (log) probability function.
pCLNB gives the (log) distribution function.
qCLNB gives the quantile function.
rCLNB generates random values.
mCLNB gives the estimated parameters along with SE and goodness-of-fit measures.
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
\code{\link{pCFBio}
}
}

\examples{
x<-rCLNB(20,2,1,2,0.7)
dCLNB(x,2,1,2,0.5)
pCLNB(x,2,1,2,0.3)
qCLNB(0.7,2,1,2,0.2)
mCLNB(x,0.2,0.1,0.2,0.5, method="B")
}

