\name{CLGeo distribution}
\alias{dCLGeo}
\alias{pCLGeo}
\alias{qCLGeo}
\alias{rCLGeo}
\alias{mCLGeo}
\title{Complementary Lomax geomatric distribution}

\description{Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary Lomax geomatric (CLGeo) distribution.
The CDF of the complementary G geomatric distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda\right)G(x)}{\left(1-\lambda G(x)\right)};\qquad\lambda\in(0,1),
}
where G(x) represents the baseline Lomax CDF, it is given by
\deqn{
G\left(x\right)=1-\left[1+\left(\frac{x}{b}\right)\right]^{-q};\qquad b,q>0.
}
By setting G(x) in the above Equation, yields the CDF of the CLGeo distribution.
}

\usage{
dCLGeo(x, b, q, lambda, log = FALSE)
pCLGeo(x, b, q, lambda, log.p = FALSE, lower.tail = TRUE)
qCLGeo(p, b, q, lambda, log.p = FALSE, lower.tail = TRUE)
rCLGeo(n, b, q, lambda)
mCLGeo(x, b, q, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.
}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CLGeo distribution.
}
\item{lambda}{
The strictly positive parameter of the geomatric distribution \eqn{\lambda \in (0,1)}.
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
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CLGeo distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CLGeo distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCLGeo gives the (log) probability function.
pCLGeo gives the (log) distribution function.
qCLGeo gives the quantile function.
rCLGeo generates random values.
mCLGeo gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes.
Journal of Statistical Distributions and Applications, 3, 1-35.

Hassan, A. S., & Abdelghafar, M. A. (2017). Exponentiated Lomax geometric distribution: properties and applications. Pakistan Journal of Statistics and Operation Research, 545-566.

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
x<-rCLGeo(20,2,1,0.7)
dCLGeo(x,2,1,0.5)
pCLGeo(x,2,1,0.3)
qCLGeo(0.7,2,1,0.2)
mCLGeo(x,0.2,0.1,0.5, method="B")
}

