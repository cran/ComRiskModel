\name{CEENB distribution}
\alias{dCEENB}
\alias{pCEENB}
\alias{qCEENB}
\alias{rCEENB}
\alias{mCEENB}
\title{Complementary exponentiated exponential negative binomial distribution}

\description{
Evaluates the PDF, CDF, QF, random numbers and MLEs based on the complementary exponentiated exponential negative binomial (CEENB) distribution. The CDF of the complementary G binomial distribution is as follows:
\deqn{
F(x)=\frac{\left(1-\lambda G(x)\right)^{-s}-1}{(1-\lambda)^{-s}-1};\qquad\lambda\in\left(0,1\right),s>0,
}
where G(x) represents the baseline exponentiated exponential CDF, it is given by
\deqn{
G(x)=\left(1-\exp(-\alpha x)\right)^{\beta};\qquad\alpha,\beta>0.
}
By setting G(x) in the above Equation, yields the CDF of the CEENB distribution.
}

\usage{
dCEENB(x, alpha, beta, s, lambda, log = FALSE)
pCEENB(x, alpha, beta, s, lambda, log.p = FALSE, lower.tail = TRUE)
qCEENB(p, alpha, beta, s, lambda, log.p = FALSE, lower.tail = TRUE)
rCEENB(n, alpha, beta, s, lambda)
mCEENB(x, alpha, beta, s, lambda, method="B")
}

\arguments{
\item{x}{
A vector of (non-negative integer) quantiles.}
\item{p}{
A vector of probablities.
}
\item{n}{
The number of random values to be generated under the CEENB distribution.
}
\item{lambda}{
The strictly positive parameter of the negative binomial distribution \eqn{\lambda \in (0,1)}.
}
\item{s}{
The positive parameter of the negative binomial distribution \eqn{s >0 }.
}
\item{alpha}{
The strictly positive scale parameter of the baseline exponentiated exponential distribution (\eqn{\alpha > 0}).
}
\item{beta}{
The strictly positive shape parameter of the baseline exponentiated exponential distribution (\eqn{\beta > 0}).
}
\item{lower.tail}{if FALSE then 1-F(x) are returned and quantiles are computed 1-p.
}
\item{log}{if TRUE, probabilities p are given as log(p).
}
\item{log.p}{if TRUE, probabilities p are given for exp(p).
}
\item{method}{the procedure for optimizing the log-likelihood function after setting the intial values of the parameters and data values for which the CEENB distribution is fitted. It could be "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", or "SANN". "BFGS" is set as the default.
}
}

\details{
These functions allow for the evaluation of the PDF, CDF, QF, random numbers and MLEs of the unknown parameters with the standard error (SE) of the estimates of the CEENB distribution. Additionally, it offers goodness-of-fit statistics such as the AIC, BIC, -2L, A test, W test, Kolmogorov-Smirnov test, P-value, and convergence status.
}

\value{
dCEENB gives the (log) probability function.
pCEENB gives the (log) distribution function.
qCEENB gives the quantile function.
rCEENB generates random values.
mCEENB gives the estimated parameters along with SE and goodness-of-fit measures.
}

\references{
Tahir, M. H., & Cordeiro, G. M. (2016). Compounding of distributions: a survey and new generalized classes. Journal of Statistical Distributions and Applications, 3, 1-35.

Bakouch, H. S., Ristic, M. M., Asgharzadeh, A., Esmaily, L., & Al-Zahrani, B. M. (2012). An exponentiated exponential binomial distribution with application. Statistics & Probability Letters, 82(6), 1067-1081.

Nadarajah, S. (2011). The exponentiated exponential distribution: a survey. AStA Advances in Statistical Analysis, 95, 219-251.
}

\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}



%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{pCEEBio}
}
}

\examples{
x<-rCEENB(500,0.7,0.2,0.2,0.7)
dCEENB(x,2,1,2,0.2)
pCEENB(x,2,1,2,0.2)
qCEENB(0.7,2,1,2,0.2)
mCEENB(x,0.2,0.1,0.2,0.2, method="B")
}

