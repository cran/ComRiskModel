\name{Actuarial data}
\alias{data_actuarialm}
\title{The mortality of retired people
}
\usage{
data_actuarialm
}

\description{
The function allows to provide the distributional
behavior of the mortality of retired people on disability of the Mexican Institute of Social
Security.
}
\arguments{
\item{data_actuarialm}{
A vector of (non-negative integer) values.
}
}
\details{The data describes the distributional
behavior of the mortality of retired people on disability of the Mexican Institute of Social
Security.
Recently, it is used by Tahir et al. (2021) and fitted the Kumaraswamy Pareto IV distribution.
}

\value{
data_actuarialm gives the mortality of retired people.
}

\references{
Tahir, M. H., Cordeiro, G. M., Mansoor, M., Zubair, M., & Alzaatreh, A. (2021).
The Kumaraswamy Pareto IV Distribution. Austrian Journal of Statistics, 50(5), 1-22.

Balakrishnan, N., Leiva, V., Sanhueza, A., & Cabrera, E. (2009). Mixture inverse Gaussian distributions and its transformations,
moments and applications. Statistics, 43(1), 91-104.

}


\author{
Muhammad Imran and M.H Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and M.H Tahir \email{mht@iub.edu.pk}.
}

%\note{
%%  ~~further notes~~
%}



\examples{
x<-data_actuarialm
summary(x)
}
