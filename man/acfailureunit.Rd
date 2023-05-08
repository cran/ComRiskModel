\name{Air Conditioning Failure Unit Interval}
\alias{data_acfailureunit}
\title{The unit interval data set  consists of the failure times of the air conditioning system of an airplane (in hours)
}
\usage{
data_acfailureunit
}

\description{
The function allows to provide the unit interval failure times of the air conditioning system of an airplane (in hours).
}
\arguments{
\item{data_acfailureunit}{
A vector of (non-negative integer) values.
}
}
\details{The unit interval data set  consists of the failure times of the air conditioning system of an airplane (in hours).
Recently, it is used by Bantan et al. (2020) and fitted the unit-Rayleigh distribution.
}

\value{
data_acfailureunit gives the failure times of the air conditioning system of an airplane (in hours).
}

\references{

Bantan, R. A., Chesneau, C., Jamal, F., Elgarhy, M., Tahir, M. H., Ali, A., ... & Anam, S. (2020).
Some new facts about the unit-Rayleigh distribution with applications. Mathematics, 8(11), 1954.

Linhart, H., & Zucchini, W. (1986). Model selection. John Wiley & Sons.
}


\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}

%\note{
%%  ~~further notes~~
%}



\examples{
x<-data_acfailureunit
summary(x)
}
