\name{Guinea Pigs}
\alias{data_guineapigs}
\title{ The survival times of guinea pigs infected
}
\usage{
data_guineapigs
}

\description{
The function allows to provide survival times (in days) of 72 guinea pigs infected with virulent tubercle bacilli.
}
\arguments{
\item{data_guineapigs}{
A vector of (non-negative integer) values.
}
}
\details{The data set represents the survival times (in days) of 72 guinea pigs infected
with virulent tubercle bacilli.
Recently, the data set is used by Alyami et al.(2022) and fitted the Topp-Leone modified Weibull model.
}

\value{
data_guineapigs gives the survival times (in days) of 72 guinea pigs infected with virulent tubercle bacilli.
}

\references{

Bjerkedal, T. (1960). Acquisition of Resistance in Guinea Pies infected with Different Doses of Virulent Tubercle Bacilli. American Journal of Hygiene, 72(1), 130-48.

Chesneau, C., & El Achi, T. (2020). Modified odd Weibull family of distributions: Properties and applications. Journal of the Indian Society for Probability and Statistics, 21, 259-286.

Khosa, S. K., Afify, A. Z., Ahmad, Z., Zichuan, M., Hussain, S., & Iftikhar, A. (2020). A new extended-f family: properties and applications to lifetime data. Journal of Mathematics, 2020, 1-9.

Alyami, S. A., Elbatal, I., Alotaibi, N., Almetwally, E. M., Okasha, H. M., & Elgarhy, M. (2022). Topp-Leone Modified Weibull Model: Theory and Applications to Medical and Engineering Data. Applied Sciences, 12(20), 10431.

Kemaloglu, S. A., & Yilmaz, M. (2017). Transmuted two-parameter Lindley distribution. Communications in Statistics-Theory and Methods, 46(23), 11866-11879.

}
\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}

%\note{
%%  ~~further notes~~
%}


\examples{
x<-data_guineapigs
summary(x)

}
