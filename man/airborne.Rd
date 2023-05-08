\name{Airborne Variations}
\alias{data_airborne}

\title{Variations in airborne exposure on the concentration of urinary metabolites
}

\usage{
data_airborne
}

\description{
The function allows to provide the effects of variations in airborne exposure on the concentration of urinary metabolites.
}

\arguments{
\item{data_airborne}{
A vector of (non-negative integer) values.
}
}

\details{The data relates to the effects of variations in airborne exposure on the concentration of urinary metabolites.
Recently, it is used by Peter et al. (2021) and fitted the Gamma odd Burr III-G family of distributions.
}

\value{
data_airborne gives the effects of variations in airborne exposure on the concentration of urinary metabolites.
}

\references{
Peter, P. O., Oluyede, B., Bindele, H. F., Ndwapi, N., & Mabikwa, O. (2021). The Gamma Odd Burr III-G Family of Distributions: Model, Properties and Applications.
Revista Colombiana de Estadistica, 44(2), 331-368.

Kumagai, S., & Matsunaga, I. (1995). Physiologically based pharmacokinetic model for acetone. Occupational and environmental medicine, 52(5), 344-352.
}


\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}

%\note{
%%  ~~further notes~~
%}

\examples{
x<-data_airborne
summary(x)
}
