\name{Acute Bone Cancer}
\alias{data_acutebcancer}
\title{The survival times of 73 patients with acute bone cancer}
\usage{
data_acutebcancer
}

\description{
The function allows to provide the survival times (in days) of 73 patients who diagnosed with acute bone cancer.
}
\arguments{
\item{data_acutebcancer}{
A vector of (non-negative integer) values.
}
}
\details{The data represents the survival times (in days) of 73 patients who
diagnosed with acute bone cancer.
Recently, the data set is used by Klakattawi, H. S. (2022) and fitted a new extended Weibull distribution.
}

\value{
data_acutebcancer gives the survival times (in days) of 73 patients who diagnosed with acute bone cancer.
}

\references{

Klakattawi, H. S. (2022). Survival analysis of cancer patients using a new extended Weibull distribution. Plos one, 17(2), e0264229.

Alanzi, A. R., Imran, M., Tahir, M. H., Chesneau, C., Jamal, F., Shakoor, S., & Sami, W. (2023). Simulation analysis, properties and applications
on a new Burr XII model based on the Bell-X functionalities.

Mansour, M., Yousof, H. M., Shehata, W. A., & Ibrahim, M. (2020). A new two parameter Burr XII distribution: properties, copula, different estimation methods and modeling
acute bone cancer data. Journal of Nonlinear Science and Applications, 13(5), 223-238.
}
\author{
Muhammad Imran and H.M Tahir.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com} and H.M Tahir \email{mht@iub.edu.pk}.
}

%\note{
%%  ~~further notes~~
%}



\examples{
x<-data_acutebcancer
summary(x)

}
