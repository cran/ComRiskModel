#' @export
dCBXGeo<-function (x, a,lambda, log = FALSE)
{
  G=(1-exp(-x^2))^a
g=2*a*x*exp(-x^2)*(1-exp(-x^2))^(a-1)
  pdf <- x
  pdf[log == FALSE] <-(1-lambda)*(g)/(1-lambda*G)^2
  pdf[log == TRUE] <-(1-lambda)*log(g)-2*log(1-lambda*G)
  return(pdf)
}

