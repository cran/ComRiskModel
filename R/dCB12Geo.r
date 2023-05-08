#' @export



dCB12Geo<-function (x, a, b, k, lambda, log = FALSE)
{
G=(1-((1+(x/a)^b))^(-k))
g=k*((1+(x/a)^b))^(-k-1) *b*x^(b-1)*a^(-b)


  pdf <- x
  pdf[log == FALSE] <-(1-lambda)*(g)/(1-lambda*G)^2
  pdf[log == TRUE] <-(1-lambda)*log(g)-2*log(1-lambda*G)
  return(pdf)
}




