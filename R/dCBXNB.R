#' @export
dCBXNB<-function (x, a, s, lambda, log = FALSE)
{
  G=(1-exp(-x^2))^a
g=2*a*x*exp(-x^2)*(1-exp(-x^2))^(a-1)
  pdf <- x
  pdf[log == FALSE] <-(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
  pdf[log == TRUE] <-log(s)+log(lambda)+log(g)+(-s-1)*log(1-lambda*G)-log((1-lambda)^(-s)-1)
  return(pdf)
}

