#' @export
dCExpNB<-function (x, alpha,s,lambda, log = FALSE)
{
  G=(1-exp(-alpha*x))
  g=alpha*exp(-alpha*x)
  pdf <- x
  pdf[log == FALSE] <-(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
  pdf[log == TRUE] <-log(s)+log(lambda)+log(g)+(-s-1)*log(1-lambda*G)-log((1-lambda)^(-s)-1)
  return(pdf)
}

