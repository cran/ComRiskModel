#' @export
dCExpGeo<-function (x, alpha,lambda, log = FALSE)
{
  G=(1-exp(-alpha*x))
  g=alpha*exp(-alpha*x)
  pdf <- x
  pdf[log == FALSE] <-(1-lambda)*(g)/(1-lambda*G)^2
  pdf[log == TRUE] <-(1-lambda)*log(g)-2*log(1-lambda*G)
  return(pdf)
}

