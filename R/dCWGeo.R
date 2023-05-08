#' @export
dCWGeo<-function (x, alpha,beta,lambda, log = FALSE)
{
  G=(1-exp(-alpha*x^beta))
g=alpha*beta*x^(beta-1)*exp(-alpha*x^beta)
 pdf <- x
  pdf[log == FALSE] <-(1-lambda)*(g)/(1-lambda*G)^2
  pdf[log == TRUE] <-(1-lambda)*log(g)-2*log(1-lambda*G)
  return(pdf)
}

