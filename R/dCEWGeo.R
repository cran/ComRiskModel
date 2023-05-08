#' @export
dCEWGeo<-function (x, alpha, beta, theta, lambda, log = FALSE)
{
 G=(1-exp(-alpha*x^(beta)))^theta
g=alpha*beta*x^(beta-1)*exp(-alpha*x^beta)*theta*(1-exp(-alpha*x^(beta)))^(theta-1)


  pdf <- x
  pdf[log == FALSE] <-(1-lambda)*(g)/(1-lambda*G)^2
  pdf[log == TRUE] <-(1-lambda)*log(g)-2*log(1-lambda*G)
  return(pdf)
}

