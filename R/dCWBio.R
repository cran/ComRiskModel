#' @export
dCWBio<-function (x, alpha,beta,m,lambda, log = FALSE)
{
  G=(1-exp(-alpha*x^beta))
g=alpha*beta*x^(beta-1)*exp(-alpha*x^beta)
  pdf <- x
  pdf[log == FALSE] <-(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
  pdf[log == TRUE] <-log(lambda)+log(m)+log(g)+(m-1)*log(1-lambda*(1-G))-log(1-(1-lambda)^(m))
  return(pdf)
}

