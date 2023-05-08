#' @export
dCBXBio<-function (x, a, m, lambda, log = FALSE)
{
  G=(1-exp(-x^2))^a
g=2*a*x*exp(-x^2)*(1-exp(-x^2))^(a-1)
  pdf <- x
  pdf[log == FALSE] <-(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
  pdf[log == TRUE] <-log(lambda)+log(m)+log(g)+(m-1)*log(1-lambda*(1-G))-log(1-(1-lambda)^(m))
  return(pdf)
}

