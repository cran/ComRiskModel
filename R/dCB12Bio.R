#' @export



dCB12Bio<-function (x, a, b, k, m, lambda, log = FALSE)
{
G=(1-((1+(x/a)^b))^(-k))
g=k*((1+(x/a)^b))^(-k-1) *b*x^(b-1)*a^(-b)


  pdf <- x
  pdf[log == FALSE] <-(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
  pdf[log == TRUE] <-log(lambda)+log(m)+log(g)+(m-1)*log(1-lambda*(1-G))-log(1-(1-lambda)^(m))
  return(pdf)
}

