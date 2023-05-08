#' @export



dCB12NB<-function (x, a, b, k, s, lambda, log = FALSE)
{
G=(1-((1+(x/a)^b))^(-k))
g=k*((1+(x/a)^b))^(-k-1) *b*x^(b-1)*a^(-b)


  pdf <- x
  pdf[log == FALSE] <-(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
  pdf[log == TRUE] <-log(s)+log(lambda)+log(g)+(-s-1)*log(1-lambda*G)-log((1-lambda)^(-s)-1)
  return(pdf)
}

