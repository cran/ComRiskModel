#' @export
dCFNB<-function (x, a, b, s, lambda, log = FALSE)
{
G=(1-((1+(x/a)^b))^(-1))
g=((1+(x/a)^b))^(-2) *b*x^(b-1)*a^(-b)


  pdf <- x
  pdf[log == FALSE] <-(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
  pdf[log == TRUE] <-log(s)+log(lambda)+log(g)+(-s-1)*log(1-lambda*G)-log((1-lambda)^(-s)-1)
  return(pdf)
}

