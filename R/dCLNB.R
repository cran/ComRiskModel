#' @export
dCLNB<-function (x, b, q, s, lambda, log = FALSE)
{
G=(1-((1+(x/b)))^(-q))
g=(q/b)*(1+(x/b))^(-q-1)

  pdf <- x
  pdf[log == FALSE] <-(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
  pdf[log == TRUE] <-log(s)+log(lambda)+log(g)+(-s-1)*log(1-lambda*G)-log((1-lambda)^(-s)-1)
  return(pdf)
}

