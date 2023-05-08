#' @export
dCLGeo<-function (x, b, q, lambda, log = FALSE)
{
G=(1-((1+(x/b)))^(-q))
g=(q/b)*(1+(x/b))^(-q-1)

  pdf <- x
  pdf[log == FALSE] <-(1-lambda)*(g)/(1-lambda*G)^2
  pdf[log == TRUE] <-(1-lambda)*log(g)-2*log(1-lambda*G)
  return(pdf)
}

