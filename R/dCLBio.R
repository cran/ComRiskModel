#' @export
dCLBio<-function (x, b, q, m, lambda, log = FALSE)
{
G=(1-((1+(x/b)))^(-q))
g=(q/b)*(1+(x/b))^(-q-1)

  pdf <- x
  pdf[log == FALSE] <-(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
  pdf[log == TRUE] <-log(lambda)+log(m)+log(g)+(m-1)*log(1-lambda*(1-G))-log(1-(1-lambda)^(m))
  return(pdf)
}

