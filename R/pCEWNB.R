#' @export
pCEWNB<-function (x,  alpha, beta, theta, s, lambda, log.p = FALSE, lower.tail = TRUE)
	{
	G=(1-exp(-alpha*x^(beta)))^theta
	 cdf <- x
    cdf[log.p == FALSE & lower.tail == TRUE] <-(((1-lambda*G)^(-s))-1)/(((1-lambda)^(-s))-1)
	cdf[log.p == TRUE & lower.tail == TRUE] <-log(((1-lambda*G)^(-s))-1)-log(((1-lambda)^(-s))-1)
    cdf[log.p == FALSE & lower.tail == FALSE] <-(((1-lambda)^(-s))-((1-lambda*G)^(-s)))/((1-lambda)^(-s)-1)
    cdf[log.p == TRUE & lower.tail == FALSE] <--s*log(1-lambda)+s*log(1-lambda*G)-log((1-lambda)^(-s)-1)
    return(cdf)
	}
