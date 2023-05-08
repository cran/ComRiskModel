#' @export
pCB12Geo<-function (x, a, b, k, lambda , log.p = FALSE, lower.tail = TRUE)
	{
	G=(1-((1+(x/a)^b))^(-k))
	cdf <- x
    cdf[log.p == FALSE & lower.tail == TRUE] <-(1-lambda)*G/(1-lambda*G)
	cdf[log.p == TRUE & lower.tail == TRUE] <-(1-lambda)*log(G)-log(1-lambda*G)
    cdf[log.p == FALSE & lower.tail == FALSE] <-(1-G)/(1-lambda*G)
    cdf[log.p == TRUE & lower.tail == FALSE] <-log(1-G)-log(1-lambda*G)
    return(cdf)
	}
