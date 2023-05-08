#' @export
pCB12Bio<-function (x, a, b, k, m, lambda , log.p = FALSE, lower.tail = TRUE)
	{
	G=(1-((1+(x/a)^b))^(-k))
	 cdf <- x
    cdf[log.p == FALSE & lower.tail == TRUE] <-((1-lambda*(1-G))^(m)-(1-lambda)^(m))/(1-(1-lambda)^(m))
	cdf[log.p == TRUE & lower.tail == TRUE] <-m*log(1-lambda*(1-G))-m*log(1-lambda)-log(1-(1-lambda)^(m))
    cdf[log.p == FALSE & lower.tail == FALSE] <-(1-(1-lambda*(1-G))^(m))/(1-(1-lambda)^(m))
    cdf[log.p == TRUE & lower.tail == FALSE] <-log(1-(1-lambda*(1-G))^(m))-log(1-(1-lambda)^(m))
    return(cdf)
	}
