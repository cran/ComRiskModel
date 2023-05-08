#' @export
#' @import stats

rCWNB<-function(n, alpha,beta,s,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCWNB(p, alpha, beta, s, lambda)
    return(rn)
	}
