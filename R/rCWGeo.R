#' @export
#' @import stats

rCWGeo<-function(n, alpha,beta,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCWGeo(p, alpha, beta, lambda)
    return(rn)
	}
