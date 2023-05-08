#' @export
#' @import stats

rCEEGeo<-function(n, alpha,beta,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCEEGeo(p, alpha, beta, lambda)
    return(rn)
	}
