#' @export
#' @import stats

rCExpGeo<-function(n, alpha,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCExpGeo(p, alpha, lambda)
    return(rn)
	}
