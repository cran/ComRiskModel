#' @export
#' @import stats

rCFGeo<-function(n, a, b, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCFGeo(p, a, b, lambda)
    return(rn)
	}
