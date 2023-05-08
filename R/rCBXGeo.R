#' @export
#' @import stats

rCBXGeo<-function(n, a,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCBXGeo(p, a, lambda)
    return(rn)
	}
