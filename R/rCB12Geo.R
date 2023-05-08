#' @export
#' @import stats

rCB12Geo<-function(n, a, b, k, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCB12Geo(p, a, b, k, lambda)
    return(rn)
	}
