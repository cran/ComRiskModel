#' @export
#' @import stats

rCLGeo<-function(n, b, q, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCLGeo(p, b, q, lambda)
    return(rn)
	}
