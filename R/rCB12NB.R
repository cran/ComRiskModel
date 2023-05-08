#' @export
#' @import stats

rCB12NB<-function(n, a, b, k, s, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCB12NB(p, a, b, k, s, lambda)
    return(rn)
	}
