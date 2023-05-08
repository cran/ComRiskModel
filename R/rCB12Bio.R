#' @export
#' @import stats

rCB12Bio<-function(n, a, b, k, m, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCB12Bio(p, a, b, k, m, lambda)
    return(rn)
	}
