#' @export
#' @import stats

rCFNB<-function(n, a, b, s, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCFNB(p, a, b, s, lambda)
    return(rn)
	}
