#' @export
#' @import stats

rCBXNB<-function(n, a, s, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCBXNB(p, a, s, lambda)
    return(rn)
	}
