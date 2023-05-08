#' @export
#' @import stats

rCLNB<-function(n, b, q, s, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCLNB(p, b, q, s, lambda)
    return(rn)
	}
