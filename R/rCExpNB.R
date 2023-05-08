#' @export
#' @import stats

rCExpNB<-function(n, alpha,s,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCExpNB(p, alpha, s,lambda)
    return(rn)
	}
