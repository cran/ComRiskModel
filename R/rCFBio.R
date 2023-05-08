#' @export
#' @import stats

rCFBio<-function(n, a, b, m, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCFBio(p, a, b, m, lambda)
    return(rn)
	}
