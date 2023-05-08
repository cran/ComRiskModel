#' @export
#' @import stats

rCBXBio<-function(n, a, m, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCBXBio(p, a, m, lambda)
    return(rn)
	}
