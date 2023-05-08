#' @export
#' @import stats

rCLBio<-function(n, b, q, m, lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCLBio(p, b, q, m, lambda)
    return(rn)
	}
