#' @export
#' @import stats

rCExpBio<-function(n, alpha,m,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCExpBio(p, alpha, m,lambda)
    return(rn)
	}
