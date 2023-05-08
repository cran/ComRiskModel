#' @export
#' @import stats

rCWBio<-function(n, alpha,beta,m,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCWBio(p, alpha, beta, m, lambda)
    return(rn)
	}
