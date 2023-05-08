#' @export
#' @import stats

rCEEBio<-function(n, alpha,beta,m,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCEEBio(p, alpha, beta, m, lambda)
    return(rn)
	}
