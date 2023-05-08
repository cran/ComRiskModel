#' @export
#' @import stats

rCEWBio<-function(n, alpha,beta,theta,m,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCEWBio(p, alpha, beta, theta, m, lambda)
    return(rn)
	}
