#' @export
#' @import stats

rCEWGeo<-function(n, alpha,beta,theta,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCEWGeo(p, alpha, beta, theta, lambda)
    return(rn)
	}
