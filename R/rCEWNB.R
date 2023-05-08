#' @export
#' @import stats

rCEWNB<-function(n, alpha,beta,theta,s,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCEWNB(p, alpha, beta, theta, s, lambda)
    return(rn)
	}
