#' @export
#' @import stats

rCEENB<-function(n, alpha,beta,s,lambda)
	{
    p <- runif(n, min = 0, max = 1)
    rn <- qCEENB(p, alpha, beta, s, lambda)
    return(rn)
	}
