#' @export
qCBXGeo<-function(p,a,lambda,log.p = FALSE, lower.tail = TRUE){
	if (log.p == TRUE)
        p <- exp(p)
    if (lower.tail == FALSE)
        p <- 1 - p
	qf <- rep(NaN, length(p))
	t=(p[p >= 0 & p <= 1]/(p[p >= 0 & p <= 1]*lambda+1-lambda))

	qf[p >= 0 & p <= 1] <- (-1*log(1-(t)^(1/a)))^(0.5)
    return(qf)
	}
