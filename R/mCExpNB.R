#' @export
#' @import AdequacyModel
#' @import graphics
mCExpNB<-function(x, alpha,s,lambda, method="B")
{
pdf_CExpNB<-function(par,x){
alpha=par[1]
s=par[2]
lambda=par[3]
G=(1-exp(-alpha*x))
g=alpha*exp(-alpha*x)
F=(((1-lambda*G)^(-s))-1)/(((1-lambda)^(-s))-1)
f=(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
return(f)
}
cdf_CExpNB<-function(par,x){
alpha=par[1]
s=par[2]
lambda=par[3]
G=(1-exp(-alpha*x))
g=alpha*exp(-alpha*x)
F=(((1-lambda*G)^(-s))-1)/(((1-lambda)^(-s))-1)
f=(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
return(F)
}
res = suppressWarnings(AdequacyModel::goodness.fit(pdf = pdf_CExpNB, cdf = cdf_CExpNB, starts = c(alpha,s,lambda), data = x, method = method, mle = NULL))
aux = cbind(res$mle, res$Erro)
colnames(aux) = c("MLE", "SE")
aux1 = cbind(res$AIC, res$BIC, res$W,res$A, res$Value)
  colnames(aux1) = c("AIC",  "BIC",  "W", "A","-2L")
  rownames(aux1) = c("")
  aux2 = cbind(res$KS$statistic, res$KS$p.value)
  colnames(aux2) = c("KS Statistic", "KS p-value")
  rownames(aux2) = c("")
  aux3 = cbind(if (res$Convergence == 0) {
    "Converged"
  }
  else {
    "Not Converged"
  })

	  
  colnames(aux3) = c("")
  rownames(aux3) = c("")
  list(Estimates = aux, `Goodness-of-Fit Tests`  = aux1, `Kolmogorov-Smirnov Test` = aux2,
       `Convergence Status` = aux3)
  }
