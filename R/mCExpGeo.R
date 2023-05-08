#' @export
#' @import AdequacyModel
#' @import graphics
mCExpGeo<-function (x, alpha,lambda, method="B")
{
pdf_CExpGeo<-function(par,x){
alpha=par[1]
lambda=par[2]
G=(1-exp(-alpha*x))
g=alpha*exp(-alpha*x)
F=(1-lambda)*G/(1-lambda*G)
f=(1-lambda)*(g)/(1-lambda*G)^2
return(f)
}
cdf_CExpGeo<-function(par,x){
alpha=par[1]
lambda=par[2]
G=(1-exp(-alpha*x))
g=alpha*exp(-alpha*x)
F=(1-lambda)*G/(1-lambda*G)
f=(1-lambda)*(g)/(1-lambda*G)^2
return(F)
}
res = suppressWarnings(AdequacyModel::goodness.fit(pdf = pdf_CExpGeo, cdf = cdf_CExpGeo, starts = c(alpha,lambda), data = x, method = method, mle = NULL))
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
