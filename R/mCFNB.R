#' @export
#' @import AdequacyModel
#' @import graphics
mCFNB<-function (x, a, b, s, lambda, method="B")
{

pdf_CFNB<-function(par,x){
a=par[1]
b=par[2]
s=par[3]
lambda=par[4]
G=(1-((1+(x/a)^b))^(-1))
g=((1+(x/a)^b))^(-2) *b*x^(b-1)*a^(-b)
F=(((1-lambda*G)^(-s))-1)/(((1-lambda)^(-s))-1)
f=(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
return(f)
}
cdf_CFNB<-function(par,x){
a=par[1]
b=par[2]
s=par[3]
lambda=par[4]
  G=(1-((1+(x/a)^b))^(-1))
g=((1+(x/a)^b))^(-2) *b*x^(b-1)*a^(-b)
F=(((1-lambda*G)^(-s))-1)/(((1-lambda)^(-s))-1)
f=(s*lambda*(g)*(1-lambda*G)^(-s-1))/((1-lambda)^(-s)-1)
return(F)
}
res = suppressWarnings(AdequacyModel::goodness.fit(pdf = pdf_CFNB, cdf = cdf_CFNB, starts = c(a, b, s, lambda), data = x, method = method, mle = NULL))
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
