#' @export
#' @import AdequacyModel
#' @import graphics
mCBXBio<-function(x, a,m,lambda, method="B")
{
pdf_CBXBio<-function(par,x){
a=par[1]
m=par[2]
lambda=par[3]
G=(1-exp(-x^2))^a
g=2*a*x*exp(-x^2)*(1-exp(-x^2))^(a-1)
F=((1-lambda*(1-G))^(m)-(1-lambda)^(m))/(1-(1-lambda)^(m))
f=(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
return(f)
}
cdf_CBXBio<-function(par,x){
a=par[1]
m=par[2]
lambda=par[3]
G=(1-exp(-x^2))^a
g=2*a*x*exp(-x^2)*(1-exp(-x^2))^(a-1)
F=((1-lambda*(1-G))^(m)-(1-lambda)^(m))/(1-(1-lambda)^(m))
f=(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
return(F)
}
res = suppressWarnings(AdequacyModel::goodness.fit(pdf = pdf_CBXBio, cdf = cdf_CBXBio, starts = c(a,m,lambda), data = x, method = method, mle = NULL))
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

