#' @export
#' @import AdequacyModel
#' @import graphics
mCEEBio<-function (x, alpha, beta, m, lambda, method="B")
{

pdf_CEEBio<-function(par,x){
alpha=par[1]
beta=par[2]
m=par[3]
lambda=par[4]
G=(1-exp(-alpha*x))^beta
g=alpha*exp(-alpha*x)*beta*(1-exp(-alpha*x))^(beta-1)
F=((1-lambda*(1-G))^(m)-(1-lambda)^(m))/(1-(1-lambda)^(m))
f=(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
return(f)
}
cdf_CEEBio<-function(par,x){
alpha=par[1]
beta=par[2]
m=par[3]
lambda=par[4]
G=(1-exp(-alpha*x))^beta
g=alpha*exp(-alpha*x)*beta*(1-exp(-alpha*x))^(beta-1)
F=((1-lambda*(1-G))^(m)-(1-lambda)^(m))/(1-(1-lambda)^(m))
f=(lambda*g*m*(1-lambda*(1-G))^(m-1))/(1-(1-lambda)^(m))
return(F)
}
res = suppressWarnings(AdequacyModel::goodness.fit(pdf = pdf_CEEBio, cdf = cdf_CEEBio, starts = c(alpha,beta,m,lambda), data = x, method = method, mle = NULL))
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
