#' Most Probable Number (MPN) function
#'
#' Calculates the MPN according to a data.frame (or list).
#' @param x A numeric vector containing values of the positive wells.
#' @param n Number of MPN replicates
#' @param v Starting volume in ml
#' @param d Dilution factor, 10 as default
#' @return data frame containing the MPN cytopathic unit per ml (MPNCU.ml), Upper and Lower 95% confidence interval (U_95CI and L_95CI), and standard error (SE)
#' @export

getMPN=function(x,n,v,d=10){
  library(bbmle)
  if(is.na(x[1]) | sum(x)==0){
    return(NA)
  } else {
    n<-rep(n,length(x))
    v<-repDIL(v,length(x),d)
    MPN=as.vector(mle2(function(mu){sum(suppressWarnings((n-x)*mu*v-x*log(1-exp(-mu*v))))},start=list(mu=1),method="SANN",optimizer="nlminb")@details$par)
    SE=(MPN^2*sum((n*v^2)/(exp(MPN*v)-1)))^-0.5
    LL=signif(exp(log(MPN)-qnorm(1-0.05/2,0,1)*SE),digits=3)
    UL=signif(exp(log(MPN)+qnorm(1-0.05/2,0,1)*SE),digits=3)
    return(list("Results"=data.frame('MPNCU.ml'=MPN, "Upper 95CI"=UL, "Lower 95CI"=LL, "Std.err"=SE), "raw.data"=data.frame("x"=x, "n"=n, "v"=v)))
  }
}


