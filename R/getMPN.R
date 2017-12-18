#' Most Probable Number (MPN) function
#'
#' Calculates the MPN
#' @param x A numeric vector containing values of the positive wells.
#' @param n Number of MPN replicates
#' @param v Starting volume in ml or vector of volume
#' @param d Dilution factor, 10 as default
#' @return data frame containing the MPN cytopathic unit per ml (MPNCU.ml), Upper and Lower 95% confidence interval (U_95CI and L_95CI), and standard error (SE)
#' @export

getMPN=function(x,n,v,d=10){
  if (length(v)==1) {
    v<-repDIL(v,length(x),d)
  }
  if (length(n)==1) {
    n<-rep(n,length(x))
  }
  if(is.na(x[1]) | sum(x)==0){
    return(list("raw.data"=data.frame("x"=rep(NA,length(x)), "n"=n, "v"=v),"Results"=data.frame('MPNCU.ml'=NA, "Upper 95CI"=NA, "Lower 95CI"=NA, "Std.err"=NA)))
  } else {
    MPN=as.vector(mle2(function(mu){sum(suppressWarnings((n-x)*mu*v-x*log(1-exp(-mu*v))))},start=list(mu=1),method="SANN",optimizer="nlminb")@details$par)
    SE=(MPN^2*sum((n*v^2)/(exp(MPN*v)-1)))^-0.5 # Standard error
    LL=signif(exp(log(MPN)-qnorm(1-0.05/2,0,1)*SE),digits=3) # Lower 95% confidence interval
    UL=signif(exp(log(MPN)+qnorm(1-0.05/2,0,1)*SE),digits=3) # Upper 95% confidence interval
    output<-list("raw.data"=data.frame("x"=x, "n"=n, "v"=v),"Results"=data.frame('MPNCU.ml'=MPN, "Upper 95CI"=UL, "Lower 95CI"=LL, "Std.err"=SE))
    class(output)<-"MPN"
    return(output)
  }
}


