#' Most Probable Number (MPN) function
#'
#' Calculates the MPN according to a data.frame (or list).
#' @param x A numeric vector containing values of the positive wells.
#' @param n Number of MPN replicates
#' @param v Starting volume in ml
#' @param d Dilution factor
#' @return data frame containing the MPN cytopathic unit per ml (MPNCU.ml), standard error (SE), Upper and Lower 95% confidence interval (U_95CI and L_95CI)
#' @export

getMPN=function(x,n,v,d){
  if(is.na(x[1]) | sum(x)==0){
    return(NA)
  } else{
    n<-rep(n,length(x))
    v<-repDIL(v,length(x),d)
    MPN=as.vector(mle2(function(mu){sum(suppressWarnings((n-x)*mu*v-x*log(1-exp(-mu*v))))},start=list(mu=1),method="SANN",optimizer="nlminb")@details$par)
    SE=(mu^2*sum((n*v^2)/(exp(mu*v)-1)))^-0.5
    LL=signif(exp(log(mu)-qnorm(1-0.05/2,0,1)*SE),digits=3)
    UL=signif(exp(log(mu)+qnorm(1-0.05/2,0,1)*SE),digits=3)
    return(list('MPNCU.ml'=MPN, "U_95CI"=UL, "L_95CI"=LL, "SE"=SE, "x"=x, "n"=n, "v"=v))
  }
}


x<-c(5,5,5,3,2,0)
getMPN(x, 5, 0.01, 10)
