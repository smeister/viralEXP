#' Most Probable Number (MPN) function
#'
#' Calculates the MPN according to a data.frame (or list).
#' @param x A numeric vector containing values of the positive wells.
#' @param n A numeric vector containing the number of MPN replicates
#' @param v A numeric vector containing the sample volumes
#' @return The MPN value
#' @export

getMPN=function(x,n,v){
  if(is.na(x[1]) | sum(y)==0){
    return(NA)
  } else{
    MPN=as.vector(mle2(function(mu){sum(suppressWarnings((n-x)*mu*v-x*log(1-exp(-mu*v))))},start=list(mu=1),method="SANN",optimizer="nlminb")@details$par)
    return(MPN)
  }
}
