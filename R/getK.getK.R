#' This is the getK S3 function for class "getK" objects
#'  @export
#'

getK.getK <- getK<-function (..., timeVECT) {
  # Merge the different getMPN or single replicates getK inputs
  input<-list(...)
  if (length(input)>1) {
    for (k in 1:length(input)) {
      input[[k]]$raw.data$rep<-rep(k,length(input[[k]]$raw.data$x))
    }
    theDF<-data.frame()
    for (l in 1:length(input)) { # Creating the dataframe with all data
      theDF<-rbind(theDF,input[[l]]$raw.data)
    }
  } else { # Here for direct data.frame input
    theDF<-data.frame(input)
  }

  theDF$t<-as.numeric(as.character(theDF$t))
  theDF$v<-as.numeric(as.character(theDF$v))
  theDF$rep<-as.numeric(as.character(theDF$rep))
  # Empty vectors for fill up
  Results_list<-list()
  lnDiff_grouped<-c()
  times_grouped<-c()
  mu0s<-c()
  LP1<-c()
  LP2<-c()
  k_val<-c()
  Intercept<-c()
  results_names<-c()
  MPNs<-c()
  lnDiffs<-c()
  logDiffs<-c()
  Std.err<-c()

  for (i in 1:length(unique(theDF$rep))) { # subset the replicate
    theDF2<-subset(theDF, rep == unique(theDF$rep)[i])
    MPN<-c();lnDiff<-c();logDiff<-c();rep<-c();times<-c()
    for (j in 1:length(unique(theDF2$t))) { # subset the time point
      # getMPN calculation per timepoint
      theDF3<-subset(theDF2, t == unique(theDF2$t)[j])
      if (j == 1) {
        mu0s<-c(mu0s,getMPN(x=theDF3$x, n=theDF3$n, v=theDF3$v)$Results$MPNCU.ml)
      }
      C1<-getMPN(x=theDF3$x, n=theDF3$n, v=theDF3$v)$Results$MPNCU.ml
      MPN<-c(MPN, C1)
      lnDiff<-c(lnDiff,log(C1/mu0s[i]))
      logDiff<-c(logDiff,log10(C1/mu0s[i]))
      rep<-c(rep, theDF3$rep[j])
      times<-c(times,theDF3$t[j])
      MPNs<-c(MPNs, rep(C1,length(theDF3$t)))
      lnDiffs<-c(lnDiffs,rep(log(C1/mu0s[i]),length(theDF3$t)))
      logDiffs<-c(logDiffs,rep(log10(C1/mu0s[i]),length(theDF3$t)))
      Std.err<-c(Std.err,rep(getMPN(x=theDF3$x, n=theDF3$n, v=theDF3$v)$Results$Std.err,length(theDF3$t)))
    }

    # getK calculation for each replicate
    k_init = -coef(lm(as.vector(lnDiff) ~ as.vector(times)))[[2]]
    lnLs<-buildlnL(theDF2)
    repSEP = mle2(lnLs, start = list(k = k_init, b = 0, mu0 = mu0s[i]), method = "BFGS", optimizer = "nlminb", skip.hessian = F)
    lnDiff_grouped<-c(lnDiff_grouped,lnDiff)
    times_grouped<-c(times_grouped,times)
    LP1<-c(LP1,confint(repSEP, parm = c("k"), method = "quad")[1])
    LP2<-c(LP2,confint(repSEP, parm = c("k"), method = "quad")[2])
    k_val<-c(k_val,coef(repSEP)[1])
    Intercept<-c(Intercept,coef(repSEP)[2])
    results_names<-c(results_names,paste("Rep", i, sep=""))
  }
  # Complete the raw results with MPNs, lnDiffs, and logDiffs vector
  theDF$MPN<-MPNs
  theDF$lnDiff<-lnDiffs
  theDF$logDiff<-logDiffs
  theDF$Std.err<-Std.err
  # getK calculation for grouped replicates
  lnLs<-buildlnL(theDF)
  k_init = -coef(lm(as.vector(lnDiff_grouped) ~ as.vector(times_grouped)))[[2]]
  repGROUP = mle2(lnLs, start = list(k = k_init, b = 0, mu0 = mean(mu0s)), method = "BFGS", optimizer = "nlminb", skip.hessian = F)
  LP1<-c(LP1,confint(repGROUP, parm = c("k"), method = "quad")[1])
  LP2<-c(LP2,confint(repGROUP, parm = c("k"), method = "quad")[2])
  k_val<-c(k_val,coef(repGROUP)[1])
  Intercept<-c(Intercept,coef(repGROUP)[2])
  results_names<-c(results_names,"Grouped")

  # Function output
  Results<-data.frame(
    "LP 2.5%"=LP1,
    "Decay rate (k)"=k_val,
    "LP 97.5%"=LP2,
    "Intercept"=Intercept,
    row.names=results_names
  )
  output<-list("raw.data"=theDF,"Results"=Results)
  class(output)<-"getK"
  return(output)

}

