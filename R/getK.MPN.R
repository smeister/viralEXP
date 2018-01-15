#' getK.MPN function
#'
#' This is the getK S3 function for class "MPN" objects.
#' @param ... MPN objects
#' @param timeVECT input the time/dose vector for each MPN objects
#' @return a list containing two data frames (raw.data and Results)
#' @export

getK.MPN <- function (..., timeVECT) {
  # Merge the different getMPN inputs
  input<-list(...)
  # Check if the timeVECT is present
  if (missing(timeVECT) | any(is.na(timeVECT)) | any(!is.numeric(timeVECT))) {
    stop("Error: the time/dose vector is missing or invalid")
  }
  if (length(input) == 1) {
    input<-input[[1]]
    for (k in 1:length(input)) {
      input[[k]]$raw.data$t<-rep(timeVECT[k],length(input[[k]]$raw.data$x))
    }
    theDF<-data.frame()
    for (l in 1:length(input)) { # Creating the dataframe with all data
      theDF<-rbind(theDF,input[[l]]$raw.data)
    }
  } else {
    if (length(input)>1) {
      for (k in 1:length(input)) {
        input[[k]]$raw.data$t<-rep(timeVECT[k],length(input[[k]]$raw.data$x))
      }
      theDF<-data.frame()
      for (l in 1:length(input)) { # Creating the dataframe with all data
        theDF<-rbind(theDF,input[[l]]$raw.data)
      }
    }
  }
  # Convert all the vectors as numeric type
  theDF$t<-as.numeric(as.character(theDF$t))
  theDF$v<-as.numeric(as.character(theDF$v))
  theDF$n<-as.numeric(as.character(theDF$n))
  theDF$x<-as.numeric(as.character(theDF$x))
  # Create vectors for $Results output
  LP1<-c()
  LP2<-c()
  k_val<-c()
  Intercept<-c()
  # Create vectors for $raw.data output
  MPNs<-c()
  lnDiffs<-c()
  logDiffs<-c()
  Std.err<-c()
  # Create vectors for getK calculation
  MPN<-c()
  lnDiff<-c()
  logDiff<-c()
  times<-c()
  # Loop that extract the values for getK calculation
  for (i in 1:length(unique(theDF$t))) {
    # Subset the dataframe into timepoints t
    theDF2<-subset(theDF, t == unique(theDF$t)[i])
    # Fill vectors for getK calculation
    Ci<-input[[i]]$Results$MPNCU.ml
    MPN<-c(MPN, Ci)
    lnDiff<-c(lnDiff,log(Ci/MPN[1]))
    logDiff<-c(logDiff,log10(Ci/MPN[1]))
    times<-c(times,theDF2$t[i])
    # Fill vectors for $raw.data output
    MPNs<-c(MPNs, rep(Ci,length(theDF2$t)))
    lnDiffs<-c(lnDiffs,rep(log(Ci/MPN[1]),length(theDF2$t)))
    logDiffs<-c(logDiffs,rep(log10(Ci/MPN[1]),length(theDF2$t)))
    Std.err<-c(Std.err,rep(getMPN(x=theDF2$x, n=theDF2$n, v=theDF2$v)$Results$Std.err,length(theDF2$t)))
  }
  # Complete the dataframe with the $raw.data vectors
  theDF$MPN<-MPNs
  theDF$lnDiff<-lnDiffs
  theDF$logDiff<-logDiffs
  theDF$Std.err<-Std.err
  theDF$rep<-rep(1,length(theDF$MPN))
  # getK calculation
  lnLs<-buildlnL(theDF)
  k_init = -coef(lm(as.vector(lnDiff) ~ as.vector(times)))[[2]]
  MLE = mle2(lnLs, start = list(k = k_init, b = 0, mu0 = MPN[1]), method = "BFGS", optimizer = "nlminb", skip.hessian = FALSE)
  LP1<-c(LP1,confint(MLE, parm = c("k"), method = "quad")[1])
  LP2<-c(LP2,confint(MLE, parm = c("k"), method = "quad")[2])
  k_val<-c(k_val,coef(MLE)[1])
  Intercept<-c(Intercept,coef(MLE)[2])
  # Build output
  Results<-data.frame(
    "LP 2.5%"=LP1,
    "Decay rate (k)"=k_val,
    "LP 97.5%"=LP2,
    "Intercept"=Intercept,
    row.names = NULL
  )
  output<-list("raw.data"=theDF,"Results"=Results)
  class(output)<-"K"
  return(output)
}

