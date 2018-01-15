#' getK.K function
#'
#' This is the getK S3 function for class "K" objects
#' @param ... K objects
#' @return a list containing two data frames (raw.data and Results)
#' @export

getK.K <- function (...) {
  # Merge the different getK inputs
  input<-list(...)
  if (length(input)>1) {
    for (k in 1:length(input)) { # Creating a rep vector in $raw.data for each biological replicate
      input[[k]]$raw.data$rep<-rep(k,length(input[[k]]$raw.data$x))
    }
    theDF<-data.frame()
    for (l in 1:length(input)) { # Creating the dataframe with all data
      theDF<-rbind(theDF,input[[l]]$raw.data)
    }
  } else {
    stop("Error: length(input) should be >1")
  }
  # Convert all the vectors as numeric type
  theDF$t<-as.numeric(as.character(theDF$t))
  theDF$v<-as.numeric(as.character(theDF$v))
  theDF$n<-as.numeric(as.character(theDF$n))
  theDF$x<-as.numeric(as.character(theDF$x))
  theDF$rep<-as.numeric(as.character(theDF$rep))
  # Create vectors for $Results output
  results_names<-c()
  ResultsREP<-data.frame()
  # Create vectors for getK calculation
  mu0s<-c()
  lnDiff<-c()
  times<-c()
  mu0s<-c()
  # Loop that extracts the values for getK calculation
  for (i in 1:length(unique(theDF$rep))) {
    theDF2<-subset(theDF, rep == unique(theDF$rep)[i])
    theDF2<-droplevels(theDF2)
    lnDiff<-c(lnDiff,unique(theDF2$lnDiff))
    times<-c(times,unique(theDF2$t))
    mu0s<-c(mu0s,unique(theDF2$MPN[1]))
    results_names<-c(results_names,paste("Rep", i, sep=""))
    ResultsREP<-rbind(ResultsREP,input[[i]]$Results)
  }
  # getK calculation all replicates grouped
  lnLs<-buildlnL(theDF)
  k_init = -coef(lm(as.vector(lnDiff) ~ as.vector(times)))[[2]]
  MLE = mle2(lnLs, start = list(k = k_init, b = 0, mu0 = mean(mu0s)), method = "BFGS", optimizer = "nlminb", skip.hessian = FALSE)
  LP1<-confint(MLE, parm = c("k"), method = "quad")[1]
  LP2<-confint(MLE, parm = c("k"), method = "quad")[2]
  k_val<-coef(MLE)[1]
  Intercept<-coef(MLE)[2]
  results_names<-c(results_names, "Grouped")
  # Build output
  ResultsGROUP<-data.frame(
    "LP 2.5%"=LP1,
    "Decay rate (k)"=k_val,
    "LP 97.5%"=LP2,
    "Intercept"=Intercept,
    row.names = NULL
  )
  Results<-rbind(ResultsREP,ResultsGROUP)
  row.names(Results)<-results_names
  output<-list("raw.data"=theDF,"Results"=Results)
  class(output)<-"K"
  return(output)
}

