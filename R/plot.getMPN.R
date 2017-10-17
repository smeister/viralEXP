#' plotting function of getMPN
#'
#' drawns a plot from getMPN objects
#' @param object One of more getMPN objects
#' @return plot
#' @export

plot.getMPN <- function (...) {
  library(ggplot2)
  input<-list(...)
  plot_df<-data.frame(matrix(nrow=0,ncol=0))
  for (i in 1:length(input)) {
    plot_df<-rbind(plot_df,input[i][[1]]$Results)
  }
  plot_df<-cbind("Obs"=as.character(rep(1:length(input))),plot_df)
  output<-ggplot(plot_df)+
    geom_pointrange(aes(x=Obs,y=MPNCU.ml, ymin=Lower.95CI, ymax=Upper.95CI))
  return(output)
}

plot.test <- function (...) {
  library(ggplot2)
  output<-names(list(...))
  return(output)
}
ewew<-2
erer<-5


plot.test(ewew, erer)
