#' plotting function of getMPN
#'
#' drawns a plot from getMPN objects
#' @param ... One of more getMPN objects
#' @return plot
#' @export

plotMPN <- function (...) {
  library(ggplot2)
  input<-list(...)
  plot_df<-data.frame(matrix(nrow=0,ncol=0))
  for (i in 1:length(input)) {
    plot_df<-rbind(plot_df,input[i][[1]]$Results)
  }
  plot_df<-cbind("Obs"=as.character(rep(1:length(input))),plot_df)
  print(plot_df)
  output<-ggplot(plot_df)+
    theme_bw()+
    geom_pointrange(aes(x=Obs,y=MPNCU.ml, ymin=Lower.95CI, ymax=Upper.95CI))+
    scale_y_log10()+
  return(output)
}


