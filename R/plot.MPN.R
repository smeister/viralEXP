#' plot.MPN function
#'
#' Plots the MPN calculated with the getMPN function
#' @param ... MPN objects
#' @param plot if =TRUE returns a dataframe, =FALSE returns a plot
#' @return a dataframe or a plot
#' @export

plot.MPN <- function (..., plot=TRUE) {
  input<-list(...)
  plot_df<-data.frame(matrix(nrow=0,ncol=0))
  for (i in 1:length(input)) {
    plot_df<-rbind(plot_df,input[i][[1]]$Results)
  }
  plot_df<-cbind("Sample"=as.character(rep(1:length(input))),plot_df)
  output<-ggplot(plot_df)+
    theme_bw()+
    geom_pointrange(aes(x=Sample,y=MPNCU.ml, ymin=Lower.95CI, ymax=Upper.95CI),size=1)+
    scale_y_log10(breaks=c(1 %o% 10^(1:15)))
  if(plot==FALSE) {
    return(plot_df)
  } else {
    return(output)
  }
}
