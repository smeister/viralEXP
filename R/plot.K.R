#' plot.K function
#'
#' Plots the K calculated with the getK function
#' @param Object K object
#' @param type y axis transformation
#' @return a plot
#' @export

plot.K <- function (Object, type="ln") {
  input<-Object
  plot_df<-data.frame(matrix(nrow=0,ncol=0))
  for (i in 1:length(unique(input$raw.data$rep))) {
    theDF<-subset(input$raw.data, rep == unique(input$raw.data$rep)[i])
    theDF<-droplevels(theDF)
    for (j in 1:length(unique(theDF$t))) {
      theDF2<-subset(theDF, t == unique(theDF$t)[j])
      theDF2<-droplevels(theDF2)
      plot_rep<-data.frame(
        "rep"=unique(theDF2$rep),
        "t"=unique(theDF2$t),
        "lnDiff"=unique(theDF2$lnDiff),
        "logDiff"=unique(theDF2$logDiff),
        "Std.err"=unique(theDF2$Std.err)
      )
      plot_rep$Decay.rate<-rep(input$Results$Decay.rate..k.[i],length(plot_rep[,1]))
      plot_rep$Intercept<-rep(input$Results$Intercept[i],length(plot_rep[,1]))
      plot_df<-rbind(plot_df,plot_rep)
    }
  }
  if (type == "ln") {
    # For ln
    output<-ggplot()+
      theme_bw()+
      geom_pointrange(data=plot_df, aes(x=t,y=lnDiff, ymin=lnDiff-(Std.err*1.96),ymax=lnDiff+(Std.err*1.96),colour=factor(rep)))+
      scale_colour_manual(values = c("black","blue","green","orange"),name="Rep")+
      geom_abline(data=plot_df,aes(slope = -Decay.rate,intercept=Intercept,colour=factor(rep)))+
      geom_abline(slope = -input$Results[nrow(input$Results),]$Decay.rate..k.,intercept = input$Results[nrow(input$Results),]$Intercept, colour="red", size=1,linetype=2)+
      xlab("time/dose")+
      ylab("ln(Ct/C0)")
  }
  if (type == "log10") {
    # For log10
    output<-ggplot()+
      theme_bw()+
      geom_pointrange(data=plot_df, aes(x=t,y=logDiff, ymin=logDiff-(Std.err*1.96),ymax=logDiff+(Std.err*1.96),colour=factor(rep)))+
      scale_colour_manual(values = c("black","blue","green","orange"),name="Rep")+
      geom_abline(data=plot_df,aes(slope = -Decay.rate*log10(exp(1)),intercept=Intercept*log10(exp(1)),colour=factor(rep)))+
      geom_abline(slope = (-input$Results[nrow(input$Results),]$Decay.rate..k.)*log10(exp(1)),intercept = (input$Results[nrow(input$Results),]$Intercept)*log10(exp(1)), colour="red", size=1,linetype=2)+
      xlab("time/dose")+
      ylab("log10(Ct/C0)")
  }
  return(output)
}

