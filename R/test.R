#rm(list=ls());Sys.setenv(LANG="en")

#library(viralEXP)
#test1<-getMPN(x=c(5,5,5,3,2,0), n=c(5,5,5,5,5,5), v=0.01)
#test2<-getMPN(x=c(5,5,5,0,0,0), n=c(5,5,5,5,5,5), v=0.01)
#test3<-getMPN(x=c(5,3,0,0,0,0), n=c(5,5,5,5,5,5), v=0.01)

#trial1<-getK(test1,test2,test3,timeVECT=c(0,4,8))
#trial2<-getK(test1,test2,test3,timeVECT=c(0,2,3))
#trial3<-getK(test1,test2,test3,timeVECT=c(0,5,10))

#trial4<-getK(trial1,typeOF = TRUE)

#plotK(trial1)

#test1$Results$Std.err

#plot_df<-trial4$Results

#library(ggplot2)
#input<-trial4

#plot_df<-data.frame(matrix(nrow=0,ncol=0))
#for (i in 1:length(unique(input$raw.data$rep))) {
#  theDF<-subset(input$raw.data, rep == unique(input$raw.data$rep)[i])
#  plot_rep<-data.frame(
#    "rep"=unique(theDF$rep),
#    "t"=unique(theDF$t),
#    "lnDiff"=unique(theDF$lnDiff),
#    "logDiff"=unique(theDF$logDiff),
#    "Std.err"=unique(theDF$Std.err)
#  )
#  plot_rep$Decay.rate<-rep(input$Results$Decay.rate..k.[i],length(plot_rep[,1]))
#  plot_rep$Intercept<-rep(input$Results$Intercept[i],length(plot_rep[,1]))
#  plot_df<-rbind(plot_df,plot_rep)
#}
#print(plot_df)


# For ln
#ggplot()+
#  theme_bw()+
#  geom_pointrange(data=plot_df, aes(x=t,y=lnDiff, ymin=lnDiff-(Std.err*1.96),ymax=lnDiff+(Std.err*1.96),colour=factor(rep)))+
#  scale_colour_manual(values = c("black","blue","green","orange"),name="Rep")+
#  geom_abline(data=plot_df,aes(slope = -Decay.rate,intercept=Intercept,colour=factor(rep)))+
#  geom_abline(slope = -input$Results[nrow(input$Results),]$Decay.rate..k.,intercept = input$Results[nrow(input$Results),]$Intercept, colour="red", size=1,linetype=2)+
#  xlab("time/dose")+
#  ylab("ln(Ct/C0)")

# For log10
#ggplot()+
#  theme_bw()+
#  geom_pointrange(data=plot_df, aes(x=t,y=logDiff, ymin=logDiff-(Std.err*1.96),ymax=logDiff+(Std.err*1.96),colour=factor(rep)))+
#  scale_colour_manual(values = c("black","blue","green","orange"),name="Rep")+
#  geom_abline(data=plot_df,aes(slope = -Decay.rate*log10(exp(1)),intercept=Intercept*log10(exp(1)),colour=factor(rep)))+
#  geom_abline(slope = (-input$Results[length(input$Results),]$Decay.rate..k.)*log10(exp(1)),intercept = (input$Results[length(input$Results),]$Intercept)*log10(exp(1)), colour="red", size=1,linetype=2)


#ggplot(data,aes(x.plot,y.plot))+stat_summary(fun.data=mean_cl_normal) +
#  geom_smooth(method='lm')

#output<-ggplot(plot_df)+
#  theme_bw()+
#  geom_pointrange(aes(x=Obs,y=MPNCU.ml, ymin=Lower.95CI, ymax=Upper.95CI))+
#  coord_trans(y="log10")

#ggplot()+
#  theme_bw()+
#  geom_abline(data=plot_df,aes(slope=-plot_df$Decay.rate..k.,intercept=plot_df$Intercept))

# coef(Mast2)[2]

#plot(times,logDiffs)
#abline(a = coef(Mast2)[2] * log10(exp(1)),b = -coef(Mast2)[1] * log10(exp(1)), lty = 1, col = "red")

#library(ggplot2)
#input<-list(...)
#plot_df<-data.frame(matrix(nrow=0,ncol=0))
#for (i in 1:length(input)) {
#  plot_df<-rbind(plot_df,input[i][[1]]$Results)
#}
#plot_df<-cbind("Obs"=as.character(rep(1:length(input))),plot_df)
#print(plot_df)
#output<-ggplot(plot_df)+
#  theme_bw()+
#  geom_pointrange(aes(x=Obs,y=MPNCU.ml, ymin=Lower.95CI, ymax=Upper.95CI))+
#  coord_trans(y="log10")
