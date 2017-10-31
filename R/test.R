#rm(list=ls());Sys.setenv(LANG="en")

#library(viralEXP)
#test1<-getMPN(x=c(5,5,5,3,2,0), n=c(5,5,5,5,5,5), v=0.01)
#test2<-getMPN(x=c(5,5,5,0,0,0), n=c(5,5,5,5,5,5), v=0.01)
#test3<-getMPN(x=c(5,3,0,0,0,0), n=c(5,5,5,5,5,5), v=0.01)

#plotMPN(test1,test2,test3)



#quote(list(test1,test2,test3))

#test1
#rbind(test1$raw.data,test2$raw.data)

#testA<-getK(test1,test2,test3,timeVECT = c(0,2,4))
#testB<-getK(test1,test2,test3,timeVECT = c(0,6,10))

#getK(testA,testB,typeOF = TRUE)

#test4<-read.csv2("D:/Dropbox/EPFL/Lab book/R/projects/viralEXP/Wa_getK.csv")
#test5<-read.csv2("D:/Dropbox/EPFL/Lab book/R/projects/viralEXP/Wa_getK.csv")
#test5$rep<-2

#trial<-rbind(test4,test5)
#str(trial)

#getK(trial)

#theplot<-ggplot(Final_DF3, aes_string(Final_DF3$dose, Final_DF3$LN, ymin = Final_DF3$LN-(1.96*Final_DF3$SE), ymax = Final_DF3$LN+(1.96*Final_DF3$SE)))+
#  theme_bw()+
#  geom_linerange()+
#  geom_point()+
#  ylab(NULL)+
#  xlab(NULL)+
#  #ylim(-18,3)+
  #xlim(0,35)+
#  annotate("text", x = 0, y=3, label = levels(Final_DF3$virus), size = 3, fontface =2,hjust = 0)
