#rm(list=ls());Sys.setenv(LANG="en")

#library(viralEXP)
#test1<-getMPN(x=c(5,5,5,3,2,0), n=c(5,5,5,5,5,5), v=0.01)
#test2<-getMPN(x=c(5,5,5,0,0,0), n=c(5,5,5,5,5,5), v=0.01)
#test3<-getMPN(x=c(5,3,0,0,0,0), n=c(5,5,5,5,5,5), v=0.01)


#rbind(test1$raw.data,test2$raw.data)

#testA<-getK(test1,test2,test3,timeVECT = c(0,2,4))
#testB<-getK(test1,test2,test3,timeVECT = c(0,6,10))

#getK(testA,testB,typeOF = TRUE)

test4<-read.csv2("D:/Dropbox/EPFL/Lab book/R/projects/viralEXP/Wa_getK.csv")
test5<-read.csv2("D:/Dropbox/EPFL/Lab book/R/projects/viralEXP/Wa_getK.csv")
test5$rep<-2

trial<-rbind(test4,test5)
#str(trial)

getK(trial)
