DFtest<-read.csv("D:/Dropbox/EPFL/Lab book/R/template.csv",sep=";", header = T)

getALL(DFtest)

test1<-getMPN(x=c(5,5,5,5,5,1,0,0), n=5, v=0.1)
test2<-getMPN(x=c(6,6,3,0,0,0,0), n=6, v=0.1)

plotMPN(test1,test2)

remove.packages("viralEXP")
library(devtools)
install_github("smeister/viralEXP")
