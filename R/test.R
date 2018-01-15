# install.packages("xlsReadWrite")
# library(xls)
#
# install.packages("rJava")
# library(rJava)

# # library(viralEXP)
# DFtest<-read.csv("D:/Dropbox/EPFL/Lab book/R/template.csv",sep=";", header = T)
# mylist
#
# DFtest<-read.csv("D:/Dropbox/EPFL/Lab book/R/template.csv",sep=";", header = T)
# levels(DFtest$t)
#
# DFtest$t[1]
#
# ifelse(!is.na(DFtest$t[1]),"yé", "yo")

# mylist<-split(DFtest, DFtest[,c("virus","dis","rep","sample","exp")])
#
# mylist<-value
#
#
# value<-list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4))
#
# length(mylist)
#
# mylist
#
# # apply the getMPN function on all element of the list
# mylist2<-lapply(mylist,FUN=test)
#
#
# test<-function (x) {
#   getMPN(x=x$x,n=x$n,v=x$v)$Results
# }
#
# unsplit(mylist2, f=F)

# data.frame(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 3, nrow = 4))
#
#
# df1<-data.frame(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 3, nrow = 4))
#
# df2<-apply(df1[2:3], as.integer)
#
#
# df3<-cbind(df1[1:2], lapply(df1[2:3], as.integer))
#
# ?apply
# str(df3)
#
# (df1[2:3])
#
# as.integer(df1[2:3])
#
# matrix(0, ncol = 3, nrow = 4)
#
# x<-c(1,2,3,NA,4,5,6)
#
# if (any(is.na(x))) {
#   print("hehe")
# }


#
#
#
#
# methods(split)
#
# unsplit(mylist2, value=DFtest[,c("virus","dis","rep","sample","exp")])
#
# ?split
# #unsplit(mylist2,mylist2[,c("virus","dis","rep","sample","exp")])
#
# g <- airquality$Month
# l <- split(airquality, g)
# l <- lapply(l, transform, Oz.Z = scale(Ozone))
# aq2 <- unsplit(l, g)
# head(aq2)
# with(aq2, tapply(Oz.Z,  Month, sd, na.rm = TRUE))
#
#
#
#
# require(stats); require(graphics)
# n <- 10; nn <- 100
# g <- factor(round(n * runif(n * nn)))
# x <- rnorm(n * nn) + sqrt(as.numeric(g))
# xg <- split(x, g)
# boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
# sapply(xg, length)
# sapply(xg, mean)
#
# ### Calculate 'z-scores' by group (standardize to mean zero, variance one)
# z <- unsplit(lapply(split(x, g), scale), g)
#
#
#
#
#
