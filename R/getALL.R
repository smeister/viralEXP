#' getALL function
#'
#' Function that allows the simultaneous MPN calculation according to the template
#' @param theDF data.frame containing data according ot the template
#' @export

getALL<-function (theDF) {
  #############################
  ### Mandatory Parameters: ###
  #############################
  # virus = virus
  # dis = disinfectant
  # sample = sample
  # exp = experiment
  # rep = replicate
  # t = dose/tome/exposure. If not such parameters, write "NA"
  # v = volume vector (dilution)
  # n = nb of MPN replicates
  # x = positive CPE
  # name = name of the person
  # date = day.month,year
  #############################

  theDF<-as.data.frame(lapply(theDF, as.factor))
  theDF$x<-as.numeric(as.character(theDF$x))
  theDF$n<-as.numeric(as.character(theDF$n))
  theDF$v<-as.numeric(as.character(theDF$v))
  theDF$t<-as.numeric(as.character(theDF$t))

  virus<-c()
  dis<-c()
  sample<-c()
  exp<-c()
  rep<-c()
  t<-c()
  MPN<-c()
  Std_err<-c()
  LCI_95<-c()
  UCI_95<-c()
  name<-c()
  date<-c()

  for (i in 1:length(levels(theDF$virus))) { # Subset the virus
    theDF2<-subset(theDF, virus == levels(theDF$virus)[i])
    theDF2<- droplevels(theDF2)
    for (j in 1:length(levels(theDF2$dis))) { # subset the dis
      theDF3<-subset(theDF2, dis == levels(theDF2$dis)[j])
      theDF3<- droplevels(theDF3)
      for (k in 1:length(levels(theDF3$sample))) { # subset the sample
        theDF4<-subset(theDF3, sample == levels(theDF3$sample)[k])
        theDF4<- droplevels(theDF4)
        for (l in 1:length(levels(theDF4$exp))) { # subset the exp
          theDF5<-subset(theDF4, exp == levels(theDF4$exp)[l])
          theDF5<- droplevels(theDF5)
          for (m in 1:length(levels(theDF5$rep))) { # subset the rep
            theDF6<-subset(theDF5, rep == levels(theDF5$rep)[m])
            theDF6<- droplevels(theDF6)
            for (o in 1:length(unique(theDF6$t))) { # subset the timepoints (t)
              theDF7<-subset(theDF6, rep == unique(theDF6$rep)[o])
              theDF7<- droplevels(theDF7)
              # MPN vectors filling
              MPN<-c(MPN,getMPN(x=theDF7$x,n=theDF7$n,v=theDF7$v)$Results$MPNCU.ml)
              UCI_95<-c(UCI_95,getMPN(x=theDF7$x,n=theDF7$n,v=theDF7$v)$Results$Upper.95CI)
              LCI_95<-c(LCI_95,getMPN(x=theDF7$x,n=theDF7$n,v=theDF7$v)$Results$Lower.95CI)
              Std_err<-c(Std_err,getMPN(x=theDF7$x,n=theDF7$n,v=theDF7$v)$Results$Std.err)
              # Information vectors filling
              virus<-c(virus,levels(theDF7$virus))
              dis<-c(dis,levels(theDF7$dis))
              sample<-c(sample,levels(theDF7$sample))
              exp<-c(exp,levels(theDF7$exp))
              rep<-c(rep,levels(theDF7$rep))
              t<-c(t,unique(theDF7$t))
              name<-c(name,levels(theDF7$name))
              date<-c(date,levels(theDF7$date))
            }
          }
        }
      }
    }
  }

  # Fill the dataframe
  Final_DF<-data.frame(
    virus=virus,
    dis=dis,
    sample=sample,
    exp=exp,
    rep=rep,
    t=t,
    MPN=MPN,
    Std_err=Std_err,
    LCI_95=LCI_95,
    UCI_95=UCI_95,
    name=name,
    date=date
  )
  return(Final_DF)
}
