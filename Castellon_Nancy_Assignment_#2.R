#open data and override NA's
library(readxl)
TxBorderCrossing <- read_excel("C:/Users/Nancy/Downloads/Border-Crossings.xls.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric"))
View(TxBorderCrossing)

#list variables
names(TxBorderCrossing)

#structure of data
str(TxBorderCrossing)

#mean and median
summary(TxBorderCrossing$`2013`)
summary(TxBorderCrossing$`2014`)
summary(TxBorderCrossing$`2015`)
summary(TxBorderCrossing$`2016`)
summary(TxBorderCrossing$`2017`)

#create mode fnct & apply
mode<-function(x) {
  unique_val<-unique(x)
  counts<-vector()
  for(i in 1: length(unique_val)){
    counts[i]<- length(which(x==unique_val[i]))
  }
  position<-c(which(counts==max(counts)))
  if(length(unique_val)==length(x))
    mode_x<-'Mode does not exist'
  else
    mode_x<-unique_val[position]
  return(mode_x)
}

mode(TxBorderCrossing$`2013`)
mode(TxBorderCrossing$`2014`)
mode(TxBorderCrossing$`2015`)
mode(TxBorderCrossing$`2016`)
mode(TxBorderCrossing$`2017`)

#psych library for sd & range #includes mean and median
install.packages("psych")
library(psych)
describe(TxBorderCrossing$`2017`)

#variance
var((TxBorderCrossing$`2017`))

#histogram
hist(TxBorderCrossing$`2017`)

#plot
stem(TxBorderCrossing$`2017`, scale = 1, width = 100, atom = 2e+08)

