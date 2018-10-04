#pull data
library(readxl)
TxBorder <- read_excel("C:/Users/Nancy/Downloads/Border-Crossings.xls.xlsx", 
                                   col_types = c("text", "text", "text", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric"))
View(TxBorder)


#create subset w/3 variables
recentTxBB<-TxBorder[c(1:36), c(6, 7, 8)] 
View(recentTxBB)

#mean and median
summary(recentTxBB)

#create function mode
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

#mode for the 3 variables
mode(recentTxBB$`2015`)
mode(recentTxBB$`2016`)
mode(recentTxBB$`2017`)

# pull from psych pckg
library(psych)

#variance and range
describe(recentTxBB$`2015`)
describe(recentTxBB$`2016`)
describe(recentTxBB$`2017`)

#variance
var(recentTxBB$`2015`)
var(recentTxBB$`2016`)
var(recentTxBB$`2017`)

#is it normal?
#run histogram to see
hist(recentTxBB$`2015`)
hist(recentTxBB$`2016`)
hist(recentTxBB$`2017`)

#run qqplot
qqnorm(recentTxBB$`2015`); qqline(recentTxBB$`2015`)
qqnorm(recentTxBB$`2016`); qqline(recentTxBB$`2016`)
qqnorm(recentTxBB$`2017`); qqline(recentTxBB$`2017`)
