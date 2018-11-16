#pull data
MMP <- read.csv(file = "C:/Users/Nancy/Downloads/mig161.csv", stringsAsFactors=F)

summary(MMP$surveyyr)

#find descriptive stats for males
summary(MMP$surveyyr)
summary(MMP$surveyyr[MMP$sex==1])
sd(MMP$surveyyr[MMP$sex==1])
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
mode(MMP$surveyyr[MMP$sex==1])

#find des. stats for females
summary(MMP$surveyyr[MMP$sex==2])
sd(MMP$surveyyr[MMP$sex==2])
mode(MMP$surveyyr[MMP$sex==2])

#group years by decade (80, 90, 00, 10)
table(MMP$surveyyr)
MMP$eighties<-ifelse(MMP$surveyyr == 1982|MMP$surveyyr == 1983|MMP$surveyyr == 1987|MMP$surveyyr == 1988|MMP$surveyyr == 1989, 1, 0)
table(MMP$eighties)
MMP$nineties<-ifelse(MMP$surveyyr == 1990|MMP$surveyyr == 1991|MMP$surveyyr == 1992|MMP$surveyyr == 1993|MMP$surveyyr == 1994|MMP$surveyyr == 1995|MMP$surveyyr == 1996|MMP$surveyyr == 1997|MMP$surveyyr == 1998|MMP$surveyyr == 1999, 1, 0)
table(MMP$nineties)
MMP$mil<-ifelse(MMP$surveyyr == 2000|MMP$surveyyr == 2001|MMP$surveyyr == 2002|MMP$surveyyr == 2003|MMP$surveyyr == 2004|MMP$surveyyr == 2005|MMP$surveyyr == 2006|MMP$surveyyr == 2007|MMP$surveyyr == 2008|MMP$surveyyr == 2009, 1, 0)
table(MMP$mil)
MMP$milten<-ifelse(MMP$surveyyr == 2010|MMP$surveyyr == 2011|MMP$surveyyr == 2012|MMP$surveyyr == 2013|MMP$surveyyr == 2014|MMP$surveyyr == 2015|MMP$surveyyr == 2016, 1, 0)
table(MMP$milten)

#combine groups into NewYrs
MMP$NewYrs<- ifelse(MMP$eighties==1, 1, ifelse(MMP$nineties==1, 2, ifelse(MMP$mil==1, 3, ifelse(MMP$milten==1, 4, 0))))
table(MMP$NewYrs)

install.packages("gmodels")
library("gmodels")

#frequency for NewYrs & sex
table(MMP$sex, MMP$NewYrs)
ftable(MMP$sex, MMP$NewYrs)

#crosstabs for newyrs and sex
CrossTable(MMP$sex, MMP$NewYrs, prop.chisq = F)
CrossTable(MMP$sex, MMP$NewYrs, expected= T)

#chisq for newYrs
chisq.test(MMP$sex, MMP$NewYrs)

#######look at usdoc type###############
summary(MMP$usdoc1)

#take out usdoc type unknown
MMP$usdoc1<-ifelse(MMP$usdoc1==9999, NA, MMP$usdoc1)
summary(MMP$usdoc1)

#look at mode by sex in usdoc
mode(MMP$usdoc1[MMP$sex==1])
mode(MMP$usdoc1[MMP$sex==2])

#group (2,3,4)= 1 work, (1,5,6,7) = 2 legal, 8= stays same undocu
table(MMP$usdoc1)
MMP$Work<-ifelse(MMP$usdoc1 == 2|MMP$usdoc1 == 3|MMP$usdoc1 == 4, 1, 0)
table(MMP$Work)
MMP$Legal<-ifelse(MMP$usdoc1 == 1|MMP$usdoc1 == 6|MMP$usdoc1 == 5|MMP$usdoc1 == 7, 1, 0)
table(MMP$Legal)
MMP$UnDoc <- ifelse (MMP$usdoc1==8, 1, 0)
table(MMP$UnDoc)

#combine groups into NewDoc
MMP$NewDoc<- ifelse(MMP$Work==1, 1, ifelse(MMP$Legal==1, 2, ifelse(MMP$UnDoc==1,3,0)))
table(MMP$NewDoc)

#look at frequency tables for NewDoc
table(MMP$sex, MMP$NewDoc)
ftable(MMP$sex, MMP$NewDoc)

#crosstable for NewDoc & Sex
CrossTable(MMP$sex, MMP$NewDoc, prop.chisq = F)
CrossTable(MMP$sex, MMP$NewDoc, expected = T)

#chisqfor newdoc
chisq.test(MMP$sex, MMP$NewDoc)