#pull data
library(readxl)
Apprehended <- read_excel("fy2016_table34d.xls", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric"))
View(Apprehended)

#create subset
UGH <-Apprehended[, c(1, 9:11)]

#descriptives
library(psych)
describe(UGH)
var(UGH$`2014`)
var(UGH$`2015`)

#visual check
boxplot(Apprehended$`2014`, Apprehended$`2016 4`)

#ttest #not significant
t.test(Apprehended$`2014`, Apprehended$`2016 4`)

#because I can't figure out how to do ANOVA on my dataset, ################
#im going to use the GSS to fulfill the assignment :/

library(readr)
GSSMain<-read_csv(file="https://raw.githubusercontent.com/mattdemography/EDU_7043/master/Data/GSS_2016_AA.csv")
View(GSSMain)

#change variables to avoid skewing data
#0-NA, 13-Refused, 98-DK, 99-NoAns
summary(GSSMain$INCOME)
GSSMain$INCOME<-ifelse(GSSMain$INCOME==0 |GSSMain$INCOME==13 |GSSMain$INCOME==98 | GSSMain$INCOME==99, NA, GSSMain$INCOME)
summary(GSSMain$INCOME)
library(psych)
describe(GSSMain$INCOME)
summary(AGE[RACE==2])
attach(GSSMain)

describe(GSSMain$RACE)

describe(INCOME[RACE==1])
describe(INCOME[RACE==2])
describe(INCOME[RACE==3])
#anova
ANtest <- aov(GSSMain$INCOME ~ as.factor(GSSMain$RACE))
summary(ANtest)
boxplot(GSSMain$INCOME ~ as.factor(GSSMain$RACE))
TukeyHSD(ANtest)
#difference between White and Blacks, and White and Other        