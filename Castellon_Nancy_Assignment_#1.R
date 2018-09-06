#creating vectors x,y,z
x<-c(5,10,15,20,25,30)
y<-c(-1,NA,75,3,5,8)
z<-c(5)

#multiplying x&y by z
x*z
y*z

#storing results into new objects
xz<-c(25,50,75,100,125,150)
yz<-c(-5,NA,375,15,25,40)

#print new vectors
print(xz)
print(yz)

#replace missing in y and print
y<-ifelse(test = is.na(y)==T, yes = 2.5, no=y)
print(y)

#read document
library(readr)
asmt<-read.csv(file="https://raw.githubusercontent.com/mattdemography/EDU_7043/master/Data/Assignment_1.csv")

#first 10 states
asmt$State[1:10] #this one

#mean murder rate
mean(asmt$Murder)

#median murder
median(asmt$Murder)

#new englad murder mean ct,me,ma,nh,ri,vt
NEData<-subset(asmt,State=="CT"|State=="ME"|State=="MA"|State=="NH"|State=="RI"|State=="VT")
mean(NEData$Murder)

#bonus
 asmt$Vcrime<-as.numeric(as.character(asmt$Vcrime));mean(asmt$Vcrime, na.rm = T)

