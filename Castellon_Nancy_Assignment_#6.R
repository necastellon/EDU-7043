#pull data
MMP <- read.csv(file = "C:/Users/Nancy/Downloads/mig161.csv", stringsAsFactors=F)

summary(MMP$age)
boxplot(MMP3$age)
MMP$age<-ifelse(MMP$age==9999, NA, MMP$age)

library("psych")
describe(MMP$age)

summary(MMP$ustrips)
MMP$ustrips<-ifelse(MMP$ustrips==9999, NA, MMP$ustrips)
describe(MMP$ustrips)
boxplot(MMP$ustrips)
hist(MMP$ustrips)

MMP2 <- subset(MMP, !is.na(MMP$age) & !is.na(MMP$ustrips))

cor.test(MMP2$ustrips, MMP2$age)
cor(MMP2$ustrips, MMP2$age, method = "spearman")

install.packages("hexbin")
library(hexbin)

bin <- hexbin(MMP2$ustrips, MMP2$age, xbins = 50)
plot(bin, main = "Age by Number of US Migrations", ylab = "Age", xlab= "Number of US Migrations")


#H2: age of participant dependends on first attempt of "illegal" crossing
summary(MMP$crsyr1)
boxplot(MMP$crsyr1)

MMP$crsyr1<-ifelse(MMP$crsyr1==9999, NA, MMP$crsyr1)
MMP$crsyr1<-ifelse(MMP$crsyr1==8888, NA, MMP$crsyr1)
describe(MMP$crsyr1)

MMP3 <- subset(MMP, !is.na(MMP$crsyr1) & !is.na(MMP$age))

cor.test(MMP3$crsyr1, MMP3$age)

cor(MMP3$crsyr1, MMP3$age, method = "spearman")

library("hexbin")
bin <- hexbin(MMP3$crsyr1, MMP3$age, xbins = 50)
plot(bin, main = "Relationship of Age and First Illegal Crossing Year", xlab = "Figure 4: Data from Mexicam Migration Project", ylab = "Age")
