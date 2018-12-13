#pull data
MMP <- read.csv(file = "C:/Users/Nancy/Downloads/mig161.csv", stringsAsFactors=F)

#anova
summary(MMP$age)
boxplot(MMP$age)
MMP$age<-ifelse(MMP$age==9999, NA, MMP$age)
MMP$usdocl<-ifelse(MMP$usdocl==9999, NA, MMP$usdocl)
summary(MMP$usdocl)

MMP678 <- subset(MMP, !is.na(MMP$usdocl) & !is.na(MMP$age))

FML <- aov(MMP678$age ~ as.factor(MMP678$usdocl))
summary(FML)
boxplot(MMP678$age ~ as.factor(MMP678$usdocl), main = "Age By Migration Documentation", xlab = "Figure 2: 1 = Legal Residency, 2 = Bracero, 3 = H2A, 4 = Work Permit, 5 = Tourist Visa, 6 = Citizen, 7 = Silva Letter, 8 = Undocumented", ylab = "Age")
TukeyHSD(FML)

library("psych")
describe(MMP678$age[MMP678$usdocl==1])


#corr age by freq
summary(MMP$age)
boxplot(MMP$age)
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
