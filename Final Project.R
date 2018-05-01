library(haven)
mydata <- read_dta("b_indresp.dta")
View(mydata)
attach(mydata)
View(mydata)
#recode sex
mydata$female <- 0
# Then recode the old field into the new one for the specified rows
mydata$female[mydata$b_sex==2] <- 1
View(mydata)
View (mydata$female)
#recode amount saved
View(b_saved)
mydata$amountsaved <- mydata$b_saved
mydata$amountsaved[mydata$b_saved<0] <- NA
View(mydata$amountsaved)
mydata$logamountsaved<- log(mydata$amountsaved)
View(mydata$logamountsaved)
hist(mydata$logamountsaved, freq= FALSE, main="Distribution of log amount saved", xlab="Log amount saved", col="green")
#recode income
mydata$income<-mydata$b_fimngrs_dv
mydata$income[mydata$b_fimngrs_dv<0]<-NA
View(mydata$income)
hist(mydata$income)
mydata$logincome<-log(mydata$income)
mydata$logincome[mydata$logincome<0]<-NA
hist(mydata$logincome, freq=FALSE, main="distribution of log income", col="blue", xlab="Log income")
View(mydata$b_savreg)
mydata$regsave<-mydata$b_savreg
mydata$regsave[mydata$b_savreg<0] <- NA
mydata$regsave[mydata$b_savreg==1] <- 1
mydata$regsave[mydata$b_savreg==2] <- 0
mydata$regsave[mydata$b_savreg==97] <-NA
View (mydata$regsave)
#recode age
View(mydata$b_dvage)
mydata$age<-mydata$b_dvage
mydata$age2<-mydata$age*mydata$age
#recode race
mydata$race<-mydata$b_racel
mydata$race[mydata$b_racel<0]<-NA
mydata$race[mydata$b_racel==1]<-1
mydata$race[mydata$b_racel==2]<-1
mydata$race[mydata$b_racel==3]<-1
mydata$race[mydata$b_racel==4]<-1
mydata$race[mydata$b_racel==5]<-2
mydata$race[mydata$b_racel==6]<-2
mydata$race[mydata$b_racel==7]<-2
mydata$race[mydata$b_racel==8]<-2
mydata$race[mydata$b_racel==9]<-3
mydata$race[mydata$b_racel==10]<-3
mydata$race[mydata$b_racel==11]<-3
mydata$race[mydata$b_racel==12]<-4
mydata$race[mydata$b_racel==13]<-4
mydata$race[mydata$b_racel==14]<-4
mydata$race[mydata$b_racel==15]<-4
mydata$race[mydata$b_racel==16]<-4
mydata$race[mydata$b_racel==17]<-4
mydata$race[mydata$b_racel==97]<-4
View(mydata$race)
counts <- table(mydata$race)
barplot(counts, main="Race Distribution in the sample", 
        xlab="1= Whites 2=Mixed 3=South Asians 4=Other", ylab="number of people", col="purple")
##regression##

regA<-lm(mydata$logamountsaved ~ mydata$female + mydata$age + mydata$age2 + mydata$logincome + factor(mydata$race), data=mydata)
summary(regA)
regB<-glm(mydata$regsave ~ mydata$female +mydata$age+ mydata$age2 +mydata$logincome+ factor(mydata$race), data=mydata, family = binomial)
summary(regB)


