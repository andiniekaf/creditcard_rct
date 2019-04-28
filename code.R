## SETTING A WORKING DIRECTORY
setwd("*insert your working directory here*")

## LOADING THE NECESSARY PACKAGES
library(dplyr)
library(readstata13)
library(coin)

## DATA PREPARATION
exp_1 <- read.dta13("Experiment1.dta")
exp_2 <- read.dta13("Experiment2.dta")
exp_3 <- read.dta13("Experiment3.dta")

# create a data frame dedicated for calculating the comparison of take-up rate between Benefit Upgrade group and Platinum Upgrade group
exp_1_group1 <- exp_1
exp_1_group1$Platinum_upgrade_merit <- NULL 
exp_1_group1 <- exp_1_group1[exp_1_group1$Gold_benefits == 1 | exp_1_group1$Platinum_upgrade == 1,]
exp_1_group1$Treatment <- ifelse(exp_1_group1$Gold_benefits == 1,1,2)
exp_1_group1 <- exp_1_group1[!is.na(exp_1_group1$decision),]
exp_1_group1$Treatment <- as.factor(exp_1_group1$Treatment)
exp_1_group1$Gold_benefits <- NULL
exp_1_group1$Platinum_upgrade <- NULL

## EXPERIMENT 1 
# calculating the comparison of take-up rate between Platinum Upgrade group and Platinum Upgrade Merit group
exp_1_group2 <- exp_1
exp_1_group2$Gold_benefits <- NULL 
exp_1_group2 <- exp_1_group2[exp_1_group2$Platinum_upgrade_merit == 1 | exp_1_group2$Platinum_upgrade == 1,]
exp_1_group2$Treatment <- ifelse(exp_1_group2$Platinum_upgrade == 1,2,3)
exp_1_group2 <- exp_1_group2[!is.na(exp_1_group2$decision),]
exp_1_group2$Treatment <- as.factor(exp_1_group2$Treatment)
exp_1_group2$Platinum_upgrade <- NULL
exp_1_group2$Platinum_upgrade_merit <- NULL

aggregate(decision ~ Treatment, FUN=mean, data=exp_1_group1)
aggregate(decision ~ Treatment, FUN=mean, data=exp_1_group2)
oneway_test(exp_1_group1$decision ~ exp_1_group1$Treatment, alternative="two.sided", distribution="exact")
oneway_test(exp_1_group2$decision ~ exp_1_group2$Treatment, alternative="two.sided", distribution="exact")

summary(lmp(decision~Platinum_upgrade_d,data=exp_1,perm="Prob",maxIter=10000))
summary(lmp(decision~Platinum_upgrade_d+jakarta+muslim+factor(strata)+female+credit_limit+caller_1+caller_2+caller_3+caller_4+caller_5+caller_6+caller_7+caller_8,data=exp_1,perm="Prob",maxIter=10000))

## EXPERIMENT 2
# calculating the comparison of take-up rate between those in positional externality control group (received only the product description) and those assigned to the positional externality treatment group (informed that the bank had recently relaxed the eligibility criteria for the platinum card)
exp_2 <- exp_2[exp_2$exclude == 0,]
aggregate(Y ~ T, FUN=length, data=exp_2)
aggregate(Y ~ T, FUN=mean, data=exp_2)
exp_2$T <- as.factor(exp_2$T)

oneway_test(exp_2$Y ~ exp_2$T, alternative="two.sided", distribution="exact")
summary(lmp(Y~T,data=exp_2,perm="Prob",maxIter=10000))
summary(lmp(Y~T+,data=exp_2,perm="Prob",maxIter=10000))
