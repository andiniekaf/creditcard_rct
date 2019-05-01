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

## EXPERIMENT 1 
# analyzing the comparison of take-up rate between Benefit Upgrade group and Platinum Upgrade group
exp_1_group1 <- exp_1
exp_1_group1$Platinum_upgrade_merit <- NULL 
exp_1_group1 <- exp_1_group1[exp_1_group1$Gold_benefits == 1 | exp_1_group1$Platinum_upgrade == 1,]
exp_1_group1$Treatment <- ifelse(exp_1_group1$Gold_benefits == 1,1,2)
exp_1_group1 <- exp_1_group1[!is.na(exp_1_group1$decision),]
exp_1_group1$Treatment <- as.factor(exp_1_group1$Treatment)
exp_1_group1$Gold_benefits <- NULL
exp_1_group1$Platinum_upgrade <- NULL

# analyzing the comparison of take-up rate between Platinum Upgrade group and Platinum Upgrade Merit group
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

summary(lmp(decision~Platinum_upgrade_d,data=exp_1,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(decision~Platinum_upgrade_d+jakarta+muslim+factor(strata)+female+credit_limit+caller_1+caller_2+caller_3+caller_4+caller_5+caller_6+caller_7+caller_8,data=exp_1,perm="Prob",Iter=10000,seqs=TRUE))

## EXPERIMENT 2
# analyzing the comparison of take-up rate between those in positional externality control group (received only the product description) and those assigned to the positional externality treatment group (informed that the bank had recently relaxed the eligibility criteria for the platinum card)
exp_2 <- exp_2[exp_2$exclude == 0,]
aggregate(Y ~ T, FUN=length, data=exp_2)
aggregate(Y ~ T, FUN=mean, data=exp_2)
exp_2$T <- as.factor(exp_2$T)

oneway_test(exp_2$Y ~ exp_2$T, alternative="two.sided", distribution="exact")
summary(lmp(Y~T,data=exp_2,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(Y~T+age+credit_limit+female+muslim+jakarta,data=exp_2_group,perm="Prob",Iter=10000,seqs=TRUE))

## EXPERIMENT 3
# calculating the comparison of take-up rate between customers who were randomly assigned to a phone version of a self-affirmation and customers in placebo treatment
exercise or a placebo treatment.
exp_3 <- exp_3[!is.na(exp_3$takeup),]
exp_3_upgrade_b <- exp_3[exp_3$upgr_ntrl == 1 | exp_3$upgr_pstv == 1,]
exp_3_upgrade_p <- exp_3[exp_3$plat_ntrl == 1 | exp_3$plat_pstv == 1,]
exp_3_upgrade_p$plat_pstv <- as.factor(exp_3_upgrade_p$plat_pstv)
exp_3_upgrade_b$upgr_pstv <- as.factor(exp_3_upgrade_b$upgr_pstv)
aggregate(takeup ~ plat_pstv, FUN=length, data=exp_3_upgrade_p)
aggregate(takeup ~ plat_pstv, FUN=mean, data=exp_3_upgrade_p)
aggregate(takeup ~ upgr_pstv, FUN=length, data=exp_3_upgrade_b)
aggregate(takeup ~ upgr_pstv, FUN=mean, data=exp_3_upgrade_b)
oneway_test(exp_3_upgrade_p$takeup ~ exp_3_upgrade_p$plat_pstv, alternative="two.sided", distribution="exact")
oneway_test(exp_3_upgrade_b$takeup ~ exp_3_upgrade_b$upgr_pstv, alternative="two.sided", distribution="exact")

summary(lmp(takeup~plat_pstv,data=exp_3_upgrade_p,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(takeup~plat_pstv+income+female+muslim+jakarta+credit_limit,data=exp_3_upgrade_p,perm="Prob",Iter=10000,seqs=TRUE))

summary(lmp(takeup~upgr_pstv,data=exp_3_upgrade_b,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(takeup~upgr_pstv+income+female+muslim+jakarta+credit_limit,data=exp_3_upgrade_b,perm="Prob",Iter=10000,seqs=TRUE))

## MTURK EXPERIMENT
# analyzing the impact of e self-esteem treatment on subjects’ self-esteem, as measured using the Rosenberg (1965) scale, and the effects of the self-esteem treatment on demand for the luxury brand gift certificate
the luxury brand gift certificate
(s.e.=0.7), or 0.17 standard deviations, higher on the self-esteem measure than participants in the
control group (statistically significant at 10%).
In Table 4, columns (2) to (6), we report the effects of the self-esteem treatment on demand for
the luxury brand gift certificate
summary(lmp(rosenberg~self,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(rosenberg~self+age+race_1+race_2+race_3+race_4+education+income+married,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))

summary(lmp(demand_armani_offer1~self,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(demand_armani_offer1~self+age+race_1+race_2+race_3+race_4+education+income+married,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))

summary(lmp(demand_armani_offer2~self,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(demand_armani_offer2~self+age+race_1+race_2+race_3+race_4+education+income+married,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))

summary(lmp(demand_armani_offer3~self,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(demand_armani_offer3~self+age+race_1+race_2+race_3+race_4+education+income+married,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))

summary(lmp(demand_armani_offer4~self,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(demand_armani_offer4~self+age+race_1+race_2+race_3+race_4+education+income+married,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))

summary(lmp(demand_armani_offer5~self,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
summary(lmp(demand_armani_offer5~self+age+race_1+race_2+race_3+race_4+education+income+married,data=mturk,perm="Prob",Iter=10000,seqs=TRUE))
