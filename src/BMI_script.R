
#To change on different PC's
#setwd("../R_Projects/BMI_Project")
#===============================

D <- read.table("data/bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)

D$bmi <- D$weight/(D$height/100)^2

hist(D$bmi, xlab="BMI",main="BMI histogram", prob=TRUE)

Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)

hist(Dfemale$bmi, xlab="BMI (female)",main="Female BMI histogram", prob=TRUE)
hist(Dmale$bmi, xlab="BMI (male)",main="Male BMI histogram", prob=TRUE)

boxplot(Dfemale$bmi, Dmale$bmi, names=c("Female", "Male"), 
        xlab="Gender", ylab="BMI", main="BMI survey boxplots by gender")

sum(!is.na(D$bmi))
## Sample mean (both genders combined)
mean(D$bmi, na.rm=TRUE)
## Sample variance (both genders combined)
var(D$bmi, na.rm=TRUE)

summary(D)
summary(Dfemale)
summary(Dmale)
str(D)

mean(D$bmi, na.rm=TRUE)
var(D$bmi, na.rm=TRUE)^(1/2)

mean(Dfemale$bmi, na.rm=TRUE)
var(Dfemale$bmi, na.rm=TRUE)^(1/2)

mean(Dmale$bmi, na.rm=TRUE)
var(Dmale$bmi, na.rm=TRUE)^(1/2)

## New variable 'logbmi' with log-transformed BMI
D$logbmi <- log(D$bmi)
Dfemale$logbmi <- log(Dfemale$bmi)
Dmale$logbmi <- log(Dmale$bmi)
## qq-plot of log-transformed BMI
qqnorm(D$logbmi,main="logBMI Q-Q plot")
qqline(D$logbmi)
qqnorm(D$bmi,main="BMI Q-Q plot")
qqline(D$bmi)

mean(D$logbmi, na.rm=TRUE)
var(D$logbmi, na.rm=TRUE)
var(D$logbmi, na.rm=TRUE)^(1/2)


mean(Dfemale$logbmi, na.rm=TRUE)
var(Dfemale$logbmi, na.rm=TRUE)
var(Dfemale$logbmi, na.rm=TRUE)^(1/2)

mean(Dmale$logbmi, na.rm=TRUE)
var(Dmale$logbmi, na.rm=TRUE)
var(Dmale$logbmi, na.rm=TRUE)^(1/2)

summary(D$logbmi)
summary(Dfemale$logbmi)
summary(Dmale$logbmi)

#F)
plot(ecdf(D$logbmi), verticals=TRUE, main="Empirical cdf of logBMI", xlab="logBMI")
xseq <- seq(0.9*min(D$logbmi), 1.1*max(D$logbmi),length.out=100)
lines(xseq, pnorm(xseq, mean(D$logbmi), sd(D$logbmi)))

#G)
uQuantCoeff <- qt(0.975, length(D$logbmi)-1)
lowConInter <- mean(D$logbmi)-uQuantCoeff*sd(D$logbmi)/sqrt(length(D$logbmi))
upConInter <- mean(D$logbmi)+uQuantCoeff*sd(D$logbmi)/sqrt(length(D$logbmi))
uQuantCoeff
lowConInter
upConInter
t.test(D$logbmi, conf.level=0.95)
exp(mean(D$logbmi))
exp(c(lowConInter, upConInter))

#H)
tobs <- (mean(D$logbmi)-log(25))/(sd(D$logbmi)/length(D$logbmi)^(1/2))
tobs
t.test(D$logbmi, mu=log(25))
pvalue <- 2*(1-pt(abs(tobs),df=length(D$logbmi)-1))
pvalue

#I)
#FEMALE
par(mfrow=c(1,1))
plot(ecdf(Dfemale$logbmi), verticals=TRUE, main="Empirical cdf of female logBMI ", xlab="logBMI")
xseq <- seq(0.9*min(Dfemale$logbmi), 1.1*max(Dfemale$logbmi),length.out=100)
lines(xseq, pnorm(xseq, mean(Dfemale$logbmi), sd(Dfemale$logbmi)))

qqnorm(Dfemale$logbmi,main="Female logBMI Q-Q plot")
qqline(Dfemale$logbmi)

#MALE
par(mfrow=c(1,1))
plot(ecdf(Dmale$logbmi), verticals=TRUE, main="Empirical cdf of male logBMI ", xlab="logBMI")
xseq <- seq(0.9*min(Dmale$logbmi), 1.1*max(Dmale$logbmi),length.out=100)
lines(xseq, pnorm(xseq, mean(Dmale$logbmi), sd(Dmale$logbmi)))

qqnorm(Dmale$logbmi,main="Male logBMI Q-Q plot")
qqline(Dmale$logbmi)

qqnorm(Dmale$bmi,main="Male BMI Q-Q plot")
qqline(Dmale$bmi)

#J)
#FEMALE
uQuantCoeff <- qt(0.975, length(Dfemale$logbmi)-1)
lowConInter <- mean(Dfemale$logbmi)-uQuantCoeff*sd(Dfemale$logbmi)/sqrt(length(Dfemale$logbmi))
upConInter <- mean(Dfemale$logbmi)+uQuantCoeff*sd(Dfemale$logbmi)/sqrt(length(Dfemale$logbmi))
uQuantCoeff
lowConInter
upConInter
t.test(Dfemale$logbmi, conf.level=0.95)
exp(mean(Dfemale$logbmi))
exp(c(lowConInter, upConInter))

#MALE
uQuantCoeff <- qt(0.975, length(Dmale$logbmi)-1)
lowConInter <- mean(Dmale$logbmi)-uQuantCoeff*sd(Dmale$logbmi)/sqrt(length(Dmale$logbmi))
upConInter <- mean(Dmale$logbmi)+uQuantCoeff*sd(Dmale$logbmi)/sqrt(length(Dmale$logbmi))
uQuantCoeff
lowConInter
upConInter
t.test(Dmale$logbmi, conf.level=0.95)
exp(mean(Dmale$logbmi))
exp(c(lowConInter, upConInter))

t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])

#K)
delta_zero <- 0
ms <- c(mean(Dfemale$logbmi), mean(Dmale$logbmi))
vs <- c(var(Dfemale$logbmi), var(Dmale$logbmi))
ns <- c(length(Dfemale$logbmi), length(Dmale$logbmi))
## The observed statistic
t_obs <- (ms[2]-ms[1]-delta_zero)/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
t_obs
## The degrees of freedom
nu <- ((vs[1]/ns[1]+vs[2]/ns[2])^2)/
  ((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))
nu
pvalue <- 2*(1-pt(abs(t_obs),df=134))
pvalue
t.test(Dfemale$logbmi, Dmale$logbmi)

#L)

#M)
Sxy <- cov(D$bmi, D$weight)
r <- Sxy/(sd(D$bmi)*sd(D$weight))
r
Sxy
sd(D$bmi)
sd(D$weight)

cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")

par(mfrow=c(1,3),)

plot(D$weight, D$bmi, xlab="Weight (kg)", ylab="BMI",main="BMI(Weight)")
plot(D$fastfood, D$bmi, xlab="Fast food consumption (DpY)", ylab="BMI", main="BMI(Fastfood)")
plot(D$fastfood, D$weight, xlab="Fast food consumption (DpY)", ylab="Weight (kg)", main="Weight(Fastfood)")
