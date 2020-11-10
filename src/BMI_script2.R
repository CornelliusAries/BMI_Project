D <- read.table("data/bmi2_data.csv", header=TRUE, sep=";", as.is=TRUE)

D$logbmi <- log(D$bmi)
#=== Descriptive analysis ===#

#=== Histograms ===#
par(mfrow=c(1,3))
hist(D$logbmi, xlab="logBMI",main="logBMI histogram", prob=TRUE)
hist(D$age, xlab="Age (years)",main="Age histogram", prob=TRUE)
hist(D$fastfood, xlab="Fast food consumption (days per year)",main="Fast food histogram", prob=TRUE)
#=== Scatter plots ===#
par(mfrow=c(1,2))
plot(D$age, D$logbmi, xlab="Age (year)", ylab="logBMI",main="logBMI(Age)")
plot(D$fastfood, D$logbmi, xlab="Fast food consumption (days per year)", ylab="logBMI",main="logBMI(Fastfood)")
#=== Box plots ===#
par(mfrow=c(1,3))
boxplot(D$logbmi, ylab="logBMI")
boxplot(D$age, ylab="Age (years)")
boxplot(D$fastfood, ylab="Fast food consumption (days per year)")
#=== Summary statistics ===#
summary(D)
var(D$logbmi, na.rm=TRUE)
var(D$logbmi, na.rm=TRUE)^(1/2)
var(D$age, na.rm=TRUE)
var(D$age, na.rm=TRUE)^(1/2)
var(D$fastfood, na.rm=TRUE)
var(D$fastfood, na.rm=TRUE)^(1/2)

#B & C ====#
D_model <- subset(D, id<= 840)
D_test <- subset(D, id>= 841)

fit <- lm(logbmi ~ age + fastfood, data = D_model)
summary(fit)

#D ====#
par(mfrow=c(2,2))
# Observations against fitted values
plot(fit$fitted.values, D_model$logbmi, xlab = "Fitted values",     
     ylab = "log(BMI)")
# Residuals against fitted values
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", 
     ylab = "Residuals")
# Residuals against each of the explanatory variables
plot(D_model$age, fit$residuals, 
     xlab = "Age (years)", ylab = "Residuals")
plot(D_model$fastfood, fit$residuals, 
     xlab = "Fast food consumption (days per year)", ylab = "Residuals")



# Normal QQ-plot of the residuals
par(mfrow=c(1,1))
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)

#E ====#
t_alpha<-1.96
beta1_minus<-0.0023744-t_alpha*0.0003890
beta1_plus<-0.0023744+t_alpha*0.0003890
beta1_minus
beta1_plus
confint(fit, level = 0.95)

#F ====#
beta1<-0.0023744
beta1_zero<-0.001
sigma_beta1<-0.0003890
t_obs<-(beta1-beta1_zero)/sigma_beta1
t_obs
pvalue <- 2*(1-pt(abs(t_obs),df=837))
pvalue

#G ====#
summary(fit)
fit2 <- lm(logbmi ~ age, data = D_model)
summary(fit2)
fit3 <- lm(logbmi ~ fastfood, data = D_model)
summary(fit3)

#H ====#
pred <- predict(fit, newdata = D_test, interval = "prediction", level = 0.95)
pred
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

