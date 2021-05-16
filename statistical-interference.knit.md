---
output:
  pdf_document: default
  html_document: default
---
# Exponential Distribution compare to Central limit Theorem
# The project consists of two parts:
# 1. A simulation exercise.
# 2. Basic inferential data analysis.
# 1: A simulation exercise
#  Overview
# In this project the exponential distribution is investigated in R and compare it with Central Limit Theorem. The mean of exponential distribution is 1/lambda and the standard deviation is also a function of 1/lambda. The exponential distribution is simulated in R with rexp(n,lambda), where lambda=0.2 for all of the simulations, sample size n = 40, and the number of simulation =1000.
# Simulations
# A series of 1000 simulations is run to create a data set for comparison purpose. Each simulation contain 40 observations and the expoential distribution function will be set to rexp(40, 0.2) where 0.2 is lambda value.
# Given data: n = 40; simNum = 1000; lambda = 0.2
# For reproducibility, set seed = 10000
# Exponential sampling parameters
# set seed for reproducability
set.seed(31)
# set lambda to 0.2
lambda <- 0.2
# 40 samples
n <- 40
# 1000 simulations
simulations <- 1000
# simulate
simulated_exponentials <- replicate(simulations, rexp(n, lambda))
# calculate mean of exponentials
means_exponentials <- apply(simulated_exponentials, 2, mean)

#Question 1

#Show where the distribution is centered at and compare it to the theoretical center of the distribution.
analytical_mean <- mean(means_exponentials)
analytical_mean
# analytical mean
theory_mean <- 1/lambda
theory_mean
# visualization
meanSample = mean(simul$Mean)
meanTheory = 1/lambda
hist(simul$Mean, breaks = 30, prob = TRUE,col = "lightblue", 
     main="Exponential Distribution of Sample Means", 
     xlab="Means of 40 Simulated Samples", ylab = "Counts")
abline(v = meanTheory, col= "red", lwd = 3)
abline(v = meanSample, col = "blue",lwd = 2)
legend('topright', c("Sample Mean", "Theoretical Mean"), 
       bty = "n",       
       lty = c(1,1), 
       col = c(col = "blue", col = "red"))
# The analytics mean is 5.006 the theoretical mean 5. The center of distribution of averages of 40 exponentials is very close to the theoretical center of the distribution.


# Question 2

# Show how variable it is and compare it to the theoretical variance of the distribution..

# standard deviation of distribution
standard_deviation_dist <- sd(means_exponentials)
standard_deviation_dist

# standard deviation from analytical expression
standard_deviation_theory <- (1/lambda)/sqrt(n)
standard_deviation_theory

# variance of distribution
variance_dist <- standard_deviation_dist^2
variance_dist
# variance from analytical expression
variance_theory <- ((1/lambda)*(1/sqrt(n)))^2
variance_theory

# Standard Deviation of the distribution is 0.7931608 with the theoretical SD calculated as 0.7905694. The Theoretical variance is calculated as ((1 / ??) * (1/???n))2 = 0.625. The actual variance of the distribution is 0.6291041
# Question 3
# Show that the distribution is approximately normal.
xfit <- seq(min(means_exponentials), max(means_exponentials), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(n)))
hist(means_exponentials,breaks=n,prob=T,col="skyblue",xlab = "means",main="Density of means",ylab="density")
lines(xfit, yfit, pch=22, col="red", lty=5)
# compare the distribution of averages of 40 exponentials to a normal distribution
qqnorm(means_exponentials)
qqline(means_exponentials, col = 2)
# Due to Due to the central limit theorem (CLT), the distribution of averages of 40 exponentials is very close to a normal distribution.
# 2. Basic inferential data analysis.
# Load the ToothGrowth data and perform some basic exploratory data analyses
# load the data ToothGrowth
data(ToothGrowth)
# preview the structure of the data
str(ToothGrowth)
# preview first 5 rows of the data
head(ToothGrowth, 5)
# Provide a basic summary of the data.
# data summary
summary(ToothGrowth)
# compare means of the different delivery methods
tapply(ToothGrowth$len,ToothGrowth$supp, mean)
# plot data graphically
library(ggplot2)
ggplot(ToothGrowth, aes(factor(dose), len, fill = factor(dose))) +
  geom_boxplot() +
  # facet_grid(.~supp)+
  facet_grid(.~supp, labeller = as_labeller(
    c("OJ" = "Orange juice", 
      "VC" = "Ascorbic Acid"))) +
  labs(title = "Tooth growth of 60 guinea pigs by dosage and\nby delivery method of vitamin C",
       x = "Dose in milligrams/day", 
       y = "Tooth Lengh") +
  scale_fill_discrete(name = "Dosage of\nvitamin C\nin mg/day") +
  theme_classic()
# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose.
# comparison by delivery method for the same dosage
t05 <- t.test(len ~ supp, 
              data = rbind(ToothGrowth[(ToothGrowth$dose == 0.5) & 
                                         (ToothGrowth$supp == "OJ"),],
                           ToothGrowth[(ToothGrowth$dose == 0.5) & 
                                         (ToothGrowth$supp == "VC"),]), 
              var.equal = FALSE)

t1 <- t.test(len ~ supp, 
             data = rbind(ToothGrowth[(ToothGrowth$dose == 1) & 
                                        (ToothGrowth$supp == "OJ"),],
                          ToothGrowth[(ToothGrowth$dose == 1) & 
                                        (ToothGrowth$supp == "VC"),]), 
             var.equal = FALSE)

t2 <- t.test(len ~ supp, 
             data = rbind(ToothGrowth[(ToothGrowth$dose == 2) & 
                                        (ToothGrowth$supp == "OJ"),],
                          ToothGrowth[(ToothGrowth$dose == 2) & 
                                        (ToothGrowth$supp == "VC"),]), 
             var.equal = FALSE)

# summary of the conducted t.tests, which compare the delivery methods by dosage,
# take p-values and CI
summaryBYsupp <- data.frame(
  "p-value" = c(t05$p.value, t1$p.value, t2$p.value),
  "Conf.Low" = c(t05$conf.int[1],t1$conf.int[1], t2$conf.int[1]),
  "Conf.High" = c(t05$conf.int[2],t1$conf.int[2], t2$conf.int[2]),
  row.names = c("Dosage .05","Dosage 1","Dosage 2"))

# show data table 
summaryBYsupp
# Conclusion
# For dosage of .5 milligrams/day and 1 milligrams/day does matter the delivery method. the delivery method for 2 milligrams/day. For dosage of 2 milligrams/day the delivery method doesn’t matter.

