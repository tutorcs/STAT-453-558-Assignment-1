# Independent   sample t-test with cement data from the book

# 1) Data preparation
Modified<-c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
Unmodified<-c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
cement_data <- data.frame( 
  group = rep(c("Modified", "Unmodified"), each = 10),
  strength = c(Modified,  Unmodified)
)
print(cement_data)

# 2) Compute summary statistics by group

#install.packages("dplyr")
library("dplyr")
group_by(cement_data, group) %>%
  summarise(
    count = n(),
    mean = mean(strength, na.rm = TRUE),
    sd = sd(strength, na.rm = TRUE)
  )
# 3) Plot and check normality assumptions
#install.packages("ggpubr")
library("ggpubr")

ggboxplot(cement_data, x = "group", y = "strength", 
          color = "group", palette = c("blue", "red"),
          order = c("Modified", "Unmodified"),
          ylab = "Strength", xlab = "Groups")

qqnorm(Modified, ylim=c(min(Modified)-1,max(Modified)+1), main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified",
       plot.it = TRUE, datax = FALSE)

qqline(Modified, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 5)

qqnorm(Unmodified, ylim=c(min(Unmodified)-1,max(Unmodified)+1), main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Unmodified",
       plot.it = TRUE, datax = FALSE)

qqline(Unmodified, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 5)

# 4) Test H0:mu1=mu2
var.test(Modified, Unmodified) # Test if H0: two variances are equal
t.test(Modified,Unmodified, alternative = "two.sided",paired = FALSE,var.equal = TRUE,conf.level = 0.95)   
#t.test(Modified,Unmodified, alternative = "less",paired = FALSE,var.equal = TRUE,conf.level = 0.95)

#Model

res.lm=lm(strength~factor(group),data=cement_data)
summary(res.lm)

#Clean data (it keeps the loaded packages)
rm(list = ls())


# Sample size Calculation
# Suppose we want to find the sample size to detect differences of 0.5 in the tension bond, 
#assume sigma=0.25, delta=(|mu1-mu2|/sigma)=2, power 0.95, alpha=0.05

#install.packages("pwr")
require("pwr")
pwr.t.test(n = NULL, d = 2, sig.level = 0.05, power = 0.95, type = "two.sample", alternative = "two.sided")

