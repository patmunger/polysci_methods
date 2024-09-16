###
### Logit and Probit Models
### Created by: Russ Luke (rluke2@gsu.edu)
### Updated by: Ozlem Tuncel - Fall 2022 (otuncelgurlek1@gsu.edu)
### R version:  4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
###

### Set up ----
# Check your working directory
getwd() 
# Specify the desired folder as your working directory
setwd()

# Set seed for replication
set.seed(1234)

### Upload library ----
library(tidyverse) ## Utility tools
library(lmtest) ## Supplemental and postestimation tests
library(sandwich) ## Specific for the sandwich version of robust SE calculations
library(stargazer) ## Tables
library(car) # Companion to Applied Regression
library(carData) # Supplemental Data
library(aod) # For wald.test
library(stats4) # For BIC
library(gmodels) # CrossTable function
library(DAMisc) # Proportional Reduction in Error
library(aod) # Wald test

### Upload data ----
# Get your data from carData package and adjust it as data frame class
# This is a data about: U.S. Women’s Labor-Force Participation
# see more => https://cran.r-project.org/web/packages/carData/carData.pdf
women_data <- Mroz
summary(women_data)

# US Women's Labor Force Participation Variables
# lfp: female labor force participation (Yes/No)
# k5: number of children under 5
# k618: number of children aged 6-18
# age: respondent age
# wc: respondent college educated (Yes/No)
# hc: husband college educated (Yes/No)
# lwg: log expected wage rate
# inc: family income exclusive of wife's income

### Data manipulation ----
women_data$lfp <- as.numeric(women_data$lfp)
women_data$lfp <- replace(women_data$lfp, women_data$lfp == 1, 0)
women_data$lfp <- replace(women_data$lfp, women_data$lfp == 2, 1)
women_data$lfp <- as.factor(women_data$lfp)
summary(women_data$lfp)

# Can easily do this yourself, but checking that the 0,1 ratio isn't above 60/40 or 40/60
CrossTable(women_data$lfp)

attach(women_data)

### Logit and probit models -----

# Note the "family = binomial(link = "logit")" is where logit is specified, 
# where probit is called in the probit models
m1 <- glm(lfp ~ k5 + k618, 
          family = binomial(link = "logit"),
          data = women_data)

m2 <- glm(lfp ~ k5 + k618 + age + wc + inc, 
          family = binomial(link = "logit"),
          data = women_data)

m3 <- glm(lfp ~ k5 + k618, 
          family = binomial(link = "probit"),
          data = women_data)

m4 <- glm(lfp ~ k5 + k618 + age + wc + inc, 
          family = binomial(link = "probit"),
          data = women_data)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# Or, alternatively
stargazer(m1, m2, m3, m4, type = "text")

### Proportional Reduction in Error ----
# Russ previously created his own function (see Wald Stargazer and PRE scripts)
# DAmisc package has pre() function that helps us with this
pre_m1 <- pre(m1)
pre_m2 <- pre(m2)
pre_m3 <- pre(m3)
pre_m4 <- pre(m4)

### Wald Test ----
# Wald Test (from aod package)
wald.test(b = coef(m1), Sigma = vcov(m1), Terms = 2:length(m1$coefficients))
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:length(m2$coefficients))
wald.test(b = coef(m3), Sigma = vcov(m3), Terms = 2:length(m3$coefficients))
wald.test(b = coef(m4), Sigma = vcov(m4), Terms = 2:length(m4$coefficients))

# To get this into your output table you can either manually copy and paste the 
# values into your table, or we can use some user generated functions to create 
# a function that does this for you

# To call the correct number of stars from the p value of the wald test
wald.test.stars<- function(pvalue){
  if(pvalue < 0.1 & pvalue >= 0.05){return("*")
    } else if(pvalue < 0.05 & pvalue >= 0.01){return("**")
    } else if(pvalue < 0.01){return("***")
    } else {return(" ")}
}

# Testing
wald.test.stars(0.001)
wald.test.stars(0.01)
wald.test.stars(0.02)
wald.test.stars(0.06)
wald.test.stars(0.11)

# Pulling the chi2 statistic from the wald.test
stargazer.wald.chi<- function(model){
  require(aod)
  w1 <- wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:length(model$coefficients))
  w1chi <- w1$result$chi2[1]
    return(format(round(w1chi, 3), nsmall=3)) 
  }

# Testing
stargazer.wald.chi(m1)

# Pulling the significance level from the wald.test 
stargazer.wald.sig<- function(model){
  require(aod)
  w1<-wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:length(model$coefficients))
  w1p<-w1$result$chi2[3]
  starw1<-wald.test.stars(w1p)
  return(starw1)
}

# Testing
stargazer.wald.sig(m1)

# Putting everything together for stargazer
stargazer.wald.output <- function(model){
  out<-paste(stargazer.wald.chi(model), stargazer.wald.sig(model))
  return(out)
}

# Testing the combined character string for stargazer
stargazer.wald.output(m1)

### Output table with Wald Chi2 and PRE using our functions ----
stargazer(m1, m3, 
          type = "text", 
          add.lines = list(
            c("Wald $\\chi^{2}$", stargazer.wald.output(m1), stargazer.wald.output(m3)),
            c("P.R.E.", round(pre_m1$pre, 2), round(pre_m3$pre, 2))
            )
          )

stargazer(m1, m2, m3, m4, 
          type = "text", 
          add.lines=list(
            c("Wald $\\chi^{2}$", stargazer.wald.output(m1), stargazer.wald.output(m2),
              stargazer.wald.output(m3), stargazer.wald.output(m4)),
            c("P.R.E.", round(pre_m1$pre, 2), round(pre_m2$pre, 2), round(pre_m3$pre, 2), 
              round(pre_m4$pre, 2))
            )
          )

# Notes: add.lines is where we're adding our information - this can be used for 
# many other things add.lines=list(c() lets us add character strings separated 
# by commas to denote different columns otherwise this would all show up in the 
# first column. 

# See https://rdrr.io/cran/stargazer/man/stargazer_table_layout_characters.html for options.

### AIC and BIC ----
AIC(m1)
BIC(m1)

m1
m2

BIC(m1)
BIC(m2)

### Likelihood Ratio Test (LR) ----
lrtest(m2, m1)
lrtest(m4, m3)

m3$coefficients
m4$coefficients

lrtest(m4, m3)

### Wald Test for Model Comparison ----
# Wald Test (from aod package)

summary(m2)

wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:2)
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 3:3)
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 4:4)
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 5:5)
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:3)

