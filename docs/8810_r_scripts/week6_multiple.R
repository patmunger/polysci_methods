###
### Purpose: Week 6 - Multiple Regression
### Author: Ozlem Tuncel; Patrick Munger 
### Date: Spring 2025
###

### Load necessary packages ----
# Use install.packages() if you do not have this package
library(tidyverse)  # Data manipulation
library(stargazer)  # Creates nice regression output tables
library(lmtest)     # Breusch-Pagan test 
library(psych)      # Histograms and correlations for a data matrix
library(dotwhisker) # Dot-and-whisker plot of regression result
library(lm.beta)    # Get beta coefficients 
library(broom)      # tidy models

## Load your data ----
# We are using V-Dem version 12 
my_data <- readRDS("data/vdem12.rds") 

# Let's change names of some of these variables for the sake of simplicity
# I am also subsetting it to only US
us_data <- my_data |>
  filter(country_name == "United States of America") |> 
  rename(democracy = v2x_polyarchy, 
         gdp_per_capita = e_gdppc,
         urbanization = e_miurbani)

### Bivariate OLS ----
# Let's fit a bivariate and multivariate models
simple <- lm(democracy ~ gdp_per_capita, data = us_data)
multiple <- lm(democracy ~ gdp_per_capita + urbanization, data = us_data)

# View model summary
summary(simple)
summary(multiple)

stargazer(simple, multiple,
          type = "text",
          title = "Factors explaining democracy in the US",
          covariate.labels = c("GDP per capita", "Urbanization"), 
          dep.var.labels = c("Democracy"),
          column.labels = c("Simple OLS", "Multiple OLS"),
          ci.level = 0.95, 
          star.cutoffs = c(0.05), 
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes", 
          notes = "p < 0.05. Standard errors are in parentheses.",
          p.auto = T,
          report = "vcsp*")


### Gauss-Markov assumptions ---
# Testing for multicollinearity 
us_data |> 
  select(democracy, gdp_per_capita, urbanization) |> 
  pairs.panels(lm = T,
               method = "pearson")

# Looking for heteroskedasticity - plot residuals ~ fitted.values
multiple |> 
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(col = 'blue') + 
  geom_abline(slope = 0) +
  labs(x = "Fitted values", y = "Residuals") + 
  theme_bw()
# Perform Breusch-Pagan test
bptest(multiple)
# Since the p-value is less than 0.05, we reject the null hypothesis. 
# We have sufficient evidence to say that heteroscedasticity is present in the model.

# Normality of Residuals 
hist(multiple$residuals)
# Use qqnorm and qqline to further examine normaility of residuals
qqnorm(residuals(multiple), ylab = "Residuals")
qqline(residuals(multiple))

# Serial/autocorrelation
# You can visualize or use a test
# Durbin-Watson test for autocorrelation
dwtest(multiple, data = us_data)
# Or, plot residuals for autocorrelation
stats::acf(multiple$residuals, type = "correlation")
