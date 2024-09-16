# Russ created this script for those who do not want to use DAmisc's pre() 
# function. 

# Upload library ----
library(dplyr)
require(plyr)

mode_of_model <- function(model_name) {
  outcome<-model_name$y
  categories <- unique(outcome)
  categories[which.max(tabulate(match(outcome, categories)))]
}

# Write new pre() function
pre <- function(model_name){
  if(length(class(model_name))==1){return(cat("\n","Error: Model not Logit or Probit", "\n", "\n"))}
  if((model_name$family$link!="logit")&(model_name$family$link!="probit")){return(cat("\n", "Error: Model not Logit or Probit", "\n", "\n"))}
  predict<-ifelse(model_name$fitted.values> 0.5, 1, 0)
  predict<-as.vector(predict)
  if(sd(predict)==0){return(cat("\n", "Error: No variation in predicted dependent variable! All values are ", mean(predict), "\n", "\n"))}
  temp<-as.data.frame(predict)
  temp$y<-model_name$y
  temp$correct<-ifelse(temp$predict==temp$y, 1, 0)
  num_count<-table(temp$correct==1)
  num_count<-as.numeric(num_count[2])
  prop_correct<-num_count/length(temp$correct)
  out<-mode_of_model(model_name)
  num_mode<-table(model_name$y==out)
  num_mode<-as.numeric(num_mode[2])
  prop_mode<-num_mode/length(temp$correct)
  prop_red_error<-(((prop_correct*100)-(prop_mode*100))/(100-(prop_mode*100)))
  perc_correct<-prop_correct*100
  perc_mode<-prop_mode*100
  perc_red_error<-prop_red_error*100
  pre_out<-as.data.frame(prop_red_error)
  names(pre_out)[1] <- 'PRE'
  cat("\n","  Proportion Correctly Predicted:    ", format(round(prop_correct, 3), nsmall = 3), "\n") 
  cat("   Proportion Modal Category:         ", format(round(prop_mode, 3), nsmall = 3), "\n")
  cat("   Proportional Reduction in Error:   ", format(round(prop_red_error, 3), nsmall = 3), "\n", "\n")
  cat("   Percent Correctly Predicted:       ", format(round(perc_correct, 3), nsmall = 3), "\n") 
  cat("   Percent Modal Category:            ", format(round(perc_mode, 3), nsmall = 3), "\n")
  cat("   Percent Reduction in Error:        ", format(round(perc_red_error, 3), nsmall = 3), "\n", "\n")
  return(pre_out)
}

## Function for call in Stargazer
stargazer.pre <- function(model){
  temp1 <-pre(model)
  temp2 <-format(round(temp1, 3), nsmall=3) 
  out <-paste(temp2)
  return(out)
}