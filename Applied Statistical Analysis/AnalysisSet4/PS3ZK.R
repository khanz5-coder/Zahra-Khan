#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

#First we must categorize the GDPWdiff column and then factor it. 
gdp_data$GDPWdiff_cat <- ifelse(gdp_data$GDPWdiff > 0, "Positive",
                        ifelse(gdp_data$GDPWdiff == 0, "No Change", "Negative"))
gdp_data$GDPWdiff_cat <- factor(gdp_data$GDPWdiff_cat)

#set the reference category
gdp_data$GDPWdiff_cat <- relevel(gdp_data$GDPWdiff_cat, ref = "No Change")

# run model
mult.log <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data)
summary(mult.log)

od <- exp(coef(mult.log))

stargazer(mult.log)
stargazer(od)
orci <- exp(cbind(OR = coef(mult.log), confint(mult.log)))

ord.log <- polr(GDPWdiff_cat ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ord.log)
stargazer(ord.log)
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Calculate confidence intervals
(ci <- confint(ord.log))
# Convert to odds ratio
or <- exp(cbind(OR = coef(ord.log), ci))
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

#Running the Model
pois_mod <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                data = mexico_elections, 
                family = poisson(link = "log"))

summary(pois_mod)
stargazer(pois_mod)

#Predicted Values
newdata <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1
)
predict(pois_mod, newdata, type = "response")
# 0.01494818
lb <- exp(-3.810 - 0.081 - 0.312)
lb
# [1] 0.01495066