#Giancarlo Carino
#EC 410
#Problem Set 1

install.packages(pkgs=c("psych", "stargazer", "lmtest", "car"))
library(psych)
library(stargazer)
library(lmtest)
library(car)
library(readxl)

setwd("/Users/gc/Desktop/GitHub/EC-410-Food-and-Agricultural-Economics/Problem Set 1/data")
pset1 <- read_excel("HW1_data.xlsx")
attach(pset1)

#Convert all nominal prices into real prices (base period: 1990)
pset1$realIncome <- (pset1$Income/pset1$CPI)*130.66
pset1$realApPr <- (pset1$ApPr/pset1$CPI)*130.66
pset1$realBaPr <- (pset1$BaPr/pset1$CPI)*130.66
pset1$realOrPr <- (pset1$OrPr/pset1$CPI)*130.66
pset1$realGfPr <- (pset1$GfPr/pset1$CPI)*130.66
pset1$realGrPr <- (pset1$GrPr/pset1$CPI)*130.66

#Sort by year
pset1 <- pset1[with(pset1, order(Year)),]

#Create time series variable t
pset1$t <- time(pset1$Apples)

#Data Summary Statistics
stargazer(
  as.data.frame(pset1[c("Apples", "Grapes", "Bananas", "Grapefruit", "Oranges", "realIncome", "realApPr", "realGrPr", "realBaPr", "realOrPr", "realGfPr")]), type = "text", title = "Descriptive Statistics", digits = 2, out = "table.htm", summary.stat = c("n","mean", "sd", "min", "max")
)

#Simple Regression (DV: QApples, IndV: realApPr)
simpRegAp <- lm(pset1$Apples ~ pset1$realApPr)
summary(simpRegAp)

#Multiple Regression (DV: QApples, IndV: realApPr, realBaPr, realIncome, t)
multRegAp <- lm(pset1$Apples ~ pset1$realApPr + pset1$realBaPr + pset1$realIncome + pset1$t)
summary(multRegAp)

#Create ln variables for log-log regression
pset1$ln_realApPr <- log(pset1$realApPr)
pset1$ln_realBaPr <- log(pset1$realBaPr)
pset1$ln_realIncome <- log(pset1$realIncome)

#Log-Log regression
ln_multRegAp <- lm(pset1$Apples ~ pset1$ln_realApPr + pset1$ln_realBaPr + pset1$ln_realIncome + pset1$t)
summary(ln_multRegAp)
