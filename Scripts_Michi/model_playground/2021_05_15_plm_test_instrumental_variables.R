# ------------------------------------------------------------------------------
# Author:  SLC
# Date:    15.05.2021
# Summary: Some Base Models & playaounrd with instrumental variables
#
#
# see https://bookdown.org/ccolonescu/RPoE4/random-regressors.html#mjx-eqn-eqmultreg10
#
#


imsbasics::clc()
library(PoEdata)
library(AER) # has the ivreg() function
library(stargazer)

data("mroz", package="PoEdata")
mroz1 <- mroz[mroz$lfp==1,] #restricts sample to lfp=1

# 2SLC - 1.step
educ.ols <- lm(educ~exper+I(exper^2)+mothereduc, data=mroz1)
educHat <- fitted(educ.ols)
# 2SLS - 2.step
wage.2sls <- lm(log(wage)~educHat+exper+I(exper^2), data=mroz1)


# OLS
mroz1.ols <- lm(log(wage)~educ+exper+I(exper^2), data=mroz1)

# Instrumental variables (only mothereduc)
mroz1.iv <- ivreg(log(wage)~educ+exper+I(exper^2)|
                    exper+I(exper^2)+mothereduc, data=mroz1)

# Instrumental variables (mothereduc + fathereduc)
mroz1.iv1 <- ivreg(log(wage)~educ+exper+I(exper^2)|
                     exper+I(exper^2)+mothereduc+fathereduc,
                   data=mroz1)


# Instrumental variables (mothereduc + fathereduc)
# mroz1$id <- seq(1,nrow(mroz1))
# mroz1$year <- rep(1976, nrow(mroz1))
# mroz1 <- mroz1[,c(26,27,seq(1,25))]
# mroz1.plm_iv <- plm::plm(log(wage)~educ+exper+I(exper^2)|
#                      exper+I(exper^2)+mothereduc+fathereduc,
#                    data=mroz1)



stargazer(mroz1.ols, wage.2sls, mroz1.iv, mroz1.iv1,
          type = "text",
          title="Wage equation: OLS, 2SLS, and IV models compared",
          header=FALSE,
          keep.stat="n",  # what statistics to print
          omit.table.layout="n",
          star.cutoffs=NA,
          digits=4,
          #  single.row=TRUE,
          intercept.bottom=FALSE, #moves the intercept coef to top
          column.labels=c("OLS","explicit 2SLS", "IV mothereduc",
                          "IV mothereduc and fathereduc"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: wage",
          model.names=FALSE,
          star.char=NULL)
