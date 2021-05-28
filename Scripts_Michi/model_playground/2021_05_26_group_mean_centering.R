# Center a variable base on a group


# ---------- misty-package  ----------------------------------------------------
library(misty)

# Predictors in a multilevel regression
dat.ml <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                     group = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                     x.l1 = c(4, 2, 5, 6, 3, 4, 1, 3, 4),
                     x.l2 = c(4, 4, 4, 1, 1, 1, 3, 3, 3),
                     y = c(5, 3, 6, 3, 4, 5, 2, 6, 5))

# Center level-1 predictor at the grand mean (CGM)
center(dat.ml$x.l1)
# Center level-1 predictor within cluster (CWC)
center(dat.ml$x.l1, type = "CWC", group = dat.ml$group)
# Center level-2 predictor at the grand mean (CGM)
center(dat.ml$x.l2, type = "CGM", group = dat.ml$group)



# ---------- rockchalk-package  ------------------------------------------------

library(rockchalk) # see ?rockchalk::gmc()

data(state)
statenew <- as.data.frame(state.x77)
statenew$region <- state.region
statenew$state <- rownames(statenew)
head(statenew.gmc1 <- gmc(statenew, c("Income", "Population"), by = "region"))
head(statenew.gmc2 <- gmc(statenew, c("Income", "Population"), by = "region",
                          fulldataframe = FALSE))
head(statenew2 <- cbind(statenew, statenew.gmc2))


## The following box plots should be identical
boxplot(Income ~ region, statenew.gmc1)
boxplot((Income_mn + Income_dev) ~ region, statenew.gmc1)


## By multiple variables
fakedat <- data.frame(i = 1:200, j = gl(4, 50), k = gl(20, 10),
                      y1 = rnorm(200), y2 = rnorm(200))
head(fakedat)
head(gmc(fakedat, "y1", by = "k"), 20)
head(gmc(fakedat, "y1", by = c("j", "k"), fulldataframe = FALSE), 40)
head(gmc(fakedat, c("y1", "y2"), by = c("j", "k"), fulldataframe = FALSE))
