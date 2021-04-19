# ------------------------------------------------------------------------------
# Author:  SLC
# Date:    11.04.2021
# Summary: Work through the vignette of lme4::lmer()
#          https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html
#          The vignette analyses the effect of a variable "STRESS" on a variable
#          "NegAff" (negative Affect). The dataset is a panel-dataset with 191 peple,
#          each having about 20-40 observations on some days.
#
#          Ziel: get helpful tipps for analysis with lmer()
# ------------------------------------------------------------------------------
imsbasics::clc()

# --------------------------- preparatory work ---------------------------------
library(lme4)
library(lmerTest) # -> adds degrees of Freedom & p-values to summary(lmer-model)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)
library(ggplot2)

## load some sample data for examples
data(aces_daily, package = "JWileymisc")

# ----------------------------- Data Analysis ----------------------------------

# Summary of the variables of interest (dependent and independent variables)
summary(aces_daily$NegAff)
summary(aces_daily$STRESS)
summary(aces_daily$UserID)

ggplot(aces_daily, aes(x = STRESS, y = NegAff)) +
  geom_point(alpha = 0.5) +
  theme(legend.position = "none") +
  geom_line(mapping = aes(x = STRESS, y = NegAff, color = as.factor(UserID)),
            stat="smooth", method = "lm", alpha = 1, size = 0.4) +
  geom_line(stat="smooth", method = "lm", alpha = 0.6, size = 3, color = "blue")


# ICC (Interclass correlation coefficient). The ICC is a measure of the proportion of variance
# that is between people versus the total variance (i.e., variance between people and variance within persons)
# ICC = 1 --> All variance exists between people (and no variance within people)
# ICC = 0 --> All variance exists within people (and the average of all people is identical)
iccMixed(dv = "NegAff", id = "UserID", data = aces_daily) # --> We have variance within and between
iccMixed(dv = "STRESS", id = "UserID", data = aces_daily) # --> We have variance within and between


# Distribution of the variables of interest (dependent and independent variables)
# --> we analyse between and within person aspects of a variable separatly using multilevelTools::meanDecompose()
#     meanDecompose() returns a list with X values at different levels, here by ID and the residuals, which in
#     this case are within person effects

tmp1 <- meanDecompose(NegAff ~ UserID, data = aces_daily)
# Between-Distritubion of "NegAff"
plot(testDistribution(tmp1[["NegAff by UserID"]]$X, extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person Negative Affect")
# within-Distritubion of "NegAff"
plot(testDistribution(tmp1[["NegAff by residual"]]$X, extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person Negative Affect")

tmp2 <- meanDecompose(STRESS ~ UserID, data = aces_daily)
# Between-Distritubion of "STRESS"
plot(testDistribution(tmp2[["STRESS by UserID"]]$X, extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person STRESS")
# Within-Distritubion of "STRESS"
plot(testDistribution(tmp2[["STRESS by residual"]]$X, extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person STRESS")


# ------------------------------- Modelling ------------------------------------
# The random effects are allowed to be correlated, so that, for example, people who have higher
# negative affect under low stress may also have a flatter slope as thye may have less room to
# increase in negative affect at higher stress levels.


# 1ST try -> Error
m <- lmer(NegAff ~ STRESS + (1 + STRESS | UserID), data = aces_daily)

# 2nd try -> with controls for the optimization algorithm
strictControl <- lmerControl(optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD",
                                            xtol_abs = 1e-12, ftol_abs = 1e-12))
m <- lmer(NegAff ~ STRESS + (1 + STRESS | UserID), data = aces_daily, control = strictControl)
# m <- lmer(NegAff ~ STRESS + (STRESS | UserID), data = aces_daily, control = strictControl) # = equal formulation
summary(m)

# ------------------------- Examination of model -------------------------------
# For linear mixed effects / multilevel models, the residuals should follow a normal distribution,
# the random effects should follow a multivariate normal distribution, and the residual variance
# should be homogeneous (the same) as a single residual variance is estimated and used for the whole model.
md <- modelDiagnostics(m, ev.perc = .001)
# top left: Distribution of residuals is near to normal, but very sharp -> many residuals are considered as extreme values.
# top middle: Residuals vs. fitted -> Variance of residuals is more or less constant. (For values below 1.7 all Residuals tend to go to 0, because many people reported a 1 for "NegAff")
# top right, bottom left -> distribution of random intercept & random slope
# bottom middle: -> Multivariate normality test based on the mahalonbis distances (test whether random slope & intercept are multivariate normal -> We might consider dropping these individuals from the analysis to examine whether results are sensitive to these extreme cases)
#    The Mahalanobis Distance is a generalized multivariate distance-metric, that:
#       - accounts for the fact that the variances in each direction are different.
#       - accounts for the covariance between variables.
#       - reduces to the familiar Euclidean distance for uncorrelated variables with unit variance.
#       - see: https://blogs.sas.com/content/iml/2012/02/15/what-is-mahalanobis-distance.html#prettyPhoto
plot(md, ask = FALSE, ncol = 3, nrow = 2)

# We can examine the extreme vlaues of the modelDiagnostics-object
md$extremeValues
unique(md$extremeValues$EffectType) # --> We can see the extreme values for Reisiduals, Intercept, Slope & Multivariate Random Effect -> See plot 1,3,4,5
# We extract the The people (UserID) that are considered extreme for the Multivariate Random Effect
mvextreme <- subset(md$extremeValues, EffectType == "Multivariate Random Effect UserID")
unique(mvextreme$UserID) # these are the 3 extreme ID's in plot 5


# ------------------------------- 2nd Model ------------------------------------
# ...and update the model (without the 3 extreme UserID's)
m2 <- update(m, data = subset(aces_daily, UserID %!in% unique(mvextreme$UserID)))
md2 <- modelDiagnostics(m2, ev.perc = .001)
plot(md2, ask = FALSE, ncol = 3, nrow = 2, main = "asdf")

mvextreme2 <- subset(md2$extremeValues, EffectType == "Multivariate Random Effect UserID")
unique(mvextreme2$UserID) # -> Now we have 1 more extreme UserID

# ------------------------------- 3rd Model ------------------------------------
m3 <- update(m, data = subset(aces_daily, UserID %!in% c(unique(mvextreme$UserID), unique(mvextreme2$UserID))))
md3 <- modelDiagnostics(m3, ev.perc = .001)
plot(md3, ask = FALSE, ncol = 3, nrow = 2)


# ------------------------------- Final evaluation -----------------------------
summary(m3)
# Now that we have a model whose diagnostics we are reasonably happy with, we can examine the results.
# The modelPerformance() function from the multilevelTools package gives some fit indices for the overall
# model.
#   - Marginal R2:      variance accounted for by the fixed effects
#   - Conditional R2:   variance accounted for by the fixed & random effects combined
#   - We also get information criterion (AIC, BIC), although note that with a REML estimator, the log
#     likelihood and thus information criterion are not comparable to if the ML estimator was used
modelPerformance(m3)


# The modelTest() function in multilevelTools provides further tests, including tests of the combined
# fixed + random effect for each variable and effect sizes based off the independent change in marginal
# and conditional R2, used to calculate a sort of cohen’s F2. All of the results are available in a
# series of tables for any programattic use.
#    - effect sizes -> based off the independent change in marginal and conditional R2, used to calculate a sort of cohen’s F2
#
mt3 <- modelTest(m3)
mt3 # many details

# good overview
# -------------
# (for individual use or reporting, calling APAStyler() will produce a nicely formatted output
# for humans. Confidence intervals are added in brackets and the effect sizes at the bottom are
# listed for stress considering fixed + random effects together.
#    - effect sizes: "MarginalF2/ConditionalF2"
APAStyler(mt3)

# overview with table-styling
APAStyler(mt3, format = list(
            FixedEffects = "%s, %s (%s; %s)",
            RandomEffects = c("%s", "%s (%s, %s)"),
            EffectSizes = "%s, %s; %s"),
          digits = 3,
          pcontrol = list(digits = 3, stars = FALSE,
                          includeP = TRUE, includeSign = TRUE,
                          dropLeadingZero = TRUE))

# --------------------------- sensitiviy analysis ------------------------------
# Use APA-Styler to comapre different models in 1 table
mt <- modelTest(m)
APAStyler(list(Original = mt, `Outliers Removed` = mt3))

