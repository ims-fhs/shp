# vignette("A_plmPackage", package = "plm") # Code is from there

imsbasics::clc()
library(ggplot2)
library(dplyr)
library(plotly)
library(plm)
data("Grunfeld", package="plm")
?Grunfeld
p1 <- ggplot(data = Grunfeld, mapping = aes(x = year, y = inv, color = as.factor(firm))) +  geom_line()
p2 <- ggplot(data = Grunfeld, mapping = aes(x = year, y = value, color = as.factor(firm))) +  geom_line()
p3 <- ggplot(data = Grunfeld, mapping = aes(x = year, y = capital, color = as.factor(firm))) +  geom_line()
ggpubr::ggarrange(p1, p2, p3, ncol = 1,
                  labels = c("Dependent variable `gross investment`",
                             "Independent variable `value of the firm`",
                             "Independent variable `stock of plant and equipment`"))







# --------- One-way fixed effects model (within individual) --------------------
grun.fe <- plm(inv~value+capital, data = Grunfeld, model = "within", effect = "individual")
# summary(grun.fe)
summary(fixef(grun.fe), effect = "individual") # -> why are the fixed effects per individual so low?
Grunfeld %>% group_by(firm) %>% summarise(mean_inv = mean(inv)) # i would expect the fixed effects to be something like the group-means.


# The resulting train-predictions are often smaller than the original values
# -> there are even a lot of negatives!
Grunfeld <- cbind(Grunfeld, pred_fe = fitted(grun.fe))
ggplot(data = Grunfeld, mapping = aes(x = year, color = as.factor(firm))) +
  geom_line(aes(y = inv)) +
  geom_line(aes(y = pred_fe), linetype = "dotdash", size = 1) +
  ggtitle("true values (solid) vs. fitted values (dashed)")






# -------------------- Solution from debugging plm::fixef() --------------------
# also see examples Model Matrices (Design Matrice) -> https://www.youtube.com/watch?v=Hrr2anyK_5s

object <- grun.fe
effect <- "individual"
data <- model.frame(object)
(nw <- names(coef(object)))

# Group means of independent variables -> see line 43
Xb <- model.matrix(data, rhs = 1, model = "between", effect = effect)
# (Intercept)    value    capital
# 1            1 4333.845 648.4350
# 2            1 1971.825 294.8550
# 3            1 1941.325 400.1600
# 4            1  693.210 121.2450
# 5            1  231.470 486.7650
# 6            1  419.865 104.2850
# 7            1  149.790 314.9450
# 8            1  670.910  85.6400
# 9            1  333.650 297.9000
# 10           1   70.921   5.9415

# Group means on dependent variable -> see line 43
yb <- pmodel.response(data, model = "between", effect = effect)
# 1        2         3        4        5        6        7        8        9         10
# 608.0200 410.4750 102.2900  86.1235  61.8025  55.4110  47.5955  42.8915  41.8890   3.0845


# That's the reproduced fixef we get from the plm::fixef()-function
#
#       True means of our         Estimate of the FE-Model for the mean of inv
#      dependent variable  -      (We calculate the "mean for `inv` that we would
#            (inv)                 expect for every individual)
#
#          ___    ____           ___               ___
#         |       yb |          |  value    capital   |
#         | 608.0200 |          |  4333.845 648.4350  |
#         | 410.4750 |          |  1971.825 294.8550  |
#         | 102.2900 |          |  1941.325 400.1600  |
#         |  86.1235 |          |   693.210 121.2450  |      _____           ____
#         |  61.8025 |          |   231.470 486.7650  |     | value:   0.1101238 |
# fixef = |  55.4110 |    -     |   419.865 104.2850  |  *  | capital: 0.3100653 |
#         |  47.5955 |          |   149.790 314.9450  |     |_____            ___|
#         |  42.8915 |          |   670.910  85.6400  |
#         |  41.8890 |          |   333.650 297.9000  |       (Model Coefficients)
#         |   3.0845 |          |    70.921   5.9415  |
#         ___      ___          |___               ___|
#
#                             (Mean for every independent
#                               variable per individual)
#
(fixef <- yb - as.vector(crossprod(t(Xb[, nw, drop = FALSE]), coef(object))))
# Therefore the fixef() gives the "Residual" of the models Fixed-Effect. (Deviation
# from true Fixed effect and the models calculated fixed effect) !!

# 1           2           3            4          5            6           7           8           9           10
# -70.296717  101.905814 -235.571841  -27.809295 -114.616813  -23.161295  -66.553474  -57.545657  -87.222272   -6.567844

# plot Effects
my_summary <- Grunfeld %>% group_by(firm) %>%
  summarise(mean_value = mean(value), mean_capital = mean(capital), mean_inv = mean(inv))
my_summary$prediction_inv <- as.vector(as.matrix(my_summary[,c("mean_value", "mean_capital")]) %*% as.matrix(coef(object)))
my_summary$fixef <- my_summary$mean_inv - my_summary$prediction_inv
#Setup Axis for regression plane
axis_x <- seq(min(my_summary$mean_value), max(my_summary$mean_value), by = 50)
axis_y <- seq(min(my_summary$mean_capital), max(my_summary$mean_capital), by = 10)
df <- expand.grid(mean_value = axis_x, mean_capital = axis_y,KEEP.OUT.ATTRS = F)
df$mean_inv <- df$mean_value*coef(object)["value"] +  df$mean_capital*coef(object)["capital"]
# create plot
plot_ly() %>%
  add_markers(data = my_summary, x = ~mean_value, y = ~mean_capital, z=~mean_inv) %>%
  add_markers(data = my_summary, x = ~mean_value, y = ~mean_capital, z=~prediction_inv) %>%
  layout(title = "Orange = Model Prediction / Blue = True means of `inv` per firm") %>%
  add_markers(x = df$mean_value, y = df$mean_capital, z = df$mean_inv,
              marker = list(color = 'orange', size = 2))





# --------------------- Two-way fixed effects model ----------------------------
# also see: https://www.alexstephenson.me/post/two-ways-to-fit-two-way-fixed-effects-in-r/
grun.twfe <- plm(inv~value+capital, data=Grunfeld, model="within", effect="twoways")
summary(grun.twfe)
Grunfeld <- cbind(Grunfeld, pred_twfe = fitted(grun.twfe))
# calculated fixed effect on individuals (firms)
summary(fixef(grun.twfe, effect = "individual"))
# calculated fixed effect on time (firms)
summary(fixef(grun.twfe, effect = "time"))
# interaction of both fixed effects
summary(fixef(grun.twfe, effect = "twoways"))

twfe <- summary(fixef(grun.twfe, effect = "twoways"))
Grunfeld <- cbind(Grunfeld, twfe = twfe[,1])
plot_ly() %>%
  add_markers(data = Grunfeld, x = ~year, y = ~firm, z=~twfe, size = 0.5) %>%
  add_markers(data = Grunfeld, x = ~year, y = ~firm, z=~pred_twfe, size = 0.5) %>%
  add_markers(data = Grunfeld, x = ~year, y = ~firm, z=~inv, size = 0.5) %>%
  layout(title = "Two ways fixed-effect (blue) & Prediction (orange) True values (green)",
         scene = list(camera = list(
                                 eye = list(x = -4, y = -5, z = 3)
                                 # up = list(x = 0, y = 0, z = 1)
                                 # center = list(x = 0, y = 0, z = -0.5
                                               ),
                      aspectmode = "manual",
                      aspectratio = list(x=1, y=4, z=1)))




#  -------------- variance of the components in a model ------------------------

# Errorcomponents don't work on FE-Models (only on RE-Models)
ercomp(grun.fe) # no errorcomponents
ercomp(grun.twfe) # no errorcomponents


# We can evaluate error components with respect to time, individuals or both
#
# 1. Time as fixed effect
#      -> How much of the variance is explained by differences between timesteps
ercomp(inv~value+capital, data=Grunfeld, effect = "time")
# 2. Individuals as fixed effect = Default
#      -> How much of the variance is explained by differences between individuals
ercomp(inv~value+capital, data=Grunfeld, effect = "individual")
# 3. Interaction -> both as fixed effect
#      -> How much is interdependently explained by individuals and by time
ercomp(inv~value+capital, data=Grunfeld, effect = "twoways")


df <- imsbasics::load_rdata("sample.RData", "Scripts_Michi/model_playground/")
ercomp(depression ~ ermuedung + arbeit_qualifikation, data = df, effect = "time") # FE-Model (within-model) -> Fixed effect of time
ercomp(depression ~ ermuedung + arbeit_qualifikation, data = df, effect = "individual") # beween-model -> Fixed effect of individuals
ercomp(depression ~ ermuedung + arbeit_qualifikation, data = df, effect = "twoways") # two-ways -> Fixed effect of intercation



grun.re <- plm(inv~value+capital, data = Grunfeld, model = "random", effect = "twoways",
               random.method = "amemiya")
summary(grun.re)
ercomp(grun.re)

# ------------------------- Balancedness of a dataset --------------------------

punbalancedness(Grunfeld) # Very balanced
punbalancedness(df) # not very balanced



# --------------------------- Instrumental Variables ---------------------------
# See Paper "Introduction to instrumental variables and their application to large‑scale assessment data"
# -->  2SLS (two stage least squares)

# Specifically, an instrumental variable Z is an additional variable used to estimate the causal
# effect of variable X on Y. The traditional definition qualifies a variable Z as an instrumental
# (relative to the pair (X, Y)) if (i) Z is independent of all variables (including error terms)
# that have an influence on Y that is not mediated by X and (ii) Z is not independent
# of X (Pearl 2000, p. 247). Therefore, the instrumental variable Z affects Y only through its
# effect on X. Consequently, variable Z is unrelated to the outcome (Y) but is related to the
# predictor (X) and is not causally affected (directly or indirectly) by X, Y, or the error term U.
# In this approach, not only one but also multiple IVs and/or causal paths could be used.

data("Crime", package = "plm")
cr <- plm(lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen + ldensity +
            lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc +
            lpctymle + lpctmin + region + smsa + factor(year)
          | . - lprbarr - lpolpc + ltaxpc + lmix,   # -> instrumental variables !!
          data = Crime, model = "random")
summary(cr)



# ---------------------- Variable Coefficient Models ---------------------------

grun.fe <- plm(inv~value+capital, data = Grunfeld, model = "within")
summary(grun.fe)
# variable coefficient model "within" -> A different model is estimated for each individual
grun.fe.var <- pvcm(inv~value+capital, data=Grunfeld, model="within")
summary(grun.fe.var)


grun.re <- plm(inv~value+capital, data = Grunfeld, model = "random")
summary(grun.re)
# variable coefficient model "random" -> A Swamy model is calculated (A swamy model
# is a generalized least squares model which uses the results of the previous model)
grun.re.var <- pvcm(inv~value+capital, data=Grunfeld, model="random")
summary(grun.re.var)




# ------ Test: within & pool vs. variable coefficient model (pvcm) -------------
# pooltest tests the hypothesis that the same coefficients apply to each individual.
# It is a standard F test, based on the comparison of a model obtained for the full sample
# and a model based on the estimation of an equation for each individual. The first argument
# of pooltest is a plm object. The second argument is a pvcm object obtained with model="within".
# If the first argument is a pooling model, the test applies to all the coefficients
# (including the intercepts), if it is a within model, different intercepts are assumed.

grun.fe <- plm(inv~value+capital, data = Grunfeld, model = "within")
summary(grun.fe)
grun.pool <- plm(inv~value+capital, data = Grunfeld, model = "pooling")
summary(grun.pool)

pooltest(grun.fe, grun.fe.var) # Test FE-model vs. variable coefficient model
pooltest(grun.pool, grun.fe.var) # Test Pooling-model vs. variable coefficient model


# For Plot: generate dataframe with true values / Pool-Estimation & within-FE-estimation
mean_inv <- Grunfeld %>% group_by(firm) %>% mutate(mean_inv = mean(inv)) %>% select(mean_inv)
michi <- data.frame(value = Grunfeld$value, captial = Grunfeld$capital,
                    inv = Grunfeld$inv,
                    pred_FE = as.numeric(fitted(grun.fe)) + mean_inv$mean_inv,
                    pred_POOL = fitted(grun.pool))
# For Plot: create hyperplane for pooled model
oneplane <- expand.grid(x = c(0,max(michi$value)), y = c(0,max(michi$captial)))
oneplane$z <- coef(grun.pool)[1] + coef(grun.pool)[2]*oneplane$x + coef(grun.pool)[3]*oneplane$y
oneplane.m <- as.matrix(tidyr::spread(oneplane, key = x, value = z)[, -1])
# Plot
plot_ly() %>% # https://community.plotly.com/t/3d-scatter-3d-regression-line/4149/6
  add_surface(x = c(0,max(michi$value)),
              y = c(0,max(michi$captial)),
              z = oneplane.m,
              type = "surface",
              opacity = 0.4, colors = 'blue') %>%
  add_markers(data = michi, x = ~value, y = ~captial, z=~inv, marker = list(color = 'green', size = 5)) %>%
  add_markers(data = michi, x = ~value, y = ~captial, z=~pred_FE, marker = list(color = 'orange', size = 5)) %>%
  add_markers(data = michi, x = ~value, y = ~captial, z=~pred_POOL, marker = list(color = 'blue', size = 5)) %>%
  layout(title = "green = True values / orange = FE-Model / blue = Pooled-Model")



# ------------------------------ Test: Ist there an Effect? ---------------------
# Test the presence of individual, time or twoways effects !!!

# plmtest implements Lagrange multiplier tests of individual or/and time effects based on
# the results of the pooling model. Its main argument is a plm object (the result of a
# pooling model) or a formula.
# Two additional arguments can be added to indicate the kind of test to be computed. The argument type is one of:
#   "honda": Honda (1985), the default value,
#   "bp": Breusch and Pagan (1980),
#   "kw": King and Wu (1997)7,
#   "ghm": Gourieroux, Holly, and Monfort (1982).

plmtest(inv~value+capital, data=Grunfeld, effect="individual")
plmtest(inv~value+capital, data=Grunfeld, effect="time")
plmtest(inv~value+capital, data=Grunfeld, effect="twoways")


# ------------------------------ Test: within vs. pool? ------------------------
# pFtest computes F tests of effects based on the comparison of the within and the pooling model.
# Its main arguments are either two plm objects (a pooling and a within model) or a formula.

# comparison of FE-Modell  & Pooling Model
pFtest(grun.fe, grun.pool) # there is a significant difference in the models.



# ----------------------------- Test-Hausmann: FE vs. RE? ----------------------
# phtest computes the Hausman test which is based on the comparison of two sets of estimates
# (see Hausman (1978)). Its main arguments are two panelmodel objects or a formula. A classical
# application of the Hausman test for panel data is to compare the fixed and the random effects
# models
phtest(grun.fe, grun.re)



# ------------ Test: Unobserved (individual or time) effects? ------------------
# --> test for individual error components and serially correlated idiosyncratic errors.

# ...There may also be serial correlation of the “usual” kind in the idiosyncratic error
# terms, e.g., as an AR(1) process. By “testing for serial correlation” we mean testing
# for this latter kind of dependence...
# In plm we provide a number of joint, marginal and conditional ML-based tests, plus some
# semiparametric alternatives which are robust vs. heteroskedasticity and free from distributional assumptions.

pwtest(inv~value, data=Grunfeld)
pwtest(inv~capital, data=Grunfeld)
pwtest(inv~value + capital, data=Grunfeld)

# !!! While not rejecting the null favours the use of pooled OLS, rejection may follow
# from serial correlation of different kinds, and in particular, quoting Wooldridge (2002),
# “should not be interpreted as implying that the random effects error structure must be true”

# pwtest(depression~ermuedung + stress_arbeit, data=df) # takes a long time -> gives 2.2e-16



# ---------------- Test: serial correlation or random effects ------------------
#  Locally robust tests for serial correlation or random effects
# The presence of random effects may affect tests for residual serial correlation,
# and the opposite. One solution is to use a joint test, which has power against
# both alternatives. A joint LM test for random effects and serial correlation under
# normality and homoskedasticity of the idiosyncratic errors has been derived by
# Baltagi and Li (1991) and Baltagi and Li (1995) and is implemented as an option in pbsytest:

data("Produc")
# Test for "joint" effects
pbsytest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, test="j")


# The authors observe that, although suboptimal, these tests may help detecting the
# right direction of the departure from the null, thus complementing the use of joint tests

# Test for AR(1) serial correlation in idiosyncratic error terms
pbsytest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, test="ar")
# Test for random effects
pbsytest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, test="re")


# ---------- Test: AR(1) or MA(1) Correlation in RE-Model ----------------------
# Baltagi and Li (1991) and Baltagi and Li (1995) derive a Lagrange multiplier test for
# serial correlation in the idiosyncratic component of the errors under (normal, heteroskedastic)
# random effects. Under the null of serially uncorrelated errors, the test turns out to
# be identical for both the alternative of AR(1) and MA(1) processes. One- and two-sided
# versions are provided, the one-sided having power against positive serial correlation
# only. The two-sided is the default, while for the other one must specify the alternative
# option to "onesided":


pbltest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, alternative="onesided")
pbltest(log(gsp)~log(pcap)+log(pc)+log(emp)+unemp, data=Produc, alternative="twosided")

# As usual, the LM test statistic is based on residuals from the maximum likelihood estimate of the
# restricted model (random effects with serially uncorrelated errors). In this case, though, the
# restricted model cannot be estimated by OLS anymore, therefore the testing function depends on
# lme() in the nlme package for estimation of a random effects model by maximum likelihood. For
# this reason, the test is applicable only to balanced panels.

# No test has been implemented to date for the symmetric hypothesis of no random effects in a
# model with errors following an AR(1) process, but an asymptotically equivalent likelihood ratio test
# is available in the nlme package (see Section plm versus nlme and lme4).


# -------------------- test: Serial correlation in General ---------------------
# Look for serial correaltions (with standard tests) in the residuals of a Pooled or
# RE-Model (not for FE-Model!! -> see 2nd example in this section).
#
# For the random effects model, Wooldridge (2002) observes that under the null of
# homoskedasticity and no serial correlation in the idiosyncratic errors, the residuals
# from the quasi-demeaned regression must be spherical as well. Else, as the individual
# effects are wiped out in the demeaning, any remaining serial correlation must be due to
# the idiosyncratic component. Hence, a simple way of testing for serial correlation is to
# apply a standard serial correlation test to the quasi-demeaned model. The same applies in
# a pooled model, w.r.t. the original data.

# Breusch–Godfrey Test for Panel Models
pbgtest(grun.fe, order = 2)
# Durbin-Watson Test for Panel Models (based on lmtest::dwtest() )
pdwtest(grun.fe)




# The test is applicable to any FE panel model, and in particular to “short” panels with small T and large n.
data("EmplUK")
pwartest(log(emp) ~ log(wage) + log(capital), data=EmplUK)


# ----------------- Test: First-Difference based test --------------------------
# Test ....
pwfdtest(log(emp) ~ log(wage) + log(capital), data=EmplUK)



# ------------------- Test: Cross-Sectional depencence -------------------------

pcdtest(inv~value+capital, data=Grunfeld)

# -------------- tests: ... many more ------------------------------------------










# ------------------------------- plm vs. lmer ---------------------------------

grun.re.lmer <- lme4::lmer(inv ~ value + capital, data = Grunfeld)


library(lme4)
reGLS <- plm(inv~value+capital, data=Grunfeld, model="random") # A plm-random-effect model
reML <- lmer(inv~value+capital + (1|firm), data=Grunfeld) # ...is a lmer-model with random intercept per group
reML2 <- lmer(inv~value+capital + year + (1|firm), data=Grunfeld) # ...is a lmer-model with random intercept per group

coef(reGLS) # -> the parameters for the "fixed" effects are identical
summary(reML)$coefficients



### ------------------..........................................-------------

### =================== other vignettes (not too interesting) ==================
vignette("B_plmFunction", package = "plm") # Estimation of error component models with the plm function
vignette("C_plmModelComponents", package = "plm") # Model components for fitted models with plm
