# ------------------------------------------------------------------------------
# Author:  SLC
# Date:    16.05.2021
# Summary: Playaround with the vcrpart package
#
#  See vignette: https://cran.r-project.org/web/packages/vcrpart/vignettes/tvcm.pdf
# The tree-based TVCM algorithm and its implementation in the R package vcrpart
# are introduced for generalized linear models. The purpose of TVCM is to learn whether
# and how the coefficients of a regression model vary by moderating variables. A separate
# partition is built for each potentially varying coefficient, allowing the user to specify
# coefficient-specific sets of potential moderators, and allowing the algorithm to select
# moderators individually by coefficient.
#
#
# ?tvcm() - Tree-based varying coefficient regression models
# ?tvcglm() - Coefficient-wise tree-based varying coefficient regression based on generalized linear models
# ?tvcolmm() - Tree-based varying coefficient regression based on ordinal and nominal two-stage linear mixed models.
# ?fvcglm() - Bagging and Random Forests based on tvcm

# ------------------------------------------------------------------------------

imsbasics::clc()
library(mgcv)
library(vcrpart)
library(sjPlot)


# Example: Gender Gap with stats::glm() ----------------------------------------
UCBA <- as.data.frame(UCBAdmissions)
UCBA$Admit <- 1 * (UCBA$Admit == "Admitted")
UCBA$Female <- 1 * (UCBA$Gender == "Female")
head(UCBA, 3)
glmL.UCBA <- glm(formula = Admit ~ -1 + Dept + Dept:Female, data = UCBA, family = binomial(), weights = UCBA$Freq)
coef(summary(glmL.UCBA))[, -4]

# Example: Gender Gap with vcrpart::tvcglm() ----------------------------------------

# vc(Dept): -> Moderator
# cv(Dept, by = Female) -> Moderator Dept that acts on predictor Female
#
# --> When no "by" argument is given, the vc term defines a varying intercept

vcmL.UCBA <- tvcglm(formula = Admit ~ -1 + vc(Dept) + vc(Dept, by = Female),
                    data = UCBA,
                    family = binomial(),
                    weights = UCBA$Freq,
                    control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
   mindev = 0.0, # D_min = 0 ninimal log-likelihood reduction --> gives us the largest possible tree.
   cv = FALSE))  # No cross-Validation.


# debug(plot.tvcm)
plot(vcmL.UCBA, type = "coef", part = "A")
plot(vcmL.UCBA, type = "coef", part = "B")

# Cross-Validation
cv.UCBA <- cvloss(vcmL.UCBA, folds = folds_control(weights = "freq", seed = 13, K = 20))
cv.UCBA
plot(cv.UCBA)

my_cp <- cv.UCBA$cp.hat # the complexity parameter --> Penalty for pruning

# Pruning
vcm.UCBA <- prune(vcmL.UCBA, cp = my_cp) # we could also use cp = 6, 10, 100 for example
plot(vcm.UCBA, type = "coef", part = "A")
plot(vcm.UCBA, type = "coef", part = "B")



# Example: Pima Indians diabetes data ------------------------------------------
# We model the response variable diabetes using a logistic model with a varying
# intercept and a varying slope for glucose in the predictor function. The remaining
# covariates 3–7 of Table 1 are used as potential moderators for both varying coefficients.
#
control <- tvcglm_control(folds = folds_control(seed = 13))

data("PimaIndiansDiabetes2", package = "mlbench")
Pima <- na.omit(PimaIndiansDiabetes2[, -c(4, 5)])


vcm.Pima.1 <- tvcglm(diabetes ~ -1 + vc(pregnant, pressure, mass, pedigree, age) +
                                     vc(pregnant, pressure, mass, pedigree, age, by = glucose),
                     data = Pima, family = binomial(), control = control)

coef(vcm.Pima.1)
plot(vcm.Pima.1, type = "coef", part = "A") # all varying intercepts -> what are the most significant intercept differences? --> mass & age determine differences in intercepts !!!
plot(vcm.Pima.1, type = "coef", part = "B") # all varying slopes -> none!!


vcm.Pima.2 <- tvcglm(diabetes ~ 1 + glucose +
                       vc(pregnant) + vc(pregnant, by = glucose) +
                       vc(pressure) + vc(pressure, by = glucose) +
                       vc(mass) + vc(mass, by = glucose) +
                       vc(pedigree) + vc(pedigree, by = glucose) +
                       vc(age) + vc(age, by = glucose),
                     data = Pima, family = binomial(), control = control)
coef(vcm.Pima.2)
plot(vcm.Pima.2, type = "coef", part = "A") # -
plot(vcm.Pima.2, type = "coef", part = "B") # -
plot(vcm.Pima.2, type = "coef", part = "C") # -
plot(vcm.Pima.2, type = "coef", part = "D") # -
plot(vcm.Pima.2, type = "coef", part = "E") # varying intercept on mass
plot(vcm.Pima.2, type = "coef", part = "F") # -
plot(vcm.Pima.2, type = "coef", part = "G") # -
plot(vcm.Pima.2, type = "coef", part = "H") # varying slope on pedigree by glucose
plot(vcm.Pima.2, type = "coef", part = "I") # varying intercept on age
plot(vcm.Pima.2, type = "coef", part = "J") # -




# Example: racial wage gap -----------------------------------------------------

data("Schooling", package = "Ecdat")
Schooling <- Schooling[c(19, 21, 7, 28, 9, 14, 17, 18, 20, 23, 2, 4)]
Schooling$black <- 1 * (Schooling$black == "yes")

# We add an instrumental variable
Schooling$ed76.IV <- fitted(lm(ed76 ~ nearc4, Schooling))

# linear model
lm.School <- lm(lwage76 ~ ed76.IV + exp76 + I(exp76^2) + black, data = Schooling)
coef(summary(lm.School))

vcm.School <- tvcglm(formula = lwage76 ~ -1 + ed76.IV + exp76 + I(exp76^2) +
                       vc(age76, momdad14, south66, south76, famed, enroll76, smsa76) +
                       vc(ed76.IV, exp76, age76, momdad14, south66, south76, famed, enroll76, smsa76,
                          by = black),
                     data = Schooling, family = gaussian(), control = control)

coef(vcm.School)
plot(vcm.School, type = "coef", part = "A") # all varying intercepts -> what are the most significant intercept differences? --< mass & age determine differences in intercepts !!!
plot(vcm.School, type = "coef", part = "B") # all varying slopes -> none!!



# Basic import MT-Data-----------------------------------------------------------------
df_mt <- imsbasics::load_rdata("df_mt.RData", "Scripts_Michi/")
var_type <- attr(df_mt, "var_type")

base_types <- c("lebenslage", "erwerbsarbeit", "carearbeit")
base_vars <- var_type$variable[var_type$type %in% base_types]
base_formula <- formula(paste("depression ~", paste(base_vars, collapse = " + ")))


# mgcv - gam (Generalized Additive model) --------------------------------------
m.gam_gaussian <- mgcv::gam(formula = base_formula, family = gaussian(), data = df_mt)
summary(m.gam_gaussian)
m.gam_poisson <- mgcv::gam(formula = base_formula, family = poisson(), data = df_mt)
summary(m.gam_poisson)


# we treat "ausbildung" + "arbeit_zeit_ueberstunden" as vc, because they are less
# relevant in m.gam_gaussian (high p-values)
mt_formula <- depression ~ alter + geschlecht + ch_nationalitaet +
  einschraenkung_weg_ges_zustand + haushaltsaequivalenzeinkommen +
  partnerschaft + tod_person + arbeit_einbezug_entscheidungen +
  arbeit_qualifikation + arbeit_zeit_wochenstunden +
  arbeit_zeit_nacht + arbeit_intensitaet + arbeit_zufriedenheit_atmosphaere +
  hausarbeit_wochenstunden + kinder_betreuung + pflege_angehoerige +
  vc(ausbildung, arbeit_zeit_ueberstunden)


mt_formula <- depression ~ vc(ausbildung, arbeit_zeit_ueberstunden)
## ---- tvcglm() on MT data ------------------------------------------------------

df <- na.omit(df_mt[,var_type$variable]) # NA-values need to be neglected --> this gives
                                         # us some problems, becaus we just erase about 50%
                                         # of all observations (53'473 -> 24'053). This happens
                                         # implicitly in most of the other models

# small tree (mindev = 2)
m.vc1 <- tvcglm(mt_formula, data = df[1:5000,], family = gaussian(),
                control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
                                         mindev = 2)) # D_min = 0 minimal log-likelihood reduction
summary(m.vc1)
plot(m.vc1, "coef")

# large tree (mindev = 0)
m.vc2 <- tvcglm(mt_formula, data = df[1:5000,], family = gaussian(),
                control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
                                         mindev = 0)) # D_min = 0 minimal log-likelihood reduction
                                                     # --> gives us the largest possible tree.

summary(m.vc2)
plot(m.vc2, "coef")


## ---- tvcolmm() on MT data ---------------------------------------------------
# this implementation needs ordered factors

df$depression <- factor(df$depression, levels = sort(unique(df_mt$depression)), ordered = TRUE)
m.vc3 <- tvcolmm(mt_formula, data = df[1:5000,], family = cumulative())

summary(m.vc3)
plot(m.vc3, "coef")
