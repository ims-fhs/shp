# Create graphics based on the exploratory analysis for the Master-Thesis

imsbasics::clc()
library(dplyr)
library(plotly)
library(plm)
library(sjPlot)


# ------------------ Import ----------------------------------------------------

# df_mt = grosser Datensatz
df_mt <- imsbasics::load_rdata("df_mt", "Scripts_Michi/")
var_type <- attr(df_mt, "var_type")
# df = kleiner Datensatz
df <- df_mt[,colnames(df_mt) %in% attr(df_mt, "var_type")$variable]
df_complete <- na.omit(df)
# dataframes for numeric and factor variables
df_numeric <- df %>% select_if(is.numeric)
df_factor <- df %>% select(id, year, depression, where(is.factor))
# variable_names for nmeric and factor variables (independent)
vars_numeric <- colnames(df_numeric)[!colnames(df_numeric) %in% "depression"]
vars_factor <- colnames(df_factor)[!colnames(df_factor) %in% c("id", "year", "depression")]
# columns id, year, depression were manually added to df_factor
assertthat::assert_that(ncol(df) == ncol(df_numeric) + ncol(df_factor) - 3)


# create base_types, base_vars and base_formula (for modelling)
base_types <- c("lebenslage", "erwerbsarbeit", "carearbeit")
base_vars <- var_type$variable[var_type$type %in% base_types]
base_formula <- formula(paste("depression ~", paste(base_vars, collapse = " + ")))

models <- list()



# ---------------------- 1. Balanciertheit -------------------------------------

# Anzahl Id's mit X Beobachtungen für den relevanten datensatz (df)
obs_per_id <- table(df_mt$id)
my_labels <- seq(min(obs_per_id), max(obs_per_id))
my_breaks <- seq(min(obs_per_id)-0.5, max(obs_per_id)+0.5)
h1 <- hist(obs_per_id, breaks = my_breaks, plot = F)
assertthat::assert_that(sum(h1$counts) == 5694)

plot(h1, col = "grey80", xaxt = "n",
     main = "Verteilung des Beobachtungszeitraums aller Personen",
     xlab = "Beobachtungszeitraum [Anzahl Jahre]", ylab = "Anzahl Personen")
axis(1, labels = my_labels, at = my_labels)


# Anzahl Id's mit X Beobachtungen für den datensatz ohne NA's (df_complete)
obs_per_id <- table(df_complete$id)
my_labels <- seq(min(obs_per_id), max(obs_per_id))
my_breaks <- seq(min(obs_per_id)-0.5, max(obs_per_id)+0.5)
h2 <- hist(obs_per_id, breaks = my_breaks, plot = F)
assertthat::assert_that(sum(h2$counts) == 4290)

plot(h2, col = "grey80", xaxt = "n",
     main = "Verteilung des Beobachtungszeitraums aller Personen",
     xlab = "Beobachtungszeitraum [Anzahl Jahre]", ylab = "Anzahl Personen")
axis(1, labels = my_labels, at = my_labels)


# -------------------------- 2.NA's --------------------------------------------

NAs_per_column <- data.frame(na_absolute = unlist(map(df, ~sum(is.na(.)))),
                             na_relative_percent = 100*round(unlist(map(df, ~sum(is.na(.))/nrow(df))),3))
knitr::kable(NAs_per_column, col.names = c("Absolute Anzahl NA's", "Relativer Anteil NA's [%]")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))



# -------------------------- 3. Verteilung variablen ---------------------------
p3 <- as.data.frame(df) %>%
  select_if(is.numeric) %>%
  pivot_longer(-id, names_to = "variablen", values_to = "daten") %>%
  ggplot() +
  geom_bar(aes(y = daten)) +
  # geom_histogram(aes(y = daten)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~variablen, scales = "free", ncol = 3)

ggsave(p3, filename = "Scripts_Michi/graphics/exp_3_verteilung_numerisch.png", device = "png",
       width = 20,height = 30, units = "cm")


p4 <- as.data.frame(df) %>%
  select(matches("id") | where(is.factor)) %>%
  pivot_longer(-id, names_to = "variablen", values_to = "daten") %>%
  ggplot() +
  geom_bar(aes(y = daten)) + # geom_bar(aes(y = daten)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~variablen, scales = "free", ncol = 3)
ggsave(p4, filename = "Scripts_Michi/graphics/exp_3_verteilung_faktor.png", device = "png",
       width = 20,height = 30, units = "cm")

# ------------------------------ 3. zeitlicher Verlauf Depression --------------

ind_add_NA <- which(!duplicated(df$id))[1:25]
ind_add_NA[1] <- 0
df_dummy <- berryFunctions::insertRows(as.data.frame(df[1:max(ind_add_NA),]), r = ind_add_NA,
                                       new= NA, rcurrent = TRUE)
id_lookup <- data.frame(old = sort(unique(df_dummy$id)),
                        new = seq(1,length(sort(unique(df_dummy$id)))))
for (i in id_lookup$old) {
  df_dummy$id[df_dummy$id == i] <- id_lookup$new[id_lookup$old ==i]
}

my_colors <- c("green4", "green1", "yellow","darkorange", "red", "darkred", "darkviolet")

p5 <- plot_ly(df_dummy, x = ~year, y = ~id, z =~depression,
        type = "scatter3d", mode = "lines",
        color = ~depression,
        colors = colorRamp(my_colors),
        size = I(4)) %>%
  layout(title = "zeitliche Entwicklung von `depression` der ersten 25 Personen",
         scene = list(camera = list(
           eye = list(x = -3, y = -4, z = 2),
           # up = list(x = 0, y = 0, z = 1)
           center = list(x = 0, y = 0, z = -0.5)
         ),
         aspectmode = "manual", aspectratio = list(x=1, y=5, z=1)
         ))
p5
rm(df_dummy)



# ------------------------------ 4. Korrelationen ------------------------------

dep_covariance <- var(df_numeric, na.rm = TRUE, use = "pairwise.complete.obs") # calculate variances & covariances
dep_correaltion <- cov2cor(dep_covariance)
corrplot::corrplot(dep_correaltion, method = "square", cl.pos = "r",
                   tl.col = "black")

corrplot::corrplot(dep_correaltion, method = "number", cl.pos = "r",
                   tl.col = "black", number.cex = 1.2)




# ------------------------------- 5. Pooled model ------------------------------

m.pool_plm <- plm(base_formula, df_mt, index = c("id","year"), model="pooling")
summary(m.pool_plm)
plot_model(m.pool_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-1,1),
           title = "Effekte im gepoolten Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)
models <- rlist::list.append(models, Pooled = m.pool_plm)



# ------------------------------- 6. variable coeff model ----------------------

# df_pvcm <- df_mt %>%
#   filter(!is.na(depression)) %>% # abhängige Variable sollte nicht NA sein
#   filter(!is.na(einschraenkung_weg_ges_zustand)) %>% # unabhängige Variable sollte nicht NA sein
#   group_by(id) %>%
#   filter(n() >= 3) # unabhängige variable sollte mind. n_obs_needed mal vorkommen
#
#
# m.pvcm <- pvcm(depression ~ einschraenkung_weg_ges_zustand, df_pvcm, model = "within")
# my_coefs <- coef(m.pvcm)[,2]
# summary(m.pvcm)


# ------------------------------7. FD Model ------------------------------------
m.fd_plm <- plm(base_formula, df_mt, index = c("id","year"), model="fd")
models <- rlist::list.append(models, FD = m.fd_plm)
summary(m.fd_plm)
plot_model(m.fd_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-5,3),
           title = "Effekte im FD-Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)



# -------------------------------- 8. FE Model ---------------------------------
m.fe_plm <- plm(base_formula, df_mt, index = c("id","year"), model="within")
models <- rlist::list.append(models, FE = m.fe_plm)
summary(m.fe_plm)
plot_model(m.fe_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-2,2),
           title = "Effekte im FE-Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)

demeaned_data <- df_complete %>%
  group_by(id) %>%
  mutate(mean_dep = mean(depression), dep_new = depression - mean_dep) %>%
  pull(dep_new)
fitted_data_fe <- fitted(m.fe_plm)

plot(demeaned_data, cex = 0.3, main = "Abweichung für 'depression' vom einheitenspezifischen Mittelwert",
     xaxt = "n", xlab = "Beobachtung", ylab = "Abweichung")
points(as.numeric(fitted_data_fe), col = alpha("red",0.5), cex = 0.2)
legend(17500, 8, legend = c("Rohdaten", "Vorhersage FE-Modell"), col = c("black", "red"),
       pch = 20, pt.cex = 2)



# -------------------------------- 9. RE Model ---------------------------------
set.seed(2)
x_re <- seq(4,19); n_re <- length(x_re)
sd_small <- 0.2; sd_big <- 2

re_small_eit <- data.frame(x = x_re,
                           y1 = 2 + rnorm(n_re, sd = sd_small),
                           y2 = 5 + rnorm(n_re, sd = sd_small),
                           y3 = 8 + rnorm(n_re, sd = sd_small))
re_big_eit <- data.frame(x = x_re,
                           y1 = 2 + runif(n_re, min = -2, max = 5),
                           y2 = 4 + runif(n_re, min = -4, max = 6),
                           y3 = 6 + runif(n_re, min = -6, max = 4))


par(mfrow = c(1,2))
plot(re_small_eit$x, re_small_eit$y1, ylim = c(0,10), type = "l",
     main = latex2exp::TeX("$var(e_{it})$ klein, $var(u_i)$ gross $\\Rightarrow$
                           $\\lambda_i \\rightarrow$ 1"),
     xlab = "Zeit", ylab = "depression", lwd = 2)
lines(re_small_eit$x, re_small_eit$y2, col = "blue", lwd = 2)
lines(re_small_eit$x, re_small_eit$y3, col = "darkorange", lwd = 2)
abline(h = mean(re_small_eit$y1))
abline(h = mean(re_small_eit$y2), col = "blue")
abline(h = mean(re_small_eit$y3), col = "darkorange")

plot(re_big_eit$x, re_big_eit$y1, ylim = c(0,10), type = "l",
     main = latex2exp::TeX("$var(e_{it})$ gross, $var(u_i)$ klein $\\Rightarrow$
                           $\\lambda_i \\rightarrow$ 0"),
     xlab = "Zeit", ylab = "depression", lwd = 2)
lines(re_big_eit$x, re_big_eit$y2, col = "blue", lwd = 2)
lines(re_big_eit$x, re_big_eit$y3, col = "darkorange", lwd = 2)
abline(h = mean(re_big_eit$y1))
abline(h = mean(re_big_eit$y2), col = "blue")
abline(h = mean(re_big_eit$y3), col = "darkorange")




m.re_plm <- plm(base_formula, df_mt, index = c("id","year"), model="random")
models <- rlist::list.append(models, RE = m.re_plm)
summary(m.re_plm)
plot_model(m.re_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-1,1),
           title = "Effekte im RE-Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)

# fitted_data_re <- fitted(m.re_plm, effect = "time")
# plot(demeaned_data, cex = 0.3, main = "Abweichung für 'depression' vom einheitenspezifischen Mittelwert",
#      xaxt = "n", xlab = "Beobachtung", ylab = "Abweichung")
# points(as.numeric(fitted_data_re), col = alpha("red",0.5), cex = 0.2)
# legend(17500, 8, legend = c("Rohdaten", "Vorhersage FE-Modell"), col = c("black", "red"),
#        pch = 20, pt.cex = 2)



# --------------- 10. RE-KV Model ----------------------------------------------

# group-mean centering for all variables exept "id" and "year"
vars_mc <- colnames(df %>% select_if(is.numeric))
vars_mc <- vars_numeric[! vars_numeric %in% c("id", "year", "depression", "cluster", "person_haushalt")]
vars_mc_formula <- c()
for (i in vars_mc) {
  vars_mc_formula <- c(vars_mc_formula, paste0(i,"_mn"), paste0(i, "_dev"))
}
# Mean centered dataset!
df_mt_mc <- rockchalk::gmc(df_mt, vars_mc, by = "id")

# create formula for group-meaned model.
vars_not_mc <- base_vars[!base_vars %in% vars_mc]
assertthat::assert_that(length(vars_mc) + length(vars_not_mc) == length(base_vars))


formula_mc <- formula(paste("depression ~",
                            paste(vars_not_mc, collapse = " + "), " + ",
                            paste(vars_mc_formula, collapse = " + ")))
print("Mean centered formula for mean-centered data:")
print(formula_mc)


m.re_kv_plm <- plm(formula_mc, df_mt_mc, index = c("id","year"), model="random")
models <- rlist::list.append(models, RE_KV = m.re_kv_plm)
summary(m.re_kv_plm)
plot_model(m.re_kv_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-1,1),
           title = "Effekte im RE-KV-Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)

# Hausmann-Test for demeaned-data
phtest(formula_mc, data = df_mt_mc)



# -------------- 11. Instrumental Variables -----------------------------------
instr_vars <- c(1,2,3,4,6,10,11,13,16,17,18)
instrumental_formula2 <- formula(paste("depression ~",
                                       paste(base_vars[-instr_vars], collapse = " + "),  " | ",
                                       paste(base_vars[instr_vars], collapse = " + ")))

print(instrumental_formula2)


m.fe_plm_inst2 <- plm(instrumental_formula2, df_mt, index = c("id","year"), model="within")
models <- rlist::list.append(models, Instrumental = m.fe_plm_inst2)
summary(m.fe_plm_inst2)
plot_model(m.fe_plm_inst2, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-1,1),
           title = "Effekte basierend auf instrumentellen Varaiblen",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)



# ---------------- 12. kml, kmlShape, kml3d ---------------------------------------------
library(tidyr)
library(kml)
library(kml3d)
library(kmlShape)


# 2d-Cluster only on depression (k = 5)
set.seed(1)
df_kml <- df_mt[1:3]
df_kml <- pivot_wider(df_kml, names_from = year, values_from = depression)
cluster <- clusterLongData(as.matrix(df_kml[c(2:ncol(df_kml))]))
kml(cluster, nbClusters = 5, nbRedrawing = 1, toPlot='none')
plot(cluster, 5, adjustLegend=-0.15, toPlot = "traj", ylab = "depression", xlab = "year")



# # 2d-cluster with kmlShape instead of kml -> Does not work !! Session abort
set.seed(1)
df_kml_shape <- df_mt[1:3]
df_kml_shape <- pivot_wider(df_kml_shape, names_from = year, values_from = depression)
# df_kml_shape <- as.data.frame(df_kml_shape[complete.cases(df_kml_shape),]) # -> gives longer clusters (otherwise they are short)
df_kml_shape <- df_kml_shape[1:300,] # -> time issues
cluster_kml_shape <- cldsWide(trajWide = as.matrix(df_kml_shape[,c(2:ncol(df_kml_shape))]),
                              id = as.numeric(df_kml_shape$id))

plot(cluster_kml_shape)
kmlShape(cluster_kml_shape, 5, toPlot = 'none')
plot(cluster_kml_shape, ylab = "depression", xlab = "year")




# # check all 3d-Clusters for depression + any numeric variable (k = 5)
# # -> hausarbeit_wochenstunden has an interesting pattern
# # -> alter has an interesting pattern
# for (i in colnames(df_numeric)[4:ncol(df_numeric)]) {
#   df_kml3d <- df_mt
#   my_columns <- c("id", "year", "depression", i)
#   df_kml3d <- pivot_wider(df_kml3d[,my_columns], names_from = year,
#                           values_from = c(depression,my_columns[4]))
#   df_kml3d <- as.data.frame(df_kml3d)
#   cluster3d <- cld3d(df_kml3d, timeInData=list(18:33, 2:17),
#                      varNames = c(my_columns[4], "depression"))
#   kml3d(cluster3d,5,nbRedrawing=2,toPlot="none")
#   plotMeans3d(cluster3d,5)
# }


# cluster for alter (5 cluster & 8 cluster sind interessant)
set.seed(1)
i <- "alter"
df_kml3d <- df_mt
my_columns <- c("id", "year", "depression", i)
df_kml3d <- pivot_wider(df_kml3d[,my_columns], names_from = year,
                        values_from = c(depression,my_columns[4]))
df_kml3d <- as.data.frame(df_kml3d)
cluster3d <- cld3d(df_kml3d, timeInData=list(18:33, 2:17),
                   varNames = c(my_columns[4], "depression"))
# kml3d(cluster3d,3:10,nbRedrawing=2,toPlot="none")
kml3d(cluster3d,5,nbRedrawing=2,toPlot="none")
# plotMeans3d(cluster3d,3)
# plotMeans3d(cluster3d,4)
plotMeans3d(cluster3d,5) # plotTraj3d(cluster3d,5)
# plotMeans3d(cluster3d,6)
# plotMeans3d(cluster3d,7)
# plotMeans3d(cluster3d,8)
# plotMeans3d(cluster3d,9)
# plotMeans3d(cluster3d,10)



# # cluster for hausarbeit_wochenstunden (nichts wirklich  interessant)
# set.seed(1)
# i <- "hausarbeit_wochenstunden"
# df_kml3d <- df_mt
# my_columns <- c("id", "year", "depression", i)
# df_kml3d <- pivot_wider(df_kml3d[,my_columns], names_from = year,
#                         values_from = c(depression,my_columns[4]))
# df_kml3d <- as.data.frame(df_kml3d)
# cluster3d <- cld3d(df_kml3d, timeInData=list(18:33, 2:17),
#                    varNames = c(my_columns[4], "depression"))
# kml3d(cluster3d,3:10,nbRedrawing=2,toPlot="none")
# plotMeans3d(cluster3d,3)
# plotMeans3d(cluster3d,4)
# plotMeans3d(cluster3d,5)
# plotMeans3d(cluster3d,6)
# plotMeans3d(cluster3d,7)
# plotMeans3d(cluster3d,8)
# plotMeans3d(cluster3d,9)
# plotMeans3d(cluster3d,10)


# ---------------- 13. jmcm  - see RMD & HTML ----------------------------------
# @MICHI: Grafiken direkt aus HTML kopiert!
#
#
# library(ggplot2)
# ggplot(df_mt, aes(x = year, y = depression, group = id)) + geom_line()
#
#
# library(jmcm)
# dt_mt_5 <- df_mt[df_mt$cluster == 5,]
#
# # MCD --> AR-coefficients
# m.jmcm_mcd <- jmcm(depression | id | year ~ 1 | 1, data = dt_mt_5,
#                    triple = c(5, 5, 5), cov.method = 'mcd')
# bootcurve(m.jmcm_mcd, nboot = 10)
#
# # ACD --> MA-Coefficients
# m.jmcm_acd <- jmcm(depression | id | year ~ 1 | 1, data = dt_mt_5,
#                    triple = c(5, 5, 5), cov.method = 'acd')



# ------------- 14. vcrpart ----------------------------------------------------
library(vcrpart)
df_without_na <- na.omit(df_mt[,var_type$variable])


# Gaussian - Only two varying Intercepts
f1_vc <- depression ~ -1 + vc(ausbildung, arbeit_zeit_ueberstunden)
print(f1_vc)
m.glm1_small <- tvcglm(f1_vc, data = df_without_na, family = gaussian(),
                       control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
                                                mindev = 30, cv = FALSE)) # D_min = 0 minimal log-likelihood reduction
plot(m.glm1_small, "coef")
summary(m.glm1_small)


# glm on base formula
m.glm_ref <- glm(base_formula, data = df_without_na, family = gaussian())
summary(m.glm_ref)
models <- rlist::list.append(models, GLM_ref = m.glm_ref)


# complete model with 1 varying intercept & 1 varying slope for moderator "ausbildung"
f3_vc <- update(base_formula,  ~ . + vc(ausbildung) + vc(ausbildung, by = arbeit_zeit_wochenstunden))
cat("f3: "); print(f3_vc)
m.glm3_large <- tvcglm(f3_vc, data = df_without_na, family = gaussian(),
                       control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
                                                mindev = 0, cv = FALSE)) # D_min = 0 minimal log-likelihood reduction
plot(m.glm3_large, "coef", part = "A")
plot(m.glm3_large, "coef", part = "B")
summary(m.glm3_large)

# complete model with 1 varying intercept & 1 varying slope for moderator "hausarbeit wochenstunden"
f4_vc <- update(base_formula,  ~ . + vc(hausarbeit_wochenstunden) +
                  vc(hausarbeit_wochenstunden, by = arbeit_zeit_wochenstunden))
cat("f4: "); print(f4_vc)
m.glm4_large <- tvcglm(f4_vc, data = df_without_na, family = gaussian(),
                       control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
                                                mindev = 0.5, cv = FALSE)) # D_min = 0 minimal log-likelihood reduction
plot(m.glm4_large, "coef", part = "A")
plot(m.glm4_large, "coef", part = "B", gp = gpar(fontsize=8), tnex = 3)
summary(m.glm4_large)


# complete model with 1 varying intercept & 1 varying slope for moderators "ausbildung" & hausarbeit wochenstunden"
f5_vc <- update(base_formula,  ~ . + vc(partnerschaft) +
                  vc(ausbildung, partnerschaft, by = arbeit_zeit_wochenstunden))
cat("f5: "); print(f5_vc)
m.glm5_large <- tvcglm(f5_vc, data = df_without_na, family = gaussian(),
                       control = tvcglm_control(minsize = 30, # N_0 = 30 minimal number of nodes.
                                                mindev = 0, cv = FALSE)) # D_min = 0 minimal log-likelihood reduction
plot(m.glm5_large, "coef", part = "A")
plot(m.glm5_large, "coef", part = "B",  gp = gpar(fontsize=10), tnex = 3)
summary(m.glm5_large)





# ------- Modelsummary --------------


modelsummary(models, # models[c(1,2,3,4,5)]
             estimate = "{estimate}{stars}",
             statistic = "({std.error})", # "(Std: {std.error} / p: {p.value})",
             stars = FALSE) %>%
  column_spec(c(5,6), background = '#E5FFCC') %>%
  column_spec(c(7,8,9), background = '#CCFFFF')
