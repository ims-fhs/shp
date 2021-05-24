# Create graphics based on the exploratory analysis for the Master-Thesis

imsbasics::clc()
library(dplyr)
library(plotly)



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
library(plm)
library(sjPlot)

m.pool_plm <- plm(base_formula, df_mt, index = c("id","year"), model="pooling")
summary(m.pool_plm)
plot_model(m.pool_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-1,1),
           title = "Effekte im gepoolten Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)
models <- rlist::list.append(models, m.pool_plm = m.pool_plm)



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
models <- rlist::list.append(models, m.fd_plm = m.fd_plm)
summary(m.fd_plm)
plot_model(m.fd_plm, show.values = TRUE, value.offset = 0.4,
           value.size = 4, axis.lim = c(-5,3),
           title = "Effekte im FD-Modell",
           axis.title = "Effektschätzung_inkl_95%-Konfidenzintervall", ci.lvl = 0.95)



# -------------------------------- 8. FE Model ---------------------------------
m.fe_plm <- plm(base_formula, df_mt, index = c("id","year"), model="within")
models <- rlist::list.append(models, m.fe_plm = m.fe_plm)
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
m.re_plm <- plm(base_formula, df_mt, index = c("id","year"), model="random")
models <- rlist::list.append(models, m.re_plm = m.re_plm)
summary(m.re_plm)
plot_model(m.re_plm, show.values = TRUE, value.offset = .5)

# fitted_data_re <- fitted(m.re_plm)
# plot(demeaned_data, cex = 0.3, main = "Abweichung für 'depression' vom einheitenspezifischen Mittelwert",
#      xaxt = "n", xlab = "Beobachtung", ylab = "Abweichung")
# points(as.numeric(fitted_data_re), col = alpha("red",0.5), cex = 0.2)
# legend(17500, 8, legend = c("Rohdaten", "Vorhersage FE-Modell"), col = c("black", "red"),
#        pch = 20, pt.cex = 2)
