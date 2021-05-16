# ------------------------------------------------------------------------------
# Author:  SLC
# Date:    16.05.2021
# Summary: Playaround with the jmcm package
#
#  See vignette: https://cran.r-project.org/web/packages/jmcm/vignettes/jss2542.pdf
# ------------------------------------------------------------------------------
library(jmcm)
library(lattice)
options(mc.cores=parallel::detectCores()) # maybe this helps to increase calculation speed...


# balanced dataset
data("cattle", package = "jmcm")
xyplot(weight ~ day | group, group = id, data = cattle, xlab = "days",ylab = "weight", col = 1, type = "l")
cattleA <- cattle[cattle$group=='A', ]

fit1 <- jmcm(weight | id | I(day/14 + 1) ~ 1 | 1, data = cattleA, triple = c(8, 3, 4), cov.method = 'mcd')
regressogram(fit1, time = 1:11)


# plot "mean" part
mean_poly_coef <- rev(fit1@opt[["beta"]]) # extract coefficients
fit1_poly <- data.frame(x = seq(1,10.5,0.1)) #
fit1_poly$y <- pracma::polyval(mean_poly_coef, fit1_poly$x)
ggplot() +
  geom_line(data = cattleA, mapping = aes(x = (day/14+1), y = weight, group = id)) +
  geom_line(data = fit1_poly, mapping = aes(x = x, y = y), col = "red", lwd = 3)


# plot "Innovation Variance" part
inno_poly_coef <- rev(fit1@opt[["lambda"]]) # extract coefficients
fit1_poly <- data.frame(x = seq(1,10.5,0.1)) #
fit1_poly$y <- pracma::polyval(inno_poly_coef, fit1_poly$x)
regressogram(fit1, time = 1:11)
plot(fit1_poly, ylim = c(2.3,4.7))



# unbalanced dtaset
data("aids", package = "jmcm")
xyplot(sqrt(cd4) ~ time, data = aids,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lines(x[aids$id==10002], y[aids$id==10002], col = 2, lwd = 2)
         panel.lines(x[aids$id==10005], y[aids$id==10005], col = 3, lwd = 2)
         panel.lines(x[aids$id==10029], y[aids$id==10029], col = 4, lwd = 2)
         panel.lines(x[aids$id==10039], y[aids$id==10039], col = 5, lwd = 2)
         panel.lines(x[aids$id==10048], y[aids$id==10048], col = 6, lwd = 2)
         panel.lines(x[aids$id==10052], y[aids$id==10052], col = 7, lwd = 2)
       },
       xlab = "Time", ylab = "CD4 cell numbers", col = 1)
fit4 <- jmcm(I(sqrt(cd4)) | id | time ~ 1 | 1, data = aids, triple = c(8, 1, 3), cov.method = 'mcd')
bootcurve(fit4, nboot = 10)




# our dataset
df_mt <- imsbasics::load_rdata("df_mt.RData", "Scripts_Michi/")
dt_mt_5 <- df_mt[df_mt$cluster == 5,]

# We try different polynoms for the mean (the innovation & autoregressive Part are set to 5)

for (i in seq(1,5)) {
  fit_df_mt <- jmcm(depression | id | year ~ 1 | 1, data = dt_mt_5,
                    triple = c(i, 5, 5), cov.method = 'mcd')
  bootcurve(fit_df_mt, nboot = 10)
}
