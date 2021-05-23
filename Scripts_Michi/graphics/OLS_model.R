# plot a OLS model with plotly

imsbasics::clc()
library(dplyr)
library(plotly)
library(plm)


# data for time lines
x_range <- seq(0,5)
y_range <- seq(1,10,0.5)
pal <- c("brown1", "orange", "gold", "green", "cyan", "magenta")

set.seed(1)
empty_row <- data.frame(x = NA, y = NA, z = NA, col = NA)
df <- empty_row
for (i in x_range) {
  my_df <- expand.grid(y = y_range, x = i)
  my_df <- my_df[,c(2,1)]
  my_df$z <- (2 + max(x_range) - i) + runif(length(y_range),-2,2) + runif(1,0,0.5)*my_df$y
  my_df$col <- rep(pal[i+1], length(y_range))
  df <- rbind(df, my_df, empty_row)
  rm(my_df)
}

# create the pooled model based on the data
df$dummy_x <- df$x; df$dummy_y <- df$y # create dummy x & y (copy of x & y) for plm
                                       # -> x&y = index / dummies = independent variables
lm_ols <- lm(z ~ dummy_y, data = df[!is.na(df$x),])

lm_resids <- residuals(lm_ols)
my_resids <- rep(NA, nrow(df))
my_resids[as.numeric(names(lm_resids))] <- lm_resids
df$z_modeled <- df$z - my_resids

# create data for surface
z <- matrix(df$z_modeled[!is.na(df$z_modeled)], ncol = length(x_range))
# add surface data to df (for plotting lines)
z_modeled <- numeric()
for (i in 1:ncol(z)) {
  z_modeled <- c(z_modeled, NA, z[,i])
}
z_modeled <- c(z_modeled, NA)
df$z_modeled <- z_modeled


for (i in seq(1,length(x_range))) {
  assign(x = paste0("df_",i),
         value = df[df$x == i-1 & !is.na(df$x),])
}



p1 <- plot_ly()
for (i in seq(1,length(x_range))) { # add the lines with separate colors
  p1 <- p1 %>%
    add_trace(data = get(paste0("df_",i)), x = ~y, y = ~z,
              type = "scatter", mode = "markers",
              color = I("black"),
              marker = list(size = 14)) %>%
    add_trace(data = get(paste0("df_",i)), x = ~y, y = ~z,
              type = "scatter", mode = "lines",
              color = I(pal[i]),
              line = list(width = 1))
}
p1 <- p1 %>%
  add_trace(data = df, x = ~y, y = ~z_modeled, # add regression line
            type = "scatter", mode = "lines",
            color = I("grey50"),
            line = list(width = 7)) %>%
  layout(title = "OLS Modell: Gewöhnliche Regressionsgerade",
         xaxis = list(title = "x_i", titlefont = list(size = 45), tickfont = list(size = 25)),
         yaxis = list(title = "y_i", titlefont = list(size = 45), tickfont = list(size = 25)))

print(p1)


p2 <- plot_ly() %>%
  add_surface(x = ~x_range,
              y = ~y_range,
              z = ~z,
              opacity = 0.5,
              colorscale = list(c(0, 1), c("black", "black"))) %>%
  add_trace(inherit = FALSE,
            data = df, x = ~x, y = ~y, z =~z,
            type = 'scatter3d',
            mode = "lines", # 'lines+markers',
            color = I(df$col),
            line = list(width = 7)) %>%
  add_trace(inherit = FALSE,
            data = df, x = ~x, y = ~y, z =~z_modeled,
            type = 'scatter3d',
            mode = "lines + markers", # 'lines+markers',
            color = I(df$col),
            line = list(width = 4),
            marker = list(size = 4)) %>%
  layout(title = "RE Modell: (n-1)-dim Hyperebene im n-dim Raum",
         scene = list(xaxis = list(title = "Untersuchungseinheit", titlefont = list(size = 25), tickfont = list(size = 15)),
                      yaxis = list(title = "x_i", titlefont = list(size = 25), tickfont = list(size = 15)),
                      zaxis = list(title = "y_i", titlefont = list(size = 25), tickfont = list(size = 15)),
                      aspectmode = "manual",
                      aspectratio = list(x=0.5, y=1, z=0.5),
                      camera = list(eye = list(x = 1, y = 0.8, z = 0.5),
                                    # up = list(x = 0, y = 0, z = 1)
                                    center = list(x = 0, y = 0, z = -0.1))
         )
  )



print(p2)




