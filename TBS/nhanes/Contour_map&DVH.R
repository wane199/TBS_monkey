# 等高线图（contour map）（https://ggplot2.tidyverse.org/reference/geom_contour.html）
# Basic plot
v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour()

# Or compute from raw data
ggplot(faithful, aes(waiting, eruptions)) +
  geom_density_2d()

# \donttest{
# use geom_contour_filled() for filled contours
v + geom_contour_filled()

# Setting bins creates evenly spaced contours in the range of the data
v + geom_contour(bins = 3)

v + geom_contour(bins = 5)

# Setting binwidth does the same thing, parameterised by the distance
# between contours
v + geom_contour(binwidth = 0.01)

v + geom_contour(binwidth = 0.001)

# Other parameters
v + geom_contour(aes(colour = after_stat(level)))

v + geom_contour(colour = "red")

v + geom_raster(aes(fill = density)) +
  geom_contour(colour = "white")


library(MASS)
Sigma <- matrix(c(1, 0.7, 0.7, 1), 2, 2)
Sigma
r <- mvrnorm(n = 1000, c(0, 3), Sigma)
par(mfrow = c(2, 2))
# density plot
plot(density(r))
# kernel density estimate
bivn.kde <- kde2d(r[, 1], r[, 2], n = 50)
# perspective plot
persp(bivn.kde, phi = 45, theta = 30)
# contour plot
contour(bivn.kde)
# contour plot with image
image(bivn.kde, col = terrain.colors(100))
contour(bivn.kde, add = T)

contour(bivn.kde, col = "red", drawlabel = FALSE, main = "Density estimation :cont Plot")

# 三维透视图亦可以很清晰的表示。
persp(bivn.kde, main = "Density estimation:perspective plot")


# 3D Surface Plots in R（https://plotly.com/r/3d-surface-plots/）
library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig

fig <- plot_ly(z = ~volcano) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig

library(plotly)

fig <- plot_ly() 
# fig <- fig %>% add_trace( ... )
# fig <- fig %>% layout( ... ) 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)





###########################
# 等剂量图（isodose map）














