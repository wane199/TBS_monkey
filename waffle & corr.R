library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)

# current verison
packageVersion("waffle")

data.frame(
  vals = c(79, 62, 56, 37),
  col = rep(c("left", "right"), 2),
  fct = c(rep("Female", 2),
          rep("Male", 2))
) -> xdf

xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(fill = fct, values = n)) +
  geom_waffle(n_rows = 20, size = 0.397, colour = "white", flip = T) +
  scale_fill_manual(
    name = NULL,
    values = c("#c68958", "#ae6056"),
    labels = c("Female", "Male")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()
 

xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(label = fct, values = n)) +
  geom_pictogram(n_rows = 20, aes(colour = fct), flip = T, make_proportional = F) +
  scale_color_manual(
    name = NULL,
    values = c("#c68958", "#ae6056"),
    labels = c("Male", "Female")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("male", "female"),
    labels = c("Male", "Female")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.5))


xdf %>%
  count(fct, wt = vals) %>%
  ggplot(aes(label = fct, values = n)) +
  geom_pictogram(
    n_rows = 20, size = 10, aes(colour = fct), flip = TRUE,
    family = "FontAwesome5Brands-Regular"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#073f9c","#f34323"),
    labels = c("Female", "Male")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("female", "male"),
    labels = c("Female", "Male")
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.text = element_text(hjust = 0, vjust = 1))



