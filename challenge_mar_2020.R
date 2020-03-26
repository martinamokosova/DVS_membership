library(tidyverse)
library(plyr)

library(rgdal)
library(rgeos)
library(maps)
library(sf)
library(mapproj)

library(wesanderson)

membership_data <- read.csv("D:/DVS/membership_challenge/membership_data.txt", stringsAsFactors = FALSE)
View(membership_data)

theme_white <- theme_bw() + theme(panel.grid = element_blank())

ggplot() + geom_bin2d(membership_data, mapping = aes(x = long, y = lat), binwidth = 3) +
  scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
  theme_white + coord_map() +
  geom_point(membership_data, mapping = aes(x = long, y = lat), size = 1)


ggplot() + geom_point(membership_data, mapping = aes(x = long, y = lat), size = .85) +
  #scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
  theme_white + coord_map()
