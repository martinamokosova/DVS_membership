library(tidyverse)
library(plyr)

library(rgdal)
library(rgeos)
library(maps)
library(sf)
library(mapproj)
library(lubridate)
library(reshape2)

library(wesanderson)

membership_data <- read.csv("D:/DVS/membership_challenge/membership_data.txt", stringsAsFactors = FALSE)
#View(membership_data)


join_dates <- strsplit(x = membership_data$date, split = "/")

membership_data$year <- as.numeric(sapply(join_dates, "[", 3))
membership_data$month <- as.numeric(sapply(join_dates, "[", 1))
membership_data$day <- as.numeric(sapply(join_dates, "[", 2))

membership_data$date2 <- make_date(year = membership_data$year, month = membership_data$month, day = membership_data$day)
  
mem_data_long <- melt(membership_data[,c(1:5, 7:10)], id.vars = c("lat", "long", "date2", "year", "month", "day"), variable.name = "area", value.name = "expertise")

mem_data_wide_avg <- dcast(data = mem_data_long, date2 + year + month + day ~ area, mean, value.var = "expertise")
mem_data_wide_count <- dcast(data = mem_data_long, date2 + year + month + day ~ area, length, value.var = "expertise")
mem_data_wide_sum <- dcast(data = mem_data_long, date2 + year + month + day ~ area, sum, value.var = "expertise")



theme_white <- theme_bw() + theme(panel.grid = element_blank(),
                                  axis.title = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.text = element_blank())

# ggplot() + geom_bin2d(membership_data, mapping = aes(x = long, y = lat), binwidth = 3) +
#   scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
#   theme_white + coord_map() +
#   geom_point(membership_data, mapping = aes(x = long, y = lat), size = 1)


ggplot() + geom_point(membership_data, mapping = aes(x = long, y = lat), size = .85) +
  #scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
  theme_white + coord_map()


ggplot() + geom_area(data = mem_data_long, mapping = aes(x = date2, y = ))