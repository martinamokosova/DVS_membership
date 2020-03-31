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
# membership_data$week <- lubridate::week(membership_data$date2)
membership_data$week_com <- floor_date(membership_data$date2, unit = "week")

  
mem_data_long <- melt(membership_data[,c(1:5, 7:length(membership_data))], id.vars = c("lat", "long", "date2", "year", "month", "day", "week_com"), 
                      variable.name = "area", value.name = "expertise")

mem_data_wide_avg <- dcast(data = mem_data_long, week_com ~ area, mean, value.var = "expertise")
mem_data_wide_count <- dcast(data = mem_data_long, week_com ~ area, length, value.var = "expertise")
mem_data_wide_sum <- dcast(data = mem_data_long, week_com ~ area, sum, value.var = "expertise")

mem_data_wide_sum$data_cumulative <- (mutate(group_by(mem_data_wide_sum, week_com), cumulative = cumsum(data))[,5])
mem_data_wide_sum$visualization_cumulative <- (mutate(group_by(mem_data_wide_sum, week_com), cumulative = cumsum(visualization))[,5])
mem_data_wide_sum$society_cumulative <- (mutate(group_by(mem_data_wide_sum, week_com), cumulative = cumsum(society))[,5])



mem_data_long_avg <- melt(mem_data_wide_avg[,1:4], id.vars = c("week_com"), variable.name = "area", value.name = "expertise")
mem_data_long_count <- melt(mem_data_wide_count[,1:4], id.vars = c("week_com"), variable.name = "area", value.name = "expertise")
mem_data_long_sum <- melt(mem_data_wide_sum[,1:4], id.vars = c("week_com"), variable.name = "area", value.name = "expertise")

mem_data_long_cumulative_sum <- melt(mem_data_wide_sum[,c(1,5:7)], id.vars = c("week_com"), 
                                     variable.name = "area", value.name = "expertise")

ncol(mem_data_wide_sum)
mem_data_wide_sum[, c(1, 5:7)]

theme_white <- theme_bw() + theme(panel.grid = element_blank(),
                                  axis.title = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.text = element_blank())

dvs_colour <- c("#368d90", "#A35685", "#E8BF12")


# ggplot() + geom_bin2d(membership_data, mapping = aes(x = long, y = lat), binwidth = 3) +
#   scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
#   theme_white + coord_map() +
#   geom_point(membership_data, mapping = aes(x = long, y = lat), size = 1)


ggplot() + geom_point(membership_data, mapping = aes(x = long, y = lat), size = .85) +
  #scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
  theme_white + coord_map()


ggplot() + geom_area(data = mem_data_long_avg, mapping = aes(x = week_com, y = expertise, fill = area), colour = NA) +
  scale_fill_manual(values = dvs_colour) +
  theme_bw()
