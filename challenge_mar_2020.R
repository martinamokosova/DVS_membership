install.packages("VennDiagram")

library(tidyverse)
library(plyr)

library(rgdal)
library(rgeos)
library(maps)
library(sf)
library(mapproj)
library(lubridate)
library(reshape2)
library(gganimate)
# library(ggVennDiagram)
library(VennDiagram)

membership_data <- read.csv("membership_data.txt", stringsAsFactors = FALSE)
#View(membership_data)

membership_data$expertise <- case_when(
  membership_data$data > membership_data$visualization & membership_data$data > membership_data$society ~ "data",
  membership_data$visualization > membership_data$data & membership_data$visualization > membership_data$society ~ "visualization",
  membership_data$society > membership_data$data & membership_data$society > membership_data$visualization ~ "society",
  membership_data$data == membership_data$visualization & membership_data$data > membership_data$society ~ "data+viz",
  membership_data$visualization > membership_data$data & membership_data$visualization == membership_data$society ~ "viz+soc",
  membership_data$society == membership_data$data & membership_data$society > membership_data$visualization ~ "soc+data",
  membership_data$society == membership_data$data & membership_data$society == membership_data$visualization ~ "all equal"
)

membership_data$V_data <- case_when(
  membership_data$data >= membership_data$visualization & membership_data$data >= membership_data$society ~ membership_data$data)
membership_data$V_viz <- case_when(
  membership_data$visualization >= membership_data$data & membership_data$visualization >= membership_data$society ~ membership_data$visualization)
membership_data$V_soc <- case_when(
  membership_data$society >= membership_data$data & membership_data$society >= membership_data$visualization ~ membership_data$society)
venn_membership <- membership_data[, c("V_data", "V_viz", "V_soc")]

membership_data$V2_data <- case_when(
  membership_data$data >= membership_data$visualization & membership_data$data >= membership_data$society ~ rownames(membership_data))
membership_data$V2_viz <- case_when(
  membership_data$visualization >= membership_data$data & membership_data$visualization >= membership_data$society ~ rownames(membership_data))
membership_data$V2_soc <- case_when(
  membership_data$society >= membership_data$data & membership_data$society >= membership_data$visualization ~ rownames(membership_data))
venn_membership2 <- membership_data[, c("V2_data", "V2_viz", "V2_soc")]



join_dates <- strsplit(x = membership_data$date, split = "/")

membership_data$year <- as.numeric(sapply(join_dates, "[", 3))
membership_data$month <- as.numeric(sapply(join_dates, "[", 1))
membership_data$day <- as.numeric(sapply(join_dates, "[", 2))

membership_data$date2 <- make_date(year = membership_data$year, month = membership_data$month, day = membership_data$day)
# membership_data$week <- lubridate::week(membership_data$date2)
membership_data$week_com <- floor_date(membership_data$date2, unit = "week")
membership_data$month_com <- floor_date(membership_data$date2, unit = "month")

  
mem_data_long <- melt(membership_data, id.vars = c("lat", "long", "date2", "year", "month", "day", "week_com", "month_com"), 
                      measure.vars = c("data", "visualization", "society"),
                      variable.name = "area", value.name = "expertise")

# -------------------------
mem_data_long$v <-"1"
membership_AVG <- dcast(data = mem_data_long, v ~ area, mean, value.var = "expertise")
mem_data_long <- mem_data_long[, c(1:(length(mem_data_long)-1))]
membership_AVG <- membership_AVG[, c(2:length(membership_AVG))]
# -------------------------


mem_data_wide_avg <- dcast(data = mem_data_long, week_com + month_com ~ area, mean, value.var = "expertise")
mem_data_wide_count <- dcast(data = mem_data_long, week_com + month_com ~ area, length, value.var = "expertise")
mem_data_wide_sum <- dcast(data = mem_data_long, week_com + month_com ~ area, sum, value.var = "expertise")

mem_data_wide_avg_m <- dcast(data = mem_data_long, month_com ~ area, mean, value.var = "expertise")
mem_data_wide_avg_w <- dcast(data = mem_data_long, week_com ~ area, mean, value.var = "expertise")
mem_data_wide_min_m <- dcast(data = mem_data_long, month_com ~ area, min, value.var = "expertise")
mem_data_wide_min_w <- dcast(data = mem_data_long, week_com ~ area, min, value.var = "expertise")

mem_data_wide_count_d <- dcast(data = mem_data_long, date2 + week_com + month_com ~ area, length, value.var = "expertise")
mem_data_wide_sum_m <- dcast(data = mem_data_long, month_com ~ area, sum, value.var = "expertise")


cumulative_d <- (mutate(group_by(mem_data_wide_count_d, date2), members_cumulative = cumsum(data)))

cumulative_w <- (mutate(group_by(mem_data_wide_sum, week_com), cumulative_data = cumsum(data)))
cumulative_w <- (mutate(group_by(cumulative_w, week_com), cumulative_viz = cumsum(visualization)))
cumulative_w <- (mutate(group_by(cumulative_w, week_com), cumulative_soc = cumsum(society)))

cumulative_m <- (mutate(group_by(mem_data_wide_sum_m, month_com), cumulative_data = cumsum(data)))
cumulative_m <- (mutate(group_by(cumulative_m, month_com), cumulative_viz = cumsum(visualization)))
cumulative_m <- (mutate(group_by(cumulative_m, month_com), cumulative_soc = cumsum(society)))


mem_data_long_avg <- melt(mem_data_wide_avg, id.vars = c("week_com", "month_com"), variable.name = "area", value.name = "expertise")
mem_data_long_count <- melt(mem_data_wide_count, id.vars = c("week_com", "month_com"), variable.name = "area", value.name = "expertise")
mem_data_long_sum <- melt(mem_data_wide_sum, id.vars = c("week_com", "month_com"), variable.name = "area", value.name = "expertise")


mem_data_long_avg_m <- melt(mem_data_wide_avg_m, id.vars = c("month_com"), variable.name = "area", value.name = "expertise")
mem_data_long_avg_w <- melt(mem_data_wide_avg_w, id.vars = c("week_com"), variable.name = "area", value.name = "expertise")
cumulative_long_w <- melt(cumulative_w, id.vars = c("week_com", "month_com"), 
                                     measure.vars = c("cumulative_data", "cumulative_viz", "cumulative_soc"), 
                                     variable.name = "area", value.name = "expertise")


cumulative_w[,c(1:2, 4:8)]

ncol(mem_data_wide_sum)
mem_data_wide_sum[, c(1, 5:7)]



theme_map <- theme_bw() + theme(panel.grid = element_blank(),
                                  axis.title = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.text = element_blank())

theme_white <- theme_bw() + theme(panel.grid = element_blank(),
                                  axis.title = element_blank(),
                                  axis.ticks = element_blank())



dvs_colour <- c("#368d90", "#A35685", "#E8BF12")

colour_data <- dvs_colour[1]
colour_viz <- dvs_colour[2]
colour_soc <- dvs_colour[3]
colour_dataviz <- colorRampPalette(c(colour_data, colour_viz))(3)[2]
colour_vizsoc <- colorRampPalette(c(colour_viz, colour_soc))(3)[2]
colour_socdata <- colorRampPalette(c(colour_soc, colour_data))(3)[2]
colour_all <- "#333333"

dvs_colours <- c("data" = colour_data,
                 "visualization" = colour_viz,
                 "society" = colour_soc,
                 "data+viz" = colour_dataviz,
                 "viz+soc" = colour_vizsoc,
                 "soc+data" = colour_socdata,
                 "all equal" = colour_all)


  
  # ggplot() + geom_bin2d(membership_data, mapping = aes(x = long, y = lat), binwidth = 3) +
#   scale_fill_viridis_c(option = "inferno", begin = .9, end = .1) +
#   theme_white + coord_map() +
#   geom_point(membership_data, mapping = aes(x = long, y = lat), size = 1)


# ggplot() + geom_area(data = cumulative_long_w, mapping = aes(x = week_com, y = expertise, fill = area), colour = NA) +
#   scale_fill_manual(values = dvs_colour) +
#   theme_white


map <- ggplot() +
  geom_point(membership_data, mapping = aes(x = long, y = lat, colour = expertise, size = date2, alpha = date2)) +
  scale_colour_manual(values = dvs_colours, breaks = names(dvs_colours)) +  
  scale_size_date(range = c(.5, 3)) + scale_alpha_date(range = c(1, .2)) + 
  guides(size = FALSE, alpha = FALSE, colour = FALSE) +
  theme_map + coord_map() +
  enter_grow(size = 0) +
  transition_time(date2) +
  shadow_mark() +
  ggtitle("{frame_time}")

animate(map, 
        width = 900, # 900px wide
        height = 480, # 600px high
        nframes = 200, # 200 frames
        fps = 10) # 10 frames per second

anim_save("dvs_map.gif")
# class(format(membership_data$date2, "%d %B %Y"))




# ...................................... language settings ................................................................................................
Sys.getlocale()
# "LC_COLLATE=Slovak_Slovakia.1250;LC_CTYPE=Slovak_Slovakia.1250;LC_MONETARY=Slovak_Slovakia.1250;LC_NUMERIC=C;LC_TIME=Slovak_Slovakia.1250"
Sys.setlocale("LC_ALL","English_United Kingdom")
Sys.setlocale("LC_ALL","Slovak_Slovakia")
# ...................................... language settings ................................................................................................




expertise_level <- ggplot(data = mem_data_long_avg_m, mapping = aes(x = month_com, y = expertise, colour = area)) + 
  geom_line(size = 1) +
  geom_point(size = 4, alpha = .8) +
  scale_colour_manual(values = dvs_colour) + 
  scale_y_continuous(limits = c(0,5), breaks = seq(0, 5, 1), labels = c("Novice", "Learning", "Beginning", "Intermediate", "Advanced", "Expert")) +
  ggtitle("Average level of expertise") + 
  guides(colour = FALSE) +
  theme_white + theme(axis.title.x = element_blank()) +
  transition_reveal(month_com)

animate(expertise_level, 
        width = 300, # 900px wide
        height = 300, # 600px high
        nframes = 200, # 200 frames
        fps = 10) # 10 frames per second

anim_save("dvs_expertise.gif")

# expertise_level_area_m <- ggplot() + geom_area(data = mem_data_long_avg_m, mapping = aes(x = month_com, y = expertise, fill = area, colour = area), 
#                                                size = 1.5, alpha = 0.1, position = "identity") +
#   scale_fill_manual(values = dvs_colour) + scale_colour_manual(values = dvs_colour) + ylim(0,5) +
#   theme_white
# 
# expertise_level_area_w <- ggplot() + geom_area(data = mem_data_long_avg_w, mapping = aes(x = week_com, y = expertise, fill = area, colour = area), 
#                                              size = 1.5, alpha = 0.1, position = "identity") +
#   scale_fill_manual(values = dvs_colour) + scale_colour_manual(values = dvs_colour) + ylim(0,5) +
#   theme_white

# ggplot() + geom_area(data = mem_data_long_avg_m, mapping = aes(x = month_com, y = expertise, fill = area), colour = "white", size = 2, alpha = 1) +
#   scale_fill_manual(values = dvs_colour) + scale_colour_manual(values = dvs_colour) +
#   theme_white

members <- ggplot() + geom_area(data = cumulative_d, mapping = aes(x = date2, y = members_cumulative), fill = "#333333", alpha = .8) +
  annotate("text", x = as.Date("2020-01-31"), y = 7000, hjust = "right", size = 6, colour = "white", label = "YEAR 1") +
  annotate("text", x = as.Date("2020-01-31"), y = 3000, hjust = "right", size = 5, colour = "white", label = "11 573 members!") +
  ggtitle("Members") +
  theme_white + theme(axis.title.x = element_blank()) +
  transition_reveal(date2)

animate(members, 
        width = 900, # 900px wide
        height = 120, # 600px high
        nframes = 200, # 200 frames
        fps = 10) # 10 frames per second

anim_save("dvs_members.gif")




#............... VENN DIAGRAM ...........................................................................................................................

venn_data <- list(data = na.omit(venn_membership2$V2_data), visualization = na.omit(venn_membership2$V2_viz), society = na.omit(venn_membership2$V2_soc))
View(venn_data$data)

# ggVennDiagram(x = venn_data)

venn.diagram(venn_data, filename = "dvs_venn_diagram.png", height = 300, width = 300,
             cex = .18, cat.cex = .2, default.cat.pos = "outer", cat.pos = c(-25,20,180), cat.dist = c(.1, .1, .07),
             fontfamily = "sans",
             fill = dvs_colour, col = dvs_colour, alpha = .4)


