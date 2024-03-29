#
# https://stackoverflow.com/questions/61838700/query-on-how-to-make-world-heat-map-using-ggplot-in-r
#
# Desde la línea de comando: Rscript World_Heatmap.R
#
library(maps)
library(ggplot2)

mydata <- readxl::read_excel("CROCO2022.xlsx")

#mydata$Country[mydata$Country == "United States"] <- "USA"

world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

ggplot(mydata) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", linewidth = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = Country, fill = Members), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Participantes") +
  expand_limits(x = world_map$long, y = world_map$lat)
