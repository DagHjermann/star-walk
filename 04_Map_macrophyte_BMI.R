#
# Making maps in "Figures" folder
# Note: does not only make the macrophyte maps, but also the "inset" maps in the overview map
# 
# NOTE 2: MAY NOT WORK IN R-STUDIO
#

#
# From
# https://stackoverflow.com/a/52617929
#

# require(devtools)
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

setwd("C:/Data/seksjon 318/star-walk")


library(ggplot2)
library(readxl)

library(ggmap)
register_google(key = "AIzaSyCkizF9jsmew_N3tFzHG7pzMkGFqjZaxnA")     # copied directly from Google Console via 'copy' button

ggmap(get_googlemap(center = c(20.19387, 41.78988), zoom=16))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


dir("Data")
mydata <- read_excel("Data/summary BMI results.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Choose colors, including grey for "not enough macrophytes" (redefines BMI for this) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# "not enough macrophytes" category => BMI = 99
sel <- is.na(mydata$BMI)
mydata$BMI[sel] <- 99

# "Correct" colors
categories <- c("oligotrophic", "oligo-mesotrophic", "mesotrophic 1", "mesotrophic 2", 
                "eutrophic 1", "eutrophic 2", "eutrophic 3", "Not enough macrophytes")
colors <- c("darkblue","blue","darkgreen","green","yellow","orange","red", "grey40")
# names(colors) <- categories   # this provides the 'link' between categories and colors. Try 'colors' to see the result

# Categorize BMI with correct colors
# mydata$Index_cat <- cut(mydata$BMI, c(1, 2.4, 2.7, 2.95, 3.3, 3.55, 3.9, 5), labels = colors, drop = FALSE)
mydata$Index_cat <- cut(mydata$BMI, c(1, 2.4, 2.7, 2.95, 3.3, 3.55, 3.9, 5.01,100), labels = categories, drop = FALSE)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Test for macrophyte maps
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Plot for selected lake
selected_lake <- "Biogradsko"
zoom <- 16
selected_lake <- "Sava"
zoom <- 14
mapdata <- with(subset(mydata, lake == selected_lake), get_googlemap(c(lon=mean(Elongitude), lat=mean(Nlatitude)), zoom=zoom, maptype = "satellite"))
map_sat <- ggmap(mapdata)
mapdata <- with(subset(mydata, lake == selected_lake), get_googlemap(c(lon=mean(Elongitude), lat=mean(Nlatitude)), zoom=zoom, maptype = "terrain"))
map_terrain <- ggmap(mapdata)

# Points without border
map_terrain + 
  geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude, color = Index_cat), size = 4) + 
  scale_color_manual(values = colors, drop = FALSE)

# Points with border
windows()
map_terrain + 
  geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude, fill = Index_cat), pch = 21, size = 4.5) + 
  scale_fill_manual(values = colors, drop = FALSE)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Macrophyte maps
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Plot "Index_cat" for selected lake, function
plot_lake_index_OLD <- function(selected_lake, zoom = 13, 
     centre_lon = mean(subset(mydata, lake == selected_lake)$Elongitude), 
     centre_lat = mean(subset(mydata, lake == selected_lake)$Nlatitude), 
     maptype = "terrain", save = FALSE){
  mapdata <- with(subset(mydata, lake == selected_lake), get_googlemap(c(lon=centre_lon, lat=centre_lat), zoom=zoom, maptype = maptype))
  map <- ggmap(mapdata)
  gg <- map + 
    geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude, color = Index_cat), size = 4) + 
    scale_color_manual(values = colors, drop = FALSE) +
    theme(axis.title = element_blank())
  if (save){
    ggsave(sprintf("Figures/Map_macrophyte_BMI_%s.png", selected_lake), gg)
    return <- NULL
  } else {
    return <- gg
  }
  return
  }

# Plot "Index_cat" for selected lake, function
plot_lake_index <- function(selected_lake, zoom = 13, 
     centre_lon = mean(subset(mydata, lake == selected_lake)$Elongitude), 
     centre_lat = mean(subset(mydata, lake == selected_lake)$Nlatitude), 
     maptype = "terrain", save = FALSE){
  mapdata <- with(subset(mydata, lake == selected_lake), get_googlemap(c(lon=centre_lon, lat=centre_lat), zoom=zoom, maptype = maptype))
  map <- ggmap(mapdata)
  gg <- map + 
    # geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude, color = Index_cat), size = 4) + 
    # scale_color_manual(values = colors, drop = FALSE) +
    geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude, fill = Index_cat), pch = 21, size = 4.5) + 
    scale_fill_manual(values = colors, drop = FALSE) +
    theme(axis.title = element_blank())
  if (save){
    ggsave(sprintf("Figures/Map_macrophyte_BMI_%s.png", selected_lake), gg)
    return <- NULL
  } else {
    return <- gg
  }
  return
  }


xtabs(~(BMI == 99) + lake, mydata)
plot_lake_index("Ohrid", zoom = 11, save = TRUE)
# subset(mydata, lake == "Biogradsko") %>% as.data.frame()
plot_lake_index("Biogradsko", zoom = 16, centre_lon = 19.59971, centre_lat = 42.89780, save = TRUE)
# subset(mydata, lake == "Prespa") %>% as.data.frame()
plot_lake_index("Prespa", zoom = 11, centre_lon = 21.0, centre_lat = 40.85, save = TRUE)
plot_lake_index("Sava", zoom = 14, save = TRUE)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Inset maps
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Plot just a small (3.5 x 3.5 sq inch) map for selected lake, function
# For making inset plots for overview map
plot_lake <- function(selected_lake, zoom = 13, 
     centre_lon = mean(subset(mydata, lake == selected_lake)$Elongitude), 
     centre_lat = mean(subset(mydata, lake == selected_lake)$Nlatitude), 
     maptype = "terrain", save = FALSE){
  mapdata <- with(subset(mydata, lake == selected_lake), get_googlemap(c(lon=centre_lon, lat=centre_lat), zoom=zoom, maptype = maptype))
  map <- ggmap(mapdata)
  gg <- map + 
    geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude), color = "black", size = 3) +
    geom_point(data = subset(mydata, lake == selected_lake), aes(Elongitude, Nlatitude), color = "white", size = 1)
  if (save){
    ggsave(sprintf("Figures/Map__%s.png", selected_lake), gg, width = 3.5, height = 3.5)
    return <- NULL
  } else {
    return <- gg
  }
  return
  }

# Plot with zoom 1 step lower than normal, in order to not get too many names on map
plot_lake("Ohrid", zoom = 10)
plot_lake("Ohrid", zoom = 10, save = TRUE)
plot_lake("Biogradsko", zoom = 15, centre_lon = 19.59971, centre_lat = 42.89780, save = TRUE)
plot_lake("Prespa", zoom = 10, centre_lon = 21.0, centre_lat = 40.85, save = TRUE)
plot_lake("Sava", zoom = 13, save = TRUE)
plot_lake("Crno", zoom = 14, save = TRUE)
plot_lake("Lura", zoom = 15, save = TRUE)




