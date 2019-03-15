
# Adding png files for plots of each lake (map or other type of diagram)
# on top of overview map ('Map_STARWALK.png') with lines pointing from site 
# to that site's diagram

# Not finished! Plots the 4 maps on the left side and two 

library(magick)

# Run this in RStudio, maps will be shown in Viewer pane

# get png dimensions
pngdim <- function(image){
  txt <- subset(image_attributes(image), property == "png:IHDR.width,height")$value
  txt2 <- strsplit(txt, ",")
  c(as.numeric(txt2[[1]][1]), as.numeric(txt2[[1]][2]))
}


#
# Overview figure, ie map of all lakes  ----
#
# dir("Figures")
overview <- image_read("Figures/Map_STARWALK.png")
overview

#
# Read figures for each lake ----
# All figures are precropped/resized to width 250
#
# Here I use plots cropped/resized manually in Paint first, but may easily
#   do the cropping using image_crop(image, geometry) [rotate, resize, crop, flip; see vignette])
#
map1 <- image_read("Figures/Map__Sava_crop2.png")
map2 <- image_read("Figures/Map__Crno_crop2.png")
map3 <- image_read("Figures/Map__Biogradsko_crop2.png")
map4 <- image_read("Figures/Map__Lura_crop2.png")
pngdim(map1)

top_border <- 10
left_border <- 10
space_between <- 20

#
# add_space() function 
# This function that adds space (transparent areas) to left and top of figure ----
#
# This is how we "place" the subplots on the overview map (all subplots are placed so the top left corner
#   aligns with the overview plot, but with added transparent areas on top/left it looks like the subplots are placed
#   further below and to the right)
#
# Actually: I discovered afterwards that the add_space() function actually does the same as image_border(); see map1c below
#

add_space <- function(image, left = 10, top = 10){
  image2 <- image_append(c(
      image_blank(width = left, height = pngdim(image)[2]),
      image
    ))
  image3 <- image_append(c(
      image_blank(width = pngdim(image)[1], height = top),
      image2
    ), stack = TRUE)
  image3  
}

#
# Add space to each figure ----
#  
map1b <- add_space(map1, 
                   left = left_border, 
                   top = top_border)

# The following line does exactly the same as for map1b (i.e., map1c = map1b)
# map1c <- image_border(map1, "transparent", "10x10")

map2b <- add_space(map2, 
                   left = left_border, 
                   top = top_border + pngdim(map1)[2] + space_between)
map3b <- add_space(map3, 
                   left = left_border, 
                   top = top_border + pngdim(map1)[2] + pngdim(map2)[2] + 2*space_between)
map4b <- add_space(map4, 
                   left = left_border, 
                   top = top_border + pngdim(map1)[2] + pngdim(map2)[2] + pngdim(map3)[2] + 3*space_between)


#
# Add plots on top of each other ----
#
result <- image_mosaic(c(overview, map1b, map2b, map3b, map4b))
result 

# Test map1c (see above):
# result <- image_mosaic(c(overview, map1c, map2b, map3b, map4b))  # test using map1c instead of map1b - see above

#
# Add lines from position of lake to subplot ----
#
dx <- 5
linecolor <- "blue3"
result2 <- image_draw(result)
segments(pngdim(map1)[1] + dx, 
         pngdim(map1)[2]/2 + top_border, 
         510, 245, col = linecolor, lwd = 3)   # found by trial and error (may use Paint to find coordinates?)
segments(pngdim(map1)[1] + dx, 
         pngdim(map1)[2] + space_between + pngdim(map2)[2]/2 + top_border, 
         385, 462, col = linecolor, lwd = 3)
dev.off()
result2


image_implode(result2, 0.7)
?segments


