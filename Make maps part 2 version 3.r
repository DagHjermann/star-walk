# Make maps part 2
# Dag Hjermann, NIVA - R course in Star-Walk project, 3.-4. July 2018 

library(ggplot2)
library(ggmap)
library(cowplot)
library(readxl)

register_google(key = "AIzaSyCkizF9jsmew_N3tFzHG7pzMkGFqjZaxnA")     # copied directly from Google Console via 'copy' button

# test 1
ggmap(get_googlemap(center = c(20.19387, 41.78988), zoom=16))

# test 2
mymap <- qmap(location = c(20.19387, 41.78988), zoom=16)
mymap

data_positions <- read_excel("Data/STAR-WALK sites.xlsx")

### Plot positions

mymap + 
  geom_point(data = data_positions, aes(Elongitude, Nlatitude), color = "black")


# Plot posisions and station names
mymap + 
  geom_point(data = data_positions, aes(Elongitude, Nlatitude), color = "black") +
  geom_text(data = data_positions, aes(x = Elongitude + 0.0003, y = Nlatitude, label=site), hjust = "left", color = "black")


# Plot chemistry data on map
# Get chemistry data into R’s memory

data_chem <- read_excel("Data/summary of most important chemistry results.xlsx", skip = 2, na = c("","n.d."))

data_chem_TSI <- read_excel("Data/Carlson's trophic state index.xlsx", sheet = "TSI")
data_chem_TSI <- data_chem_TSI[,c(1,2,4,5)]
data_chem <- merge(data_chem, data_chem_TSI, by = c("lake","site"), all.x = TRUE, all.y = FALSE)
data_chem <- merge(data_chem, data_positions, by = c("lake","site"), all.x = TRUE, all.y = FALSE)



# Note: if you have the dplyr package installed, you can do as follows:
# data_chem <- left_join(data_chem, data_positions, by = c("lake","site"))

# Plot (first try)
mymap + 
  geom_point(data = data_chem, aes(Elongitude, Nlatitude, color = water_TP_autumn))


# Plot (second try)
# We make the points larger by setting size = 5

mymap + 
  geom_point(data = data_chem, aes(Elongitude, Nlatitude, color = water_TP_autumn), size = 5)


# Plot (with different color scale)
# We use a better color scale by setting scale_color_distiller(palette = "RdYlGn")
# + Note: ‘data_chem’ has data for all lakes, and the colours scale is adapted to that. But because ‘mymap’ sets the limits of the map we only see the points around this lake - the other lakes are outside the picture.

mymap + 
  geom_point(data = data_chem, aes(Elongitude, Nlatitude, color = water_TP_autumn), size = 5) +
  scale_color_distiller(palette = "RdYlGn")

# Plot (color scale based on this lake only)
# To make the colour scale adjust to only the lake we show, we have to make a new data set (we call it ‘data_chem_mylake’) for only the data of this lake.

data_chem_mylake <- subset(data_chem, lake == "Lura")
mymap + 
  geom_point(data = data_chem_mylake, aes(Elongitude, Nlatitude, color = water_TP_autumn), size = 5) +
  scale_color_distiller(palette = "RdYlGn")


# More things we can do to improve our plot
# Make points more visible by adding a black border
# We do this by using ‘shape = 21’. + Note that we have to change ‘color’ and ‘scale_color_distiller’ into ‘fill’ and ‘scale_fill_distiller’, because for this shape, ‘color’ is only the color of the border around the circle.

mymap + 
  geom_point(data = data_chem, aes(Elongitude, Nlatitude, fill = water_TP_autumn), shape = 21, size = 5) +
  scale_fill_distiller(palette = "RdYlGn")


# Change shape
# Here we change ‘shape = 21’ to ‘shape = 22’. Run the command ?points to see the different shapes we can use.

mymap + 
  geom_point(data = data_chem_mylake, aes(Elongitude, Nlatitude, fill = water_TP_autumn), shape = 22, size = 5) +
  scale_fill_distiller(palette = "RdYlGn")


# Change title of the legend
# We do this by adding the title just after the first parenthesis in the scale definition  scale_fill_distiller.
# Note that \p means “line shift”, so Total water P\nAutumn prints “Total water”, line shift, “Autumn”.

mymap + 
  geom_point(data = data_chem_mylake, aes(Elongitude, Nlatitude, fill = water_TP_autumn), shape = 22, size = 5) +
  scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn")


# Add plot title
mymap + 
  geom_point(data = data_chem, aes(Elongitude, Nlatitude, fill = water_TP_autumn), shape = 22, size = 5) +
  scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn") + 
  labs(title = "Lake Lura, total phosphorus")



# Show variation using point size
# Added size = water_TP_autumn inside the aes() paranthesis
# Remember to delete the size = 5 outside the aes() paranthesis
# We can also use size to show a different variable

mymap + 
  geom_point(data = data_chem, aes(Elongitude, Nlatitude, fill = water_TP_autumn, size = water_TP_autumn), shape = 22) +
  scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn")


# Plot a different variable (sediment total P)
# Changed water_TP_autumn to sed_TP_autumn
# Remember to also change any titles (here, legend title)
mymap + 
  geom_point(data = data_chem_mylake, aes(Elongitude, Nlatitude, fill = sed_TP_autumn), shape = 22, size = 5) +
  scale_fill_distiller("Total sediment P\nAutumn", palette = "RdYlGn")


##################################
# Plot diatom data
# Get diatom data into R’s memory
##################################

data_diatoms <- read_excel("summary of the most important diatom results.xlsx", na = c("","n.d."))

# Add columns for longitude and latitude to the data
data_diatoms <- merge(data_diatoms, data_positions, by = c("lake","site"), all.x = TRUE, all.y = FALSE)


# Plot IPS as colours on map
mymap + 
  geom_point(data = data_diatoms, aes(Elongitude, Nlatitude, color = IPS), size = 5) +
  scale_color_distiller(palette = "RdYlGn")

##################################
# Plot macroinvertebrate data
##################################

# Get macroinvertebrate data into R’s memory
data_invert <- read_excel("summary of most important macroinvertebrate data.xlsx", sheet = "indices_traits", skip = 1, na = c("","n.d."))

# Add columns for longitude and latitude to the data
data_invert <- merge(data_invert, data_positions, by = c("lake","site"), all.x = TRUE, all.y = FALSE)

# Plot ASPT as colours on map
mymap + 
  geom_point(data = data_invert, aes(Elongitude, Nlatitude, color = ASPT), size = 5) +
  scale_color_distiller(palette = "RdYlGn")

##################################
# Plot macrophyte data
##################################
# Get macrophyte data into R’s memory
data_macrophyte <- read_excel("summary of most important macrophyte results.xlsx", skip = 1, na = c("","n.d."))

# Add columns for longitude and latitude to the data
data_macrophyte <- merge(data_macrophyte, data_positions, by = c("lake","site"), all.x = TRUE, all.y = FALSE)

# Plot MI as colours on map
mymap + 
  geom_point(data = data_macrophyte, aes(Elongitude, Nlatitude, color = MI), size = 5) +
  scale_color_distiller(palette = "RdYlGn")

####################################################################
#
# Make plot of all lakes
#
####################################################################

# Mean position of each lake
x <- data_positions[,c("Elongitude","Nlatitude")] 
aggregate(x, list(Lake = data_positions$lake), mean)
#
# Note: with the package dplyr installed, you can instead do as follows:
#
# data_positions %>%
#   group_by(lake) %>%
#   summarise(long = mean(Elongitude), lat = mean(Nlatitude)) %>%
#   as.data.frame()


# Create background maps
mymap1 <- qmap(location = c(19.09116, 43.14598), zoom=16)
mymap2 <- qmap(location = c(19.5983, 42.898), zoom=16)
mymap3 <- qmap(location = c(20.39183, 44.78354), zoom=14)
mymap4 <- qmap(location = c(20.19387, 41.78988), zoom=16)
mymap5 <- qmap(location = c(20.73327, 41.03089), zoom=11)
mymap6 <- qmap(location = c(20.95873, 40.90712), zoom=11)

# Create a list of maps and arrange them
map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
plot_grid(plotlist = map_list, ncol = 2)

# Create a list of maps, add points, and arrange them
# We first make a list of 6 maps as before. We then make a loop that goes through the numbers 1 to 6 (represnted by i), and for each round in the loop we pick map number i and add the data points just as before.

map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
  
for (i in 1:6){
  map_list[[i]] <- map_list[[i]] +
    geom_point(data = data_chem, aes(Elongitude, Nlatitude, fill = water_TP_autumn), pch = 21, size = 5) +
    scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn")
}

# Plot this new list of maps
plot_grid(plotlist = map_list, ncol = 2)


#
# Plot only points for the correct lake
#
# NOTE: below there is defined a function for making these maps (plot_lakes_continuous), making this much simpler
#
# Inside geom_point, we replace data_chem with subset(data_chem, lake == lakes[i])
# Note that in order to keep the same color scale for all plots, we need to add
# total_range <- range... where we find the minimum/maximum of  water_TP_autumnand store it in total_range
# limits = total_range inside scale_fill_distiller; this sets all color scales to go between the same minimum/maximum
#
#


map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
lakes <- c("Crno","Biogradsko","Sava","Lura","Ohrid","Prespa")
total_range <- range(data_chem$water_TP_autumn, na.rm = TRUE)
for (i in 1:6){
  map_list[[i]] <- map_list[[i]] +
    geom_point(data = subset(data_chem, lake == lakes[i]), aes(Elongitude, Nlatitude, fill = water_TP_autumn), pch = 21, size = 5) +
    scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn", limits = total_range)
}

# Plot this new list of maps
plot_grid(plotlist = map_list, ncol = 2)


# As the previous plot but include just a common legend for all lakes
# * Inside scale_fill_distiller, include guide = FALSE to make plots without legend
# * Make one plot (plot_with_legend) just for “picking out” the legend using get_legend. 
#   Here, plot_with_legend is made using the data for lake no. 1, but which we use doesn't doesn’t matter.

map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
lakes <- c("Crno","Biogradsko","Sava","Lura","Ohrid","Prespa")
total_range <- range(data_chem$water_TP_autumn, na.rm = TRUE)
for (i in 1:6){
  map_list[[i]] <- map_list[[i]] +
    geom_point(data = subset(data_chem, lake == lakes[i]), aes(Elongitude, Nlatitude, fill = water_TP_autumn), pch = 21, size = 5) +
    scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn", limits = total_range, guide = FALSE)
}

# plot_with_legend will not actually be plotted, but we will use its legend (by command get_legend)
plot_with_legend <-  ggplot() +
  geom_point(data = subset(data_chem, lake == lakes[1]), aes(Elongitude, Nlatitude, fill = water_TP_autumn), pch = 21, size = 5) +
  scale_fill_distiller("Total water P\nAutumn", palette = "RdYlGn", limits = total_range) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

# Plot this new list of maps
plot_grid(
  plot_grid(plotlist = map_list, ncol = 2),
  get_legend(plot_with_legend + scale_shape(guide = FALSE) + theme(legend.position = "bottom")),
   ncol=1, rel_heights=c(.85, .15)
  )


  
  
#
## Add and plot index / categorization of a variable
#
# NOTE: below there is defined a function for making these maps (plot_lakes_categories), making this much simpler
#
### Make index variable based on `water_TP_autumn`, let us call it `water_TP_autumn_index`.  
# - As an example we here define 
#     - 3 categories (low, medium, high)  
#     - Threshold values are 15 and 25
# -  When you change this, remember that **number of colors = number of categories** but **number of threshold values = number of categories + 1**
#
# NOTE: 
#

my_categories <- c("Low autumn P", "Medium autumn P", "High autumn P")
my_colors <- c("green","orange","red")
names(my_colors) <- my_categories   # this provides the 'link' between categories and colors

data_chem$water_TP_autumn_index <- cut(data_chem$water_TP_autumn, 
                        c(0, 15, 25, 100),
                        labels = my_categories)

                        
### Define plots  
# Similar to the version with continuous colors, except
# - We change `water_TP_autumn`to `water_TP_autumn_index`
# - We use `scale_fill_manual` with colors to be `my_colors` as we defined above
# - When we define the scale in `plot_with _legend`, we replace `limits = total_range` with `drop = TRUE` (this ensures that all categories are shown in the legend)  

map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
lakes <- c("Crno","Biogradsko","Sava","Lura","Ohrid","Prespa")
for (i in 1:6){
  label_x <- min(subset(data_chem, lake == lakes[i])$Elongitude)
  label_y <- max(subset(data_chem, lake == lakes[i])$Nlatitude)
  map_list[[i]] <- map_list[[i]] +
    geom_point(data = subset(data_chem, lake == lakes[i]), aes(Elongitude, Nlatitude, fill = water_TP_autumn_index), pch = 21, size = 5) +
    scale_fill_manual(values = my_colors, guide = FALSE)
    }

# To be used just for "picking out" the legend using get_legend
plot_with_legend <-  ggplot() +
  geom_point(data = subset(data_chem, lake == lakes[1]), aes(Elongitude, Nlatitude, fill = water_TP_autumn_index), pch = 21, size = 5) +
  scale_fill_manual("Total water P Autumn", values = my_colors, drop = FALSE) +
  theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"))

### Plot this new list of maps
plot_grid(
  plot_grid(plotlist = map_list, ncol = 2, labels = lakes, label_x = 0.1, hjust = 0),
  get_legend(plot_with_legend + scale_shape(guide = FALSE) + theme(legend.position = "bottom")),
   ncol=1, rel_heights=c(.85, .15)
  )


#########################################################################################################################
#
# Define functions for plotting all lakes
#
#########################################################################################################################

plot_lakes_continuous <- function(dataset, variable, legend_title = NULL, 
                                      palette = "RdYlGn", reverse_colors = FALSE){
  dataset$variable_plot <- dataset[,variable]
  total_range <- range(dataset$variable_plot, na.rm = TRUE)
  if (is.null(legend_title))
    legend_title = variable
  map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
  lakes <- c("Crno","Biogradsko","Sava","Lura","Ohrid","Prespa")
  for (i in 1:6){
    label_x <- min(subset(dataset, lake == lakes[i])$Elongitude)
    label_y <- max(subset(dataset, lake == lakes[i])$Nlatitude)
    map_list[[i]] <- map_list[[i]] +
      geom_point(data = subset(dataset, lake == lakes[i]), 
                 aes(Elongitude, Nlatitude, fill = variable_plot), 
                 pch = 21, size = 5) +
      scale_fill_distiller(palette = palette, limits = total_range, guide = FALSE, 
                           direction = ifelse(reverse_colors,-1,1))
  }
  
  plot_with_legend <-  ggplot() +
    geom_point(data = subset(dataset, lake == lakes[2]), 
               aes(Elongitude, Nlatitude, fill = variable_plot), 
               pch = 21, size = 5) +
    scale_fill_distiller(legend_title, palette = palette, limits = total_range, 
                         direction = ifelse(reverse_colors,-1,1)) +
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
  
  plot_grid(
    plot_grid(plotlist = map_list, ncol = 2, labels = lakes, label_x = 0.1, hjust = 0),
    get_legend(plot_with_legend + scale_shape(guide = FALSE) + theme(legend.position = "bottom")),
    ncol=1, rel_heights=c(.85, .15)
  )
}

# plot_lakes_categories 
# length(my_categories) was length(my_colors) - corrected now
#

plot_lakes_categories <- function(dataset, variable, legend_title = NULL, colors, main_title = NULL){
  dataset$variable_plot <- dataset[,variable]
  if (is.null(legend_title))
    legend_title = variable
  my_categories <- levels(dataset[,variable])
  if (length(my_categories) != length(colors))
    stop("Number of colors does not equal the number of categories")
  names(colors) <- levels(dataset[,variable]) 
  map_list <- list(mymap1, mymap2, mymap3, mymap4, mymap5, mymap6)
  lakes <- c("Crno","Biogradsko","Sava","Lura","Ohrid","Prespa")
  for (i in 1:6){
    label_x <- min(subset(dataset, lake == lakes[i])$Elongitude)
    label_y <- max(subset(dataset, lake == lakes[i])$Nlatitude)
    map_list[[i]] <- map_list[[i]] +
      geom_point(data = subset(dataset, lake == lakes[i]), 
                 aes(Elongitude, Nlatitude, fill = variable_plot), 
                 pch = 21, size = 5) +
      scale_fill_manual(values = colors, guide = FALSE)
  }
  
  plot_with_legend <-  ggplot() +
    geom_point(data = subset(dataset, lake == lakes[2]), 
               aes(Elongitude, Nlatitude, fill = variable_plot), 
               pch = 21, size = 5) +
    scale_fill_manual(legend_title, values = colors, drop = FALSE) +
    theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"))

  if (!is.null(main_title)){
    title <- ggdraw() + draw_label(main_title, fontface='bold')  
    result <- plot_grid(
      title,
      plot_grid(plotlist = map_list, ncol = 2, labels = lakes, label_x = 0.1, hjust = 0),
      get_legend(plot_with_legend + scale_shape(guide = FALSE) + theme(legend.position = "bottom")),
      ncol=1, rel_heights=c(0.10, 0.75, 0.15)
    )
  } else {
    result <- plot_grid(
      plot_grid(plotlist = map_list, ncol = 2, labels = lakes, label_x = 0.1, hjust = 0),
      get_legend(plot_with_legend + scale_shape(guide = FALSE) + theme(legend.position = "bottom")),
      ncol=1, rel_heights=c(.85, .15)
    )
  }
  result
}


#########################################################################################################################
#
## Examples of using the functions
#
#########################################################################################################################

### Example 1: plotting a continuous variable
# debugonce(plot_lakes_continuous)
plot_lakes_continuous(data_invert, "ASPT", "ASPT value")

# If you want the colors in reverse direction, do as follows (remove the # sign):
# plot_lakes_continuous(data_invert, "ASPT", "ASPT value", reverse_colors = TRUE)
#

### Example 2: making and plotting a categorical variable

# Making categorical variable
data_invert$ASPT_categories <- cut(data_invert$ASPT, breaks = c(0,3,5,100), labels = c("Low","Median","High"))

# Plotting categorical variable
plot_lakes_categories(data_invert, "ASPT_categories", "ASPT", c("red","orange","green"))


#
# Chemistry
#

#
var <- "TSI_spring"
text <- "TSI spring"
title <- "TSI (TP) spring campaign"

var <- "TSI_autumn"
text <- "TSI autumn"
title <- "TSI (TP) autumn campaign"

brks <- c(0,30,40,50,60,100)
labs1 <- c("High", "Good", "Moderate", "Poor", "Bad")
a <- brks[-6] + c(0,1,1,1,1)
b <- brks[-1]
labs2 <- paste0(labs1, "\n", a, "-", b)
data_chem$Z_categories <- cut(data_chem[,var], breaks = brks, labels = labs2)
plot_lakes_categories(data_chem, "Z_categories", text, 
  c("blue","green","yellow","orange","red"),
  main_title = title)
ggsave(paste0("Figures/Map_chemistry_", var, ".png"), width = 7.7, height = 7.2)
