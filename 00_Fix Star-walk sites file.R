#
# Read 'STAR-WALK sites original.xlsx', extract decimal longitude/latitude and write to csv file '01_STAR-WALK sites.csv'
# Decimal longitude/latitude from this file was used to create the Elongitude, Nlatitude values in'STAR-WALK sites.xlsx', 
#   which becomes our new source file to use for script 01
# 

library(purrr)
library(readxl)

data_positions <- read_excel("Data/STAR-WALK sites original.xlsx")
str(data_positions)

# Function that returns a list of 3 numeric vectors
position_split <- function(x){
  x2 <- strsplit(x, " ") %>% transpose()
  list(degrees = x2[[1]] %>% map_chr(~.) %>% gsub("[^[:digit:]]", "", .) %>% as.numeric(),
       minutes = x2[[2]] %>% map_chr(~.) %>% gsub("'", "", .) %>% as.numeric(),
       seconds = x2[[3]] %>% map_chr(~.) %>% gsub("'", "", .) %>% as.numeric())
}

#
# Longitude
#
pos <- position_split(data_positions$longitude)
long <- pos$degrees + pos$minutes/60 + pos$seconds/3600

# Check differences
data_positions$Elongitude - long

# Update Elongitude column
data_positions$Elongitude <- long

#
# Latitude
#
pos <- position_split(data_positions$latitude)
lat <- pos$degrees + pos$minutes/60 + pos$seconds/3600

# Check differences
data_positions$Nlatitude - lat

# Update (overwrite) the Nlatitude columns
data_positions$Nlatitude <- lat


# Save data
write.csv2(data_positions, "Data/01_STAR-WALK sites.csv", row.names = FALSE, quote = FALSE)

# Afterwards we open this file in Excel and copy the correct Elongitude, Nlatitude values back to 'STAR-WALK sites.xlsx', which becomes our new source file

#
# Test plots
#

# Test plot 1
library(ggplot2)
ggplot(data_positions, aes(Elongitude, Nlatitude, color = lake)) +
  geom_point() +
  coord_fixed(1.7)

# Test plot 2
ggplot(data_positions, aes(Elongitude, Nlatitude, color = lake)) +
  geom_point() +
  coord_fixed(1.7)