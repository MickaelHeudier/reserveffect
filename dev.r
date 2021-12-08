
#create compendium
install.packages("remotes")
remotes::install_github("benmarwick/rrtools")

rrtools::use_compendium("../reserveffect", open = FALSE)

#create file
dir.create("data")
dir.create("outputs")

##################################### map ######################################
#Define study area coordnates
#small region
lat1_sm =  -21.51 ; lat2_sm = -21.66
lon1_sm = 165.21 ; lon2_sm = 165.45

#fond de carte
install.packages("cartography")
install.packages("OpenStreetMap")
install.packages("raster")
library(cartography)
library(OpenStreetMap)
library(raster)

createmap <- function(){
  map = OpenStreetMap::openmap(c(lat2_sm,lon1_sm), c(lat1_sm,lon2_sm), zoom = NULL,
                               type = c("bing"), #for satellite view
                               mergeTiles = TRUE)
  maplatlon = OpenStreetMap::openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  plot(maplatlon)
  barscale(size = 5, lwd = 2, cex = 1.2, pos = c(-21.64,165.44))
  cartography::layoutLayer(title = "Poe", sources = "OpenStreetMap", author = "", frame = FALSE, scale = 5, coltitle = "white",tabtitle = TRUE, postitle = "left", north = TRUE)
  cartography::getGridLayer(lat1_sm, cellsize, type = "regular", var)
}

fondcarte = createmap()

