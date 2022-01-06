

#' Make grid from study area raster
#'
#' @param r
#'
#' @return
#' @export
#'

make_grid <- function(r){

  # project raster
  rproj = raster::projectRaster(r, crs="+init=epsg:3163") #NC projection

  # convert raster to spatial polygon
  p = raster::rasterToPolygons(rproj)

  # convert to sf object and change names
  polygon = sf::st_as_sf(p)

  #add id
  polygon$id = 1:nrow(polygon)

  return(polygon)

}




#' restrict telemetry to dates
#'
#' @param telem
#' @param dates
#'
#' @return
#' @export
#'

restrict_telem_dates <- function(telem, dates){

  telem %>%
    dplyr::filter(date %in% dates) %>%
    droplevels() -> telem_dates

  return(telem_dates)
}



#' convert telemetry points to lines
#'
#' @param telem
#'
#' @return
#' @export
#'

convert_telemetry_points_to_lines <- function(telem){

  library(sp)
  #set coords
  coordinates(telem) = ~ lon + lat

  # list of Lines per date, each with one Line in a list
  # ***** does not work in function
  listlines = lapply(split(telem, telem$date), function(x) Lines(list(Line(coordinates(x))), x$date[1L]))

  return(listlines)
}



#' Sum length of tracks (m) in grid cells per date
#'
#' @param listlines
#' @param polygon
#' @param telem
#'
#' @return
#' @export
#'


sum_length_per_grid_per_date <- function(polygon, listlines, dates){

  # convert to spatial lines
  splines = sp::SpatialLines(listlines, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # convert to SpatialLinesDataFrame
  splinesdata = sp::SpatialLinesDataFrame(sl = splines, data = data.frame(date = dates), match.ID = FALSE)

  # project spatial lines
  lproj = sp::spTransform(splinesdata, CRS("+init=epsg:3163")) #NC projection

  # convert to sf object
  lines = sf::st_as_sf(lproj)

  #convert polygon to sf
  polygon %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = 1:nrow(.)) -> polygon2

  #intersect polygon with lines and sum line length per date
  sf::st_intersection(polygon2, lines) %>%
    #dplyr::group_by(id, date) %>%
    dplyr::mutate(length = sf::st_length(.)) %>%
    sf::st_drop_geometry() -> intersection

  #join polygon with lines
  polygon2 %>%
    dplyr::left_join(intersection, by = "id") -> polygon3

  return(polygon3)

}





#' Map track length per grid cell for Poe
#'
#' @param maplatlonproj
#' @param polytracks
#'
#' @return
#' @export
#'

map_tracklen_per_grid_poe <- function(maplatlonproj, polytracks){

  # convert back to spatial object for plotting
  polytracks2 = sf::as_Spatial(polytracks)

  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])

  # sum length per grid cell and select polygons with effort > 0
  effort2 = effort %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_length = sum(length)) %>% #IMPORTANT to sum lenght per cell across all flights
    dplyr::filter(tot_length > 0)

  # map
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_point(data = effort2, ggplot2::aes(x = lon, y = lat, color = tot_length), shape = 15, size=1.2) +
    #ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "length(m)")

  ggplot2::ggsave(here::here("outputs/poe_on_effort/map_tracklen_per_grid_poe_on.png"), map, width = 7, height = 5)

}






#' Map track length per grid cell per date for Poe
#'
#' @param maplatlonproj
#' @param polytracks
#'
#' @return
#' @export
#'

map_tracklen_per_grid_per_date_poe <- function(maplatlonproj, polytracks){

  # convert back to spatial object for plotting
  polytracks2 = sf::as_Spatial(polytracks)

  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      date = polytracks2$date,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])

  # sum length per grid cell
  effort2 = effort %>%
    dplyr::filter(length > 0)

  #loop on pages
  for (i in 1:5){
    map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
      ggplot2::geom_point(data = effort2, ggplot2::aes(x = lon, y = lat, color = length), shape = 15, size=0.55, alpha = 0.85) +
      ggforce::facet_wrap_paginate(~date, nrow = 2, ncol = 2, page = i) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                    name = "length(m)")

    ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_tracklen_per_grid_per_date_p", i, "_poe_on.png")), map, width = 7, height = 5)

  }

}






#' Make dataframe length of tracks per grid cell per date
#'
#' @param polytracks
#'
#' @return
#' @export
#'

make_df_tracklen_per_grid_per_date_poe <- function(polytracks){

  # convert back to spatial object for plotting
  polytracks2 = sf::as_Spatial(polytracks)

  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      date = polytracks2$date,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])

  # sum length per grid cell
  effort2 = effort %>%
    dplyr::filter(length > 0)

 return(effort2)

}





#' Count total number of observations in grid cells per date
#'
#' @param polygon
#' @param telem_obs
#'
#' @return
#' @export
#'
#'
count_obs_per_grid_per_date <- function(polygon, telem_obs){

  # keep megafauna observations
  telem_obs %>%
    dplyr::filter(object %in% c("Turtle", "Dugong_certain", "Dugong_probable", "Round_ray", "Eagle_ray", "Manta_ray", "Dolphin", "Shark")) -> telem_obs2

  # convert to sf object
  points = sf::st_as_sf(x = telem_obs2,
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  #project points
  points = sf::st_transform(points, CRS("+init=epsg:3163")) #NC projection

  # Intersection between polygon and points and count nb of points per date per object
  sf::st_intersection(x = polygon, y = points) %>%
    dplyr::group_by(id, date, object) %>%
    dplyr::count() %>%
    sf::st_drop_geometry() -> intersection

  #join polygon with points
  polygon %>%
    dplyr::left_join(intersection, by = "id") -> polygon2

  return(polygon2)

}

#' Map number of observations per grid cell for given species for POe
#'
#' @param maplatlonproj
#' @param polygon
#'
#' @return
#' @export
#'

map_obs_per_grid_species_poe <- function(maplatlonproj, polygon, species){

  # select polygons with counts > 0 for species
  polygon2 = polygon %>%
    dplyr::filter(object == species)

  # convert back to spatial object for plotting
  polygon3 = sf::as_Spatial(polygon2)

  # make dataframe for plotting
  counts = data.frame(id = polygon3$id,
                      count = polygon3$n,
                      date = polygon3$date,
                      lon = coordinates(polygon3)[,1],
                      lat = coordinates(polygon3)[,2])

  #IMPORTANT to sum counts per cell across all flights
  counts2 = counts %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_count = sum(count)) %>%
    dplyr::filter(tot_count > 0)

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_point(data = counts2, ggplot2::aes(x = lon, y = lat, color = tot_count), shape = 15, size=1.2, alpha = 0.85) +
    #ggplot2::theme_minimal() +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "individuals")

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_obs_per_grid_", species, "_poe_on.png")), map, width = 7, height = 5)

}






#' Map number of observations per grid cell for given species for POe
#'
#' @param maplatlonproj
#' @param polygon
#'
#' @return
#' @export
#'
#'
map_obs_per_grid_per_date_species_poe <- function(maplatlonproj, polygon, species){

  # select polygons with counts > 0 for species
  polygon2 = polygon %>%
    dplyr::filter(object == species)

  # convert back to spatial object for plotting
  polygon3 = sf::as_Spatial(polygon2)

  # make dataframe for plotting
  counts = data.frame(id = polygon3$id,
                      count = polygon3$n,
                      date = polygon3$date,
                      lon = coordinates(polygon3)[,1],
                      lat = coordinates(polygon3)[,2])

  dates = unique(counts$date)

  # if (length(dates) == 10) {nbpages <- 3}
  # if (length(dates) == 16) {nbpages <- 3}
  # if (length(dates) == 20) {nbpages <- 3}
  # if (length(dates) == 18) {nbpages <- 3}
  # if (length(dates) == 17) {nbpages <- 3}
  # if (length(dates) == 5) {nbpages <- 3}
  # if (length(dates) == 5) {nbpages <- 3}

  #loop on pages
  #handle error when there is one plot per page (Dolphin and Eagle_ray)
  if (!species %in% c("Dolphin", "Eagle_ray")){

    for (i in 1:(ceiling(length(dates)/4))){ #4 maps per page
      map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
        ggplot2::geom_point(data = counts, ggplot2::aes(x = lon, y = lat, color = count), shape = 15, size=0.55, alpha = 0.85) +
        ggforce::facet_wrap_paginate(~date, nrow = 2, ncol = 2, page = i) +
        ggplot2::ggtitle(species) +
        ggplot2::theme(axis.title = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                      name = "individuals")

      ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_obs_per_grid_per_date_", species, "_p", i, "_poe_on.png")), map, width = 7, height = 5)
    }

  }

  if (species == "Dolphin"){ #3 rows 2 columns on one page

    map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
        ggplot2::geom_point(data = counts, ggplot2::aes(x = lon, y = lat, color = count), shape = 15, size=0.55, alpha = 0.85) +
        ggforce::facet_wrap_paginate(~date, nrow = 3, ncol = 2, page = 1) +
        ggplot2::ggtitle(species) +
        ggplot2::theme(axis.title = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                      name = "individuals")

      ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_obs_per_grid_per_date_", species, "_p1_poe_on.png")), map, width = 7, height = 5)

  }

  if (species == "Eagle_ray"){ #3 rows 2 columns on 3 page

    for (i in 1:3){
      map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
        ggplot2::geom_point(data = counts, ggplot2::aes(x = lon, y = lat, color = count), shape = 15, size=0.55, alpha = 0.85) +
        ggforce::facet_wrap_paginate(~date, nrow = 3, ncol = 2, page = i) +
        ggplot2::ggtitle(species) +
        ggplot2::theme(axis.title = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                      name = "individuals")

    ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_obs_per_grid_per_date_", species, "_p", i, "_poe_on.png")), map, width = 7, height = 5)
    }

  }


}






#' Map density per grid cell for given species for POe (nb of observations / (length of tracks in meters * footprint_width in meters))
#' with mpa overlaid
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#' @param species
#' @param maplatlonproj
#' @param pa
#'
#' @return
#' @export
#'

map_dens_per_grid_species_poe <- function(maplatlonproj, polyobs, polytracks, footprintwidth, species, pa){

  # select polygons with counts of species
  polyobs2 = polyobs %>%
    dplyr::filter(object == species)

  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)

  # make dataframe for plotting
  counts = data.frame(id = polyobs3$id,
                      count = polyobs3$n,
                      date = polyobs3$date,
                      lon = coordinates(polyobs3)[,1],
                      lat = coordinates(polyobs3)[,2])

  #IMPORTANT to sum counts per cell across all flights
  counts2 = counts %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_count = sum(count)) %>%
    dplyr::filter(tot_count > 0)

  # convert back to spatial object for plotting (in meters)
  polytracks2 = sf::as_Spatial(polytracks)

  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])

  # sum length per grid cell and select polygons with effort > 0
  effort2 = effort %>%
    dplyr::group_by(id, lat, lon) %>%
    dplyr::summarise(tot_length = sum(length)) %>% #IMPORTANT to sum lenght per cell across all flights
    dplyr::filter(tot_length > 0)

  # merge effort and counts based on polygon id and calculate density
  result = counts2 %>%
    dplyr::left_join(effort2, by = "id") %>%
    dplyr::mutate(density = tot_count / (tot_length*footprintwidth))  %>% #density in indiv/m2
    dplyr::mutate(density = density * 10000)  %>%  #density in indiv/ha (1 ha = 10000m2 - 1m2 = 10-4 ha)
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y)

  # mpa polygon

  #project mpa polygon
  paproj = sp::spTransform(pa, CRS("+init=epsg:3163")) #NC projection

  # fortify
  pa2 = paproj %>%
    ggplot2::fortify(region = "NAME")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format and add merged results
    ggplot2::geom_point(data = result, ggplot2::aes(x = lon, y = lat, color = density), shape = 15, size=1.2, alpha = 0.85) +
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0.1) +
    #ggplot2::theme_minimal() +
    ggplot2::ggtitle(species) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                  name = "indiv/ha")

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_dens_per_grid_", species, "_poe_on.png")), map, width = 7, height = 5)

}






#' Map density per grid cell for given species per date for POe (nb of observations / (length of tracks in meters * footprint_width in meters))
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#' @param species
#' @param maplatlonproj
#'
#' @return
#' @export
#'


map_dens_per_grid_per_date_species_poe <- function(maplatlonproj, polyobs, polytracks, footprintwidth, species){

  # select polygons with counts > 0 for species
  polyobs2 = polyobs %>%
    dplyr::filter(object == species)

  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)

  # make dataframe for plotting
  counts = data.frame(id = polyobs3$id,
                      count = polyobs3$n,
                      date = polyobs3$date,
                      lon = coordinates(polyobs3)[,1],
                      lat = coordinates(polyobs3)[,2])

  # convert back to spatial object for plotting (in meters)
  polytracks2 = sf::as_Spatial(polytracks)

  # make dataframe for plotting
  effort = data.frame(id = polytracks2$id,
                      date = polytracks2$date,
                      length = as.numeric(polytracks2$length), #convert class units to numeric
                      lon = coordinates(polytracks2)[,1],
                      lat = coordinates(polytracks2)[,2])

  #select polygons with effort > 0
  effort2 = effort %>%
    dplyr::filter(length > 0)

  # merge effort and counts based on polygon id and date and calculate density

  result = counts %>%
    dplyr::right_join(effort2, by = c("id", "date")) %>%
    dplyr::mutate(density = count / (length*footprintwidth))  %>% #density in indiv/m2
    dplyr::mutate(density = density * 10000)  %>%  #density in indiv/ha (1 ha = 10000m2 - 1m2 = 10-4 ha)
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y)

  #loop on pages
  for (i in 1:5){
    map = OpenStreetMap::autoplot.OpenStreetMap(maplatlonproj) + ##convert OSM to ggplot2 format
      ggplot2::geom_point(data = result, ggplot2::aes(x = lon, y = lat, color = density), shape = 15, size=0.55, alpha = 0.85) +
      ggforce::facet_wrap_paginate(~date, nrow = 2, ncol = 2, page = i) +
      ggplot2::ggtitle(species) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_gradient(low = "dark grey", high = "red", na.value = NA,
                                    name = "indiv/ha")

    ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_dens_per_grid_per_date_", species, "_p", i, "_poe_on.png")), map, width = 7, height = 5)

  }

}





#' Count total number of observations in coral polygons per date
#'
#' @param coral_poly
#' @param telem_obs
#'
#' @return
#' @export
#'

count_obs_per_coral_poly_per_date <- function(coral_poly, telem_obs){

  # keep megafauna observations
  telem_obs %>%
    dplyr::filter(object %in% c("Turtle", "Dugong_certain", "Dugong_probable", "Round_ray", "Eagle_ray", "Manta_ray", "Dolphin", "Shark")) -> telem_obs2

  # convert to sf object
  points = sf::st_as_sf(x = telem_obs2,
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # project points
  points = sf::st_transform(points, CRS("+init=epsg:3163")) #NC projection

  # convert coral polygon to sf object
  coralsf = sf::st_as_sf(coral_poly)

  #project coral polygon
  coralproj = sf::st_transform(coralsf, CRS("+init=epsg:3163")) #NC projection

  # Intersection between coral polygon and points and count nb of points per date per object
  sf::st_intersection(x = coralproj, y = points) %>%
    dplyr::group_by(objectid, date, object) %>%
    dplyr::count() %>%
    sf::st_drop_geometry() -> intersection

  #join coral polygon with points
  coralproj %>%
    dplyr::left_join(intersection, by = "objectid") -> coralproj2

  return(coralproj2)

}



#' Map number of observations per coral polygon for given species for POe
#'
#' @param maplatlonproj
#' @param polygon
#' @param species
#'
#' @return
#' @export
#'

map_obs_per_coral_poly_species_poe <- function(maplatlonproj, polygon, species){

  # select polygons with counts > 0 for species
  polygon2 = polygon %>%
    dplyr::filter(object == species)

  # convert back to spatial object for plotting
  polygon3 = sf::as_Spatial(polygon2)

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(polygon3) #returns FALSE
  polygon4 = rgeos::gBuffer(polygon3, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(polygon4) #returns TRUE

  #crop polygons to osm extent
  cor3 = raster::crop(polygon4, raster::extent(maplatlonproj$bbox$p1[1], maplatlonproj$bbox$p2[1],
                                               maplatlonproj$bbox$p2[2], maplatlonproj$bbox$p1[2]))

  #convert to sf
  cor4 = sf::st_as_sf(cor3)

  #group and summarise
  cor4 %>%
    dplyr::group_by(l4_attrib) %>%
    dplyr::summarise(n_per_l4=sum(n)) -> cor5

  #convert back to spatial object
  cor6 = sf::as_Spatial(cor5)

  # fortify
  cor7 = cor6 %>%
    ggplot2::fortify(region = "n_per_l4")


  ### background polygon

  # convert back to spatial object for plotting
  polygon = sf::as_Spatial(polygon)

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(polygon) #returns FALSE
  polygon = rgeos::gBuffer(polygon, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(polygon) #returns TRUE

  #crop polygons to osm extent
  pol = raster::crop(polygon, raster::extent(maplatlonproj$bbox$p1[1], maplatlonproj$bbox$p2[1],
                                              maplatlonproj$bbox$p2[2], maplatlonproj$bbox$p1[2]))

  # fortify
  pol = pol %>%
    ggplot2::fortify(region = "l4_code")

  # convert id to factor for colouring in order
  cor7$id2 = as.factor(cor7$id)
  cor7$id2 = factor(cor7$id2, levels = rev(sort(as.numeric(levels(cor7$id2)))))

  map = ggplot2::ggplot() +
    #background polygon
    ggplot2::geom_polygon(data = pol, ggplot2::aes(x = long, y = lat, group = group), fill= "white", color = "black") +
    ggplot2::geom_polygon(data = cor7, ggplot2::aes(x = long, y = lat, group = group, fill = id2), alpha = 0.8) +
    ggplot2::coord_equal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),
                   legend.key.size = ggplot2::unit(1,"line")) +
    ggplot2::labs(fill = "count") +
    #colors
    ggplot2::scale_fill_viridis_d(option="viridis", alpha=0.4, direction =-1) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_obs_per_coral_poly_", species, "_poe_on.png")), map, width = 7, height = 5)


}

