#' Make map australia + nc : Figure 1
#'
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#'
#' @return
#' @export
#'
#' @examples
# map_australia_nc <- function(lon1, lon2, lat1, lat2){
#
#     monde <- raster::getData("countries")
#
#     png("outputs/map_australia_nc_raster.png", width = 480, height = 360) #to save + dev.off()
#
#     par(mar = c(0.5,0.5,0.5,0.5), bg = "white")
#     plot(monde, xlim = c(lon1 ,lon2), ylim = c(lat1,lat2), col = "#999999", border = "black", lwd = 1,)
#
#     #add arrow
#     #can't change arrow size
#     # cartography::north(pos = "topright", col = "black")
#
#     #add scalebar
#     raster::scalebar(1000, type='bar', below = "Km")
#
#     #add text
#     raster::text(132,-24.5, label = "AUSTRALIA", col = "white", cex = 1.8, font = 1)
#     #write in two lines : \n
#     raster::text(155.3,-17, label = "Coral\nSea", col = "black", cex = 1.6, font = 3)
#
#     dev.off()
# }


#' Make New caledonia map with Poé location
#'
#' @return
#' @export
#'

map_new_caledonia <- function(){

  # Use raster::getData("ISO3") to see codes
  new_caledonia <- raster::getData("GADM", country = "NCL", level = 1)

  # mar = c(bottom, left, top, right)
  par(mar = c(0.5,0.5,0.5,0.5), bg = "white")
  plot(new_caledonia, xlim = c(163.56,168.2), ylim = c(-22.62,-19.6), col ="#999999", border = "black", lwd = 1, cex = 2) ; box()
  #add arrow (can't change size)
  cartography::north(pos = "bottomleft", col = "black")
  #add scalebar
  raster::scalebar(100, type='bar', below = "Km", lonlat = TRUE, lwd = 100)
  raster::text(165.88,-19.8, label = "NEW CALEDONIA", col = "black", cex = 1.8, font = 1)
  #add text
  raster::text(164.7,-22.1, label = "Poé", col = "black", cex = 1.8, font = 1)

  dev.off()

}


#' Make open street map for study area
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return
#' @export
#'

osm_map <- function(lat1, lon1, lat2, lon2){

  #Open OSM
  map = OpenStreetMap::openmap(c(lat2,lon1), c(lat1,lon2), zoom = NULL,
                               type = c("bing"), #for satellite view
                               mergeTiles = TRUE)
  maplatlon = OpenStreetMap::openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  return(maplatlon)

}

#' Make open street map for nc area + australia
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return
#' @export
#'

map_nc <- function(lat1, lon1, lat2, lon2){

  #Open OSM
  map = OpenStreetMap::openmap(c(-10,140), c(-30,170), zoom = NULL,
                               type = c("bing"), #for satellite view
                               mergeTiles = TRUE)
  maplatlon_nc = OpenStreetMap::openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon_nc) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) +
  #add north arrow
  ggsn::north(data = NULL, location = "bottomleft",
              x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
              symbol = 1, scale = 0.15) +
  #add scalebar
  ggsn::scalebar(data = NULL, dist = 4, transform = TRUE, model = "WGS84", dist_unit = "km",
                   x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                   anchor = c(x = lon1 + offset_lon , y = lat2 + offset_lat),
                   st.color = "white", box.fill = c("white", "white"), st.size = 0.2)

  ggplot2::ggsave(here::here(paste0("outputs/map_nc_", extent, ".png")), map, width = 7, height = 5)


}


#' Make open street map for study area projected
#'
#' @return
#' @export
#'

osm_mapproj <- function(map){

  # Reproject OSM
  mapproj = OpenStreetMap::openproj(map, projection = "+init=epsg:3163")# NC projection

  return(mapproj)

}





#' Make alternate open street map for study area
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return
#' @export
#'

osm_map2 <- function(lat1, lon1, lat2, lon2){

  #Open OSM
  map = OpenStreetMap::openmap(c(lat2,lon1), c(lat1,lon2), zoom = NULL,
                               type = c("stamen-terrain"), #for terrain
                               mergeTiles = TRUE)
  maplatlon = OpenStreetMap::openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  return(maplatlon)

}


#' Make map of coral geomorphology polygons
#'
#' @param cor
#' @param maplatlon
#' @param attrib
#' @param extent
#'
#' @return
#' @export
#'

map_coral_poly <- function(maplatlon, cor, attrib, extent){

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE

  #crop polygons to osm extent
  cor3 = raster::crop(cor2, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                           maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  # fortify
  cor4 = cor3 %>%
    ggplot2::fortify(region = attrib)

  map = ggplot2::ggplot() +
    ggplot2::geom_polygon(data = cor4, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 4),
                   legend.key.size = ggplot2::unit(0.5,"line")) +
    ggplot2::labs(fill = attrib) +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor4$id)), c = 70, l = 50))) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here(paste0("outputs/map_coral_poly_",attrib,"_", extent, ".png")), map, width = 7, height = 5)

}

#' Make map of Allen coral geomorphology polygons benthic
#'
#' @param cor
#' @param maplatlon
#' @param extent
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#' @param offset_lon
#' @param offset_lat
#'
#' @return
#' @export
#'

map_allen_coral_poly <- function(maplatlon, cor, extent, lon1, lon2, lat1, lat2, dist, offset_lon, offset_lat){

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE

  #crop polygons to osm extent
  cor3 = raster::crop(cor2, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                           maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  # fortify
  cor4 = cor3 %>%
    ggplot2::fortify(region = "class")

  map = ggplot2::ggplot() +
    ggplot2::geom_polygon(data = cor4, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    # ggplot2::theme_minimal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 6),
                   legend.key.size = ggplot2::unit(0.8,"line"),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white")) +
    ggplot2::labs(fill = "") +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor4$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude")

    # ggplot2::theme(axis.title = ggplot2::element_blank())
    #add north arrow
    map = map + ggsn::north(data = NULL, location = "bottomleft",
              x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
              symbol = 3, scale = 0.08)
    #add scalebar
    map = map + ggsn::scalebar(data = NULL, dist = dist, transform = TRUE, model = "WGS84", dist_unit = "km",
                   x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                   anchor = c(x = lon1 + offset_lon , y = lat2 + offset_lat),
                   st.color = "black", box.fill = c("white", "white"), st.size = 2)

  ggplot2::ggsave(here::here(paste0("outputs/map_allen_coral_poly_", extent, ".png")), map, width = 7, height = 5)

}


#' Make map of Allen coral geomorphology polygons geomorphic
#'
#' @param cor
#' @param maplatlon
#' @param extent
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#' @param offset_lon
#' @param offset_lat
#'
#' @return
#' @export
#'

map_allen_coral_poly_geomorphic <- function(maplatlon, cor, extent, lon1, lon2, lat1, lat2, dist, offset_lon, offset_lat){

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE

  #crop polygons to osm extent
  cor3 = raster::crop(cor2, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                           maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  # fortify
  cor4 = cor3 %>%
    ggplot2::fortify(region = "class")

  map = ggplot2::ggplot() +
    ggplot2::geom_polygon(data = cor4, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    # ggplot2::theme_minimal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 6),
                   legend.key.size = ggplot2::unit(0.8,"line"),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white")) +
    ggplot2::labs(fill = "") +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor4$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  # ggplot2::theme(axis.title = ggplot2::element_blank())
  #add north arrow
  map = map + ggsn::north(data = NULL, location = "bottomleft",
                          x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                          symbol = 3, scale = 0.08)
  #add scalebar
  map = map + ggsn::scalebar(data = NULL, dist = dist, transform = TRUE, model = "WGS84", dist_unit = "km",
                             x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                             anchor = c(x = lon1 + offset_lon , y = lat2 + offset_lat),
                             st.color = "black", box.fill = c("white", "white"), st.size = 2)

  ggplot2::ggsave(here::here(paste0("outputs/map_allen_coral_poly_geomorphic_", extent, ".png")), map, width = 7, height = 5)

}


#' Make map of Allen coral geomorphology polygons with satellite view
#'
#' @param cor
#' @param maplatlon
#' @param extent
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#' @param offset_lon
#' @param offset_lat
#' @param pa
#' @param dist
#'
#' @return
#' @export
#'

map_allen_coral_poly_satellite <- function(maplatlon, pa, cor, extent, lon1, lon2, lat1, lat2, dist, offset_lon, offset_lat){

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE

  #crop polygons to osm extent
  cor3 = raster::crop(cor2, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                           maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  # fortify
  cor4 = cor3 %>%
    ggplot2::fortify(region = "class")

  #add mpa
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon)

  map = map +
    ggplot2::geom_polygon(data = cor4, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    # ggplot2::theme_minimal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 13),
                   legend.key.size = ggplot2::unit(1.15,"line"),
                   legend.position = "bottom",
                   axis.title.x =  ggplot2::element_blank(),
                   axis.title.y =  ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::labs(fill = "") +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor4$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    #add mpa
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0.1)

  ggplot2::ggsave(here::here(paste0("outputs/map_allen_coral_poly_satellite_", extent, ".png")), map, width = 7, height = 5)

}


#' Make map of Allen coral geomorphology polygons geomorphic with satellite view
#'
#' @param cor
#' @param maplatlon
#' @param extent
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#' @param offset_lon
#' @param offset_lat
#'
#' @return
#' @export
#'

map_allen_coral_poly_geomorphic_satellite <- function(maplatlon, cor, extent, lon1, lon2, lat1, lat2, dist, offset_lon, offset_lat){

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE

  #crop polygons to osm extent
  cor3 = raster::crop(cor2, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                           maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  # fortify
  cor4 = cor3 %>%
    ggplot2::fortify(region = "class")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon)

  map = map +
    ggplot2::geom_polygon(data = cor4, ggplot2::aes(x = long, y = lat, group = group, fill = id), alpha = 0.8) +
    # ggplot2::theme_minimal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 6),
                   legend.key.size = ggplot2::unit(0.8,"line")) +
    ggplot2::labs(fill = "") +
    #colors
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor4$id)), c = 70, l = 50))) +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  # ggplot2::theme(axis.title = ggplot2::element_blank())
  #add north arrow
  map = map + ggsn::north(data = NULL, location = "bottomleft",
                          x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                          symbol = 3, scale = 0.08)
  #add scalebar
  map = map + ggsn::scalebar(data = NULL, dist = dist, transform = TRUE, model = "WGS84", dist_unit = "km",
                             x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                             anchor = c(x = lon1 + offset_lon , y = lat2 + offset_lat),
                             st.color = "black", box.fill = c("white", "white"), st.size = 2)

  ggplot2::ggsave(here::here(paste0("outputs/map_allen_coral_poly_geomorphic_satellite_", extent, ".png")), map, width = 7, height = 5)

}




#' Make map of Allen coral geomorphology polygons with added open sea category
#'
#' @param cor
#' @param maplatlon
#' @param extent
#' @param lon1
#' @param lon2
#' @param lat1
#' @param lat2
#' @param pa
#'
#' @return
#' @export
#'

map_allen_coral_poly_coastline_with_open_sea <- function(maplatlon, pa, cor, extent, lon1, lon2, lat1, lat2){

  #crop coral polygons to osm extent
  cor2 = raster::crop(cor, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                          maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  #fortify coral polygons
  cor3 = cor2 %>%
    ggplot2::fortify(region = "class")

  #fortify mpa polygon
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")

  #read nc land (projection is Lambert new caledonia so needs reproject)
  land = sf::st_read(here::here("data", "nc_land", "NOUVELLE_CALEDONIE.shp"))
  sf::st_crs(land) <- 3163
  land = sf::st_transform(land,  crs = 4326)

  map = ggplot2::ggplot(land) +
    #land
    ggplot2::geom_sf(fill = "dark grey", color = "dark grey") +
    #add open sea category in light blue
    ggplot2::geom_rect(ggplot2::aes(xmin = 165.225, xmax = 165.45, ymin = -21.65, ymax = -21.62), fill = "light blue") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 165.225, xmax = 165.35, ymin = -21.62, ymax = -21.595), fill = "light blue") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 165.225, xmax = 165.25, ymin = -21.595, ymax = -21.52), fill = "light blue") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 165.25, xmax = 165.255, ymin = -21.60, ymax = -21.57), fill = "light blue") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 165.25, xmax = 165.28, ymin = -21.60, ymax = -21.587), fill = "light blue") +
    #add coral polygon
    ggplot2::geom_polygon(data = cor3, ggplot2::aes(x = long, y = lat, group = group, fill = id)) +
    #zoom on poe
    ggplot2::scale_x_continuous(limits = c(lon1, lon2), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(lat2, lat1), expand = c(0.0001, 0.0001)) +
    #scale bar
    ggplot2::scale_fill_manual(values = rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))) +
    #add mpa
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", size = 0.6, alpha = 0) +
    #plot parameters
    ggplot2::theme(legend.text = ggplot2::element_text(size = 13),
                   legend.key.size = ggplot2::unit(1.15,"line"),
                   legend.position = "bottom",
                   axis.title.x =  ggplot2::element_blank(),
                   axis.title.y =  ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(fill = "")

  ggplot2::ggsave(here::here(paste0("outputs/map_allen_coral_poly_coastline_", extent, ".png")), map, width = 7, height = 5)

}


#' Make map of coral cover raster
#'
#' @param cor
#' @param maplatlon
#' @param extent
#'
#' @return
#' @export
#'

map_coral_cover <- function(maplatlon, cor, extent){

  cor_spdf = as(cor, "SpatialPixelsDataFrame")
  cor_df = as.data.frame(cor_spdf)
  colnames(cor_df) <- c("coral_cover", "lon", "lat")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) +
    ggplot2::geom_tile(data=cor_df, ggplot2::aes(x = lon, y = lat, fill = coral_cover), alpha=0.4) +
    ggplot2::scale_fill_viridis_c(option="plasma", alpha=0.4) +
    #ggplot2::theme_minimal() +
    ggplot2::labs(x = "longitude", y = "latitude")

  ggplot2::ggsave(here::here(paste0("outputs/map_coral_cover_", extent, ".png")), map, width = 7, height = 5)

}



#' Make map of mpa
#'
#' @param maplatlon
#' @param extent
#' @param pa
#'
#' @return
#' @export
#'

map_mpa <- function(maplatlon, pa, extent){

  #crop polygons to osm extent
  pa2 = raster::crop(pa, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1],
                                        maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  # fortify and make short mpa name
  pa3 = pa2 %>%
    ggplot2::fortify(region = "NAME")  %>%
    dplyr::mutate(mpa_name = stringr::str_sub(id, 1, 30))

  map = ggplot2::ggplot() +
    ggplot2::geom_polygon(data = pa3, ggplot2::aes(x = long, y = lat, group = group, fill = mpa_name), alpha = 0.8) +
    #ggplot2::theme_minimal() +
    #legend
    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),
                   legend.key.size = ggplot2::unit(0.5,"line")) +
    #colors
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here(paste0("outputs/map_mpa_", extent, ".png")), map, width = 7, height = 5)

}



#' Make map of Poe mpa
#'
#' @param maplatlon
#' @param extent
#' @param pa
#'
#' @return
#' @export
#'

map_mpa_poe <- function(maplatlon, pa, extent){

  # fortify
  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0.1) +
    #ggplot2::theme_minimal() +
    #colors
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here(paste0("outputs/map_mpa_poe_", extent, ".png")), map, width = 7, height = 5)

}


#' Make map of transect lines
#'
#' @param maplatlon
#' @param transects
#' @param extent
#'
#' @return
#' @export
#'

map_transects <- function(maplatlon, transects, extent, just_poe = FALSE){

  #transects = raster::crop(transects, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1], does not work
  #                                                   maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  transects_fortify = ggplot2::fortify(transects)

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "red", alpha = 0.5) + #transects in red
    #ggplot2::geom_text(data = transects_fortify, ggplot2::aes(x = long, y = lat, label = id), hjust = 0, vjust = 0, size = 1, col = "red") + #add point transect numbers
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    #ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    #ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (just_poe == TRUE) {
    ggplot2::ggsave(here::here(paste0("outputs/map_poe_transects_", extent, ".png")), map, width = 7, height = 5)
  }else{
    ggplot2::ggsave(here::here(paste0("outputs/map_transects_", extent, ".png")), map, width = 7, height = 5)
  }

}




#' Make map of transect lines with scale bar and Poe mpa
#'
#' @param maplatlon
#' @param transects
#' @param extent
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#' @param dist
#' @param offset_lon
#' @param offset_lat
#' @param just_poe
#' @param pa
#'
#' @return
#' @export
#'

map_transects_scalebar_mpa <- function(maplatlon, transects, pa, extent, lat1, lon1, lat2, lon2, dist, offset_lon, offset_lat, just_poe = FALSE){

  transects_fortify = ggplot2::fortify(transects)

  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "white", alpha = 1) + #transects in white
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0) + #mpa polygon in yellow
    #add scalebar
    ggsn::scalebar(data = NULL, dist = 2.5, transform = TRUE, model = "WGS84", dist_unit = "km", height = 0.3,
                 x.min = 165.20, x.max = 165.30, y.min = -21.633, y.max = -21.640,
                 st.dist = 0.5, st.color = "white", box.color = "white", border.size = 0.5, box.fill = c("white", "white"), st.size = 3.4) +
    #north arrow
    ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                      height = grid::unit(1, "cm"),  width = grid::unit(1, "cm"),
                                      style = ggspatial::north_arrow_orienteering(line_col = "white"),
                                      text_col = "white") +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  if (just_poe == TRUE) {
    ggplot2::ggsave(here::here(paste0("outputs/map_poe_transects_", extent, "_scalebar_mpa.png")), map, width = 7, height = 5)
  }else{
    ggplot2::ggsave(here::here(paste0("outputs/map_transects_", extent, "_scalebar_mpa.png")), map, width = 7, height = 5)
  }

}





#' Make map of telemetry with one color per date for Poe on effort
#'
#' @param telem
#' @param maplatlon
#' @param extent
#'
#' @return
#' @export
#'

map_telemetry_date_poe_on <- function(maplatlon, telem){

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat, color = date), size = 0.01, alpha = 0.5) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=3))) + #increase dot size in legend
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here("outputs/map_telemetry_date_poe_on.png"), map, width = 7, height = 5)

}


#' Make map of telemetry with one color per video_id for Poe on effort
#'
#' @param telem
#'
#' @return
#' @export
#'

map_telemetry_video_id_separate_poe_on <- function(maplatlon, telem){

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::facet_wrap(~video_id, nrow = 5) + #facet per date
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  ggplot2::ggsave(here::here("outputs/map_telemetry_video_id_separate_poe_on.png"), map, width = 7, height = 5)


}




#' Make map of telemetry with one color per video_id with species observation for Poe on effort
#'
#' @param telem
#'
#' @return
#' @export
#'

map_telemetry_video_id_obs_separate_poe_on <- function(maplatlon, telem, telem_obs, species){

telem_obs %>%
    dplyr::filter(object == species) -> telem_obs

  #page 1
  map1 = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="red") +
    #ggplot2::facet_wrap(~video_id, nrow = 6) + #facet per date
    ggforce::facet_wrap_paginate(~video_id, nrow = 4, ncol = 5, page = 1) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  #page 2
  map2 = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="red") +
    #ggplot2::facet_wrap(~video_id, nrow = 6) + #facet per date
    ggforce::facet_wrap_paginate(~video_id, nrow = 4, ncol = 5, page = 2) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  #page 3
  map3 = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="red") +
    #ggplot2::facet_wrap(~video_id, nrow = 6) + #facet per date
    ggforce::facet_wrap_paginate(~video_id, nrow = 4, ncol = 5, page = 3) +
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  ggplot2::ggsave(here::here(paste0("outputs/map_telemetry_video_id_obs_", species, "_separate_poe_on_p1.png")), map1, width = 7, height = 5)
  ggplot2::ggsave(here::here(paste0("outputs/map_telemetry_video_id_obs_", species, "_separate_poe_on_p2.png")), map2, width = 7, height = 5)
  ggplot2::ggsave(here::here(paste0("outputs/map_telemetry_video_id_obs_", species, "_separate_poe_on_p3.png")), map3, width = 7, height = 5)


}

#' Make separate map of telemetry per date for all telemetry and telemetry on effort
#'
#' @param telem
#' @param maplatlon
#' @param extent
#'
#' @return
#' @export
#'

map_telemetry_date_separate_on_off <- function(maplatlon, telem, telem_on, extent, just_poe = FALSE){

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="red") +
    ggplot2::geom_point(data = telem_on, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::facet_wrap(~date, nrow = 2) + #facet per date
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  if (just_poe == TRUE) {
    ggplot2::ggsave(here::here(paste0("outputs/map_telemetry_poe_date_separate_on_off_", extent, ".png")), map, width = 7, height = 5)
  }else{
    ggplot2::ggsave(here::here(paste0("outputs/map_telemetry_date_separate_on_off_", extent, ".png")), map, width = 7, height = 5)
  }

}


#' Make map of telemetry with all species observations for Poe on effort
#'
#' @param telem
#' @param maplatlon
#' @param extent
#' @param telem_obs
#' @param just_poe
#'
#' @return
#' @export
#'

map_all_species_telemetry_poe_on <- function(maplatlon, telem, telem_obs){

  #select species observations and remove coral and plane
  telem_obs %>%
    dplyr::filter(!object %in% c("Coral", "Plane_shadow")) -> telem_obs

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat), size = 0.1, alpha = 0.5, col = "red") + ## all species obs in red
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here("outputs/map_all_species_telemetry_poe_on.png"), map, width = 7, height = 5)

}


#' Make map of telemetry with individual species observations for Poe on effort
#'
#' @param telem
#' @param maplatlon
#' @param extent
#' @param telem_obs
#' @param just_poe
#'
#' @return
#' @export
#'

map_indiv_species_telemetry_poe_on <- function(maplatlon, telem, telem_obs){

  #select species observations and remove coral and plane
  telem_obs %>%
    dplyr::filter(!object %in% c("Coral", "Plane_shadow")) -> telem_obs

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat, color = object), size = 0.1, alpha = 0.5) +
    ggplot2::guides(color = ggplot2::guide_legend("Species", override.aes = list(size=3))) + #change title and increase dot size in legend
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  ggplot2::ggsave(here::here("outputs/map_indiv_species_telemetry_poe_on.png"), map, width = 7, height = 5)

}


#' Make map of telemetry with individual species observations with separate map per species for Poe on effort
#'
#' @param telem
#' @param maplatlon
#' @param extent
#' @param telem_obs
#' @param just_poe
#'
#' @return
#' @export
#'

map_indiv_species_telemetry_separate_poe_on <- function(maplatlon, telem, telem_obs){

  #select species observations and remove coral and plane
  telem_obs %>%
    dplyr::filter(!object %in% c("Coral", "Plane_shadow")) -> telem_obs

  #create dataset grouped by image and species to get number of images per species
  telem_obs %>%
    dplyr::group_by(image_id, object) %>%
    dplyr::summarise(n = dplyr::n()) -> telem_species_per_image

  #define species labels including for each species number of individuals and number of images
  species_labels <- c(
    "Turtle" = paste0("Turtle ind=", nrow(telem_obs[telem_obs$object=="Turtle",]),
                      " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Turtle"]))),
    "Dugong_certain" = paste0("Dugong_certain ind=", nrow(telem_obs[telem_obs$object=="Dugong_certain",]),
                              " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dugong_certain"]))),
    "Dugong_probable" = paste0("Dugong_probable ind=", nrow(telem_obs[telem_obs$object=="Dugong_probable",]),
                               " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dugong_probable"]))),
    "Shark" = paste0("Shark ind=", nrow(telem_obs[telem_obs$object=="Shark",]),
                     " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Shark"]))),
    "Round_ray" = paste0("Round_ray ind=", nrow(telem_obs[telem_obs$object=="Round_ray",]),
                         " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Round_ray"]))),
    "Eagle_ray" = paste0("Eagle_ray ind=", nrow(telem_obs[telem_obs$object=="Eagle_ray",]),
                         " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Eagle_ray"]))),
    "Humpback_whale" = paste0("Humpback_whale ind=", nrow(telem_obs[telem_obs$object=="Humpback_whale",]),
                              " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Humpback_whale"]))),
    "Dolphin" = paste0("Dolphin ind=", nrow(telem_obs[telem_obs$object=="Dolphin",]),
                       " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Dolphin"]))),
    "Sea_snake" = paste0("Sea_snake ind=", nrow(telem_obs[telem_obs$object=="Sea_snake",]),
                         " imag=", length((telem_species_per_image$image_id[telem_species_per_image$object=="Sea_snake"]))))

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem_obs, ggplot2::aes(x = lon, y = lat), size = 0.1, col = "red", alpha = 0.5) +
    ggplot2::facet_wrap(~object, nrow = 3, labeller = ggplot2::as_labeller(species_labels)) + #facet per species
    #ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.01, alpha = 0.5) + #telem only
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size=7)) #facet wrap title size

  ggplot2::ggsave(here::here("outputs/map_indiv_species_telemetry_separate_poe_on.png"), map, width = 7, height = 5)

}


#' Make separate map of telemetry per date for Poe on effort
#'
#' @param telem
#' @param maplatlon
#' @param extent
#'
#' @return
#' @export
#'

map_telemetry_date_separate_poe_on <- function(maplatlon, telem){

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_point(data = telem, ggplot2::aes(x = lon, y = lat), size = 0.005, alpha = 0.5, col="white") +
    ggplot2::facet_wrap(~date, nrow = 2) + #facet per date
    #ggplot2::theme_minimal() +
    #limits on x and y axes
    ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank())

  ggplot2::ggsave(here::here("outputs/map_telemetry_date_separate_poe_on.png"), map, width = 7, height = 5)

}





#' barplot habitat proportion in and out of mpa
#'
#' @param df
#'
#' @return
#' @export
#'

barplot_habitat_proportion_in_out_mpa <- function(df){

  df %>%
    #select cells with effort
    dplyr::filter(length > 0) %>%
    #count proportions of habitat in/out mpa
    sf::st_drop_geometry() %>%
    dplyr::count(class, mpa_status) %>%
    dplyr::group_by(mpa_status) %>%
    dplyr::mutate(pct = n / sum(n)) -> df

  levels(df$mpa_status) <- c("Inside MPA", "Outside MPA")

  p <- ggplot2::ggplot(data = df, ggplot2::aes(y = pct, x = mpa_status, fill = class)) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    ggplot2::scale_fill_manual(labels = c("Coral/Algae", "Open Sea", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass"),
                               values = c(rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))[1], "light blue",
                                          rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))[2:6]),
                               name = "Habitat class") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(pct*100), '%')), position = ggplot2::position_stack(vjust = 0.5), size = 3) +
    ggplot2::scale_y_continuous(labels = c(0, 25, 50, 75, 100)) +
    ggplot2::ylab("Percentage") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 15),
                   axis.text.y =  ggplot2::element_text(size = 15),
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 12))

  ggplot2::ggsave(here::here("outputs",  "barplot_habitat_proportion_in_out_mpa.png"), p, width = 7, height = 5)

}




#' barplot habitat proportion in whole sampled area
#'
#' @param df
#'
#' @return
#' @export
#'

barplot_habitat_proportion <- function(df){

  df %>%
    #select cells with effort
    dplyr::filter(length > 0) %>%
    #count proportions of habitat in/out mpa
    sf::st_drop_geometry() %>%
    dplyr::count(class) %>%
    dplyr::mutate(pct = n / sum(n)) -> df

  #Dummy group variable
  df$row <- 1

  p <- ggplot2::ggplot(data = df, ggplot2::aes(y = pct, x = row, fill = class)) +
    ggplot2::geom_col(stat = "identity", position = "fill") +
    ggplot2::scale_fill_manual(labels = c("Coral/Algae", "Open Sea", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass"),
                               values = c(rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))[1], "light blue",
                                          rev(colorspace::qualitative_hcl(length(unique(cor3$id)), c = 70, l = 50))[2:6]),
                               name = "Habitat class") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(pct*100), '%')), position = ggplot2::position_stack(vjust = 0.5), size = 3) +
    ggplot2::scale_y_continuous(labels = c(0, 25, 50, 75, 100)) +
    ggplot2::ylab("Percentage") +
    ggplot2::xlab("Whole survey area") +
    ggplot2::theme(axis.title.x = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 15),
                   axis.text.y =  ggplot2::element_text(size = 15),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(size = 14),
                   legend.text = ggplot2::element_text(size = 12))

  ggplot2::ggsave(here::here("outputs",  "barplot_habitat_proportion.png"), p, width = 5, height = 5)

}
