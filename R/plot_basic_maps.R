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
    ggplot2::theme(legend.text = ggplot2::element_text(size = 11),
                   legend.key.size = ggplot2::unit(1.15,"line"),
                   legend.position = "bottom",
                   axis.title.x =  ggplot2::element_text(size = 15),
                   axis.title.y =  ggplot2::element_text(size = 15)) +
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

  #transects = raster::crop(transects, raster::extent(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1], does not work
  #                                                   maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]))

  transects_fortify = ggplot2::fortify(transects)

  pa2 = pa %>%
    ggplot2::fortify(region = "NAME")

  map = OpenStreetMap::autoplot.OpenStreetMap(maplatlon) + ##convert OSM to ggplot2 format
    ggplot2::geom_line(data = transects_fortify, ggplot2::aes(x = long, y = lat, group = id), col = "red", alpha = 0.5) + #transects in red
    ggplot2::geom_polygon(data = pa2, ggplot2::aes(x = long, y = lat), col = "yellow", alpha = 0.1) +
    #add north arrow
    ggsn::north(data = NULL, location = "bottomleft",
                x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                symbol = 4, scale = 0.08) +
    #add scalebar
    ggsn::scalebar(data = NULL, dist = dist, transform = TRUE, model = "WGS84", dist_unit = "km",
                   x.min = lon1, x.max = lon2, y.min = lat1, y.max = lat2,
                   anchor = c(x = lon1 + offset_lon , y = lat2 + offset_lat),
                   st.color = "white", box.fill = c("white", "white"), st.size = 3) +
    #limits on x and y axes
    #ggplot2::xlim(maplatlon$bbox$p1[1], maplatlon$bbox$p2[1]) +
    #ggplot2::ylim(maplatlon$bbox$p2[2], maplatlon$bbox$p1[2]) +
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

  ggplot2::ggsave(here::here("outputs/poe_on_effort/map_telemetry_date_poe_on.png"), map, width = 7, height = 5)

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

  ggplot2::ggsave(here::here("outputs/poe_on_effort/map_telemetry_video_id_separate_poe_on.png"), map, width = 7, height = 5)


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

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_telemetry_video_id_obs_", species, "_separate_poe_on_p1.png")), map1, width = 7, height = 5)
  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_telemetry_video_id_obs_", species, "_separate_poe_on_p2.png")), map2, width = 7, height = 5)
  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/map_telemetry_video_id_obs_", species, "_separate_poe_on_p3.png")), map3, width = 7, height = 5)


}

