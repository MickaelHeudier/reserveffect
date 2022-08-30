
#' Make dataframe per grid cell (incl. empty cells) of observations, track length and densities per date and per species for POe
#'
#' @param polyobs
#' @param polytracks
#' @param footprintwidth
#'
#' @return
#' @export
#'

make_df_all_species_poe <- function(polyobs, polytracks, footprintwidth){


  ### polyobs

  # replace NAs
  polyobs2 = polyobs %>%
    dplyr::mutate(date = as.character(date)) %>%
    tidyr::replace_na(list(n = 0, object = "unknown", date = "unknown"))

  # convert back to spatial object for plotting
  polyobs3 = sf::as_Spatial(polyobs2)

  #add coords
  polyobs3@data$lon = coordinates(polyobs3)[,1]
  polyobs3@data$lat = coordinates(polyobs3)[,2]

  #reformat
  polyobs3@data %>%
    #replace special character
    dplyr::mutate(date = stringr::str_replace_all(date, "-", "_")) %>%
    #reshape
    tidyr::pivot_wider(names_from = c(date, object), values_from = n, values_fill = 0, names_glue = "n_{object}_{date}") %>%
    #remove columns
    dplyr::select(c(-layer, -n_unknown_unknown)) %>%
    #calculate tot observation nb per species
    dplyr::mutate(n_Turtle = rowSums(dplyr::across(tidyselect::starts_with("n_Turtle")))) %>%
    dplyr::mutate(n_Dugong_certain = rowSums(dplyr::across(tidyselect::starts_with("n_Dugong_certain")))) %>%
    dplyr::mutate(n_Dugong_probable = rowSums(dplyr::across(tidyselect::starts_with("n_Dugong_probable")))) %>%
    dplyr::mutate(n_Eagle_ray= rowSums(dplyr::across(tidyselect::starts_with("n_Eagle_ray")))) %>%
    dplyr::mutate(n_Round_ray = rowSums(dplyr::across(tidyselect::starts_with("n_Round_ray")))) %>%
    dplyr::mutate(n_Manta_ray = rowSums(dplyr::across(tidyselect::starts_with("n_Manta_ray")))) %>%
    dplyr::mutate(n_Dolphin = rowSums(dplyr::across(tidyselect::starts_with("n_Dolphin")))) %>%
    dplyr::mutate(n_Shark = rowSums(dplyr::across(tidyselect::starts_with("n_Shark")))) -> polyobs4



  ### polytracks

  # replace NAs
  polytracks2 = polytracks %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::select(-layer.y, -layer.x) %>%
    dplyr::mutate(length = as.numeric(length)) %>%
    tidyr::replace_na(list(length = 0, date = "unknown"))

  # convert back to spatial object for plotting
  polytracks3 = sf::as_Spatial(polytracks2)

  #add coords
  polytracks3@data$lon = coordinates(polytracks3)[,1]
  polytracks3@data$lat = coordinates(polytracks3)[,2]

  #reformat
  polytracks3@data %>%
    #replace special character
    dplyr::mutate(date = stringr::str_replace_all(date, "-", "_")) %>%
    #reshape
    tidyr::pivot_wider(names_from = date, values_from = length, values_fill = 0, names_glue = "length_{date}") %>%
    #calculate tot length
    dplyr::mutate(length = rowSums(dplyr::across(tidyselect::starts_with("length")))) %>%
    #remove columns
    dplyr::select(c(-length_unknown)) -> polytracks4


  #join polyobs and polytracks and compute density per date and per species
  polyobs4 %>%
    #join
    dplyr::right_join(polytracks4, by = c("id"))  %>%
    #compute desnity per date and per species
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_07_24"),
                                .fns = function(x){1000 * x/(length_2021_07_24*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_07_29"),
                                .fns = function(x){1000 * x/(length_2021_07_29*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_08_08"),
                                .fns = function(x){1000 * x/(length_2021_08_08*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_08_21"),
                                .fns = function(x){1000 * x/(length_2021_08_21*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_09_05"),
                                .fns = function(x){1000 * x/(length_2021_09_05*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_09_12"),
                                .fns = function(x){1000 * x/(length_2021_09_12*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_09_14"),
                                .fns = function(x){1000 * x/(length_2021_09_14*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_10_06"),
                                .fns = function(x){1000 * x/(length_2021_10_06*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_10_18"),
                                .fns = function(x){1000 * x/(length_2021_10_18*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_10_23"),
                                .fns = function(x){1000 * x/(length_2021_10_23*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_10_31"),
                                .fns = function(x){1000 * x/(length_2021_10_31*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_06"),
                                .fns = function(x){1000 * x/(length_2021_11_06*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_10"),
                                .fns = function(x){1000 * x/(length_2021_11_10*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_11"),
                                .fns = function(x){1000 * x/(length_2021_11_11*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_14"),
                                .fns = function(x){1000 * x/(length_2021_11_14*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_16"),
                                .fns = function(x){1000 * x/(length_2021_11_16*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_23"),
                                .fns = function(x){1000 * x/(length_2021_11_23*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_24"),
                                .fns = function(x){1000 * x/(length_2021_11_24*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_26"),
                                .fns = function(x){1000 * x/(length_2021_11_26*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::ends_with("2021_11_29"),
                                .fns = function(x){1000 * x/(length_2021_11_29*footprintwidth)}, #density in indiv / ha
                                .names = "density_{.col}")) %>%
    #compute density per species
    dplyr::mutate(density_Turtle = 1000 * n_Turtle / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Dugong_certain = 1000 * n_Dugong_certain / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Dugong_probable = 1000 * n_Dugong_probable / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Round_ray = 1000 * n_Round_ray / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Eagle_ray = 1000 * n_Eagle_ray / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Manta_ray = 1000 * n_Manta_ray / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Dolphin = 1000 * n_Dolphin / (length*footprintwidth)) %>%  #density in indiv / ha
    dplyr::mutate(density_Shark = 1000 * n_Shark / (length*footprintwidth)) %>%  #density in indiv / ha

    #clean
    dplyr::select(-tidyselect::starts_with("density_length")) %>%
    dplyr::rename(lon = lon.x, lat = lat.x) %>%
    dplyr::select(-lat.y, -lon.y) -> result


  #clean column names
  names(result) <- gsub(x = names(result), pattern = "density_n", replacement = "density")


  # NB 0/0 returns nan (no observation and no effort)


  return(result)

}




#' Intersect desnity dataframe with coral polygon (ie intersection of points with polygons)
#'
#' @param df
#' @param coralpoly
#'
#' @return
#' @export
#'

intersect_df_coral_poly <- function(df, coralpoly){

  # convert dataframe to sf object
  points = sf::st_as_sf(x = df,
                        coords = c("lon", "lat"),
                        crs = "+init=epsg:3163")

  # check if there are invalid coral polygon geometries & correct that
  rgeos::gIsValid(coralpoly) #returns FALSE
  coralpoly2 = rgeos::gBuffer(coralpoly, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(coralpoly2) #returns TRUE

  # convert coral polygon to sf object
  coralsf = sf::st_as_sf(coralpoly2)

  #project coral polygon
  coralproj = sf::st_transform(coralsf, CRS("+init=epsg:3163")) #NC projection

  # Intersection between coral polygon and points
  # attention: only cells that fall within coral polygon are returned
  pointswithcoral = sf::st_intersection(x = points, y = coralproj) # returns non empty intersections

  # retrieve cells that fall beyond coral polygon
  all_ids = points$id
  coral_ids = pointswithcoral$id
  noncoral_ids = setdiff(all_ids, coral_ids)

  pointswithoutcoral = points[points$id %in% noncoral_ids, ]
  pointswithoutcoral[names(coralproj)[!names(coralproj) == "geometry"]] = "open_sea"

  #bind
  res = rbind(pointswithcoral, pointswithoutcoral)

  #clean columns
  res %>%
    dplyr::select(- c(Shape__Are, Shape__Len, objectid, gridcode, shape_leng)) -> res

  return(res)

}


#' Intersect desnity dataframe with Allen coral polygon (ie intersection of points with polygons)
#'
#' @param df
#' @param coralpoly
#'
#' @return
#' @export
#'

intersect_df_allen_coral_poly <- function(df, coralpoly){

  # convert dataframe to sf object
  points = sf::st_as_sf(x = df,
                        coords = c("lon", "lat"),
                        crs = "+init=epsg:3163")

  # convert coral polygon to sf object
  coralsf = sf::st_as_sf(coralpoly)

  #project coral polygon
  coralproj = sf::st_transform(coralsf, CRS("+init=epsg:3163")) #NC projection

  # Intersection between coral polygon and points
  # attention: only cells that fall within coral polygon are returned
  pointswithcoral = sf::st_intersection(x = points, y = coralproj) # returns non empty intersections

  # retrieve cells that fall beyond coral polygon
  all_ids = points$id
  coral_ids = pointswithcoral$id
  noncoral_ids = setdiff(all_ids, coral_ids)

  pointswithoutcoral = points[points$id %in% noncoral_ids, ]
  pointswithoutcoral[names(coralproj)[!names(coralproj) == "geometry"]] = "Deep_sea"

  #bind
  res = rbind(pointswithcoral, pointswithoutcoral)

  return(res)

}


#' Intersect desnity dataframe with mpa polygon (ie intersection of points with polygons)
#'
#' @param df
#' @param mpa_poly
#'
#' @return
#' @export
#'

intersect_df_mpa_poly <- function(df, mpa_poly){

  # convert dataframe to sf object
  points = sf::st_as_sf(x = df,
                        coords = c("lon", "lat"),
                        crs = "+init=epsg:3163")

  # convert mpa polygon to sf object
  mpasf = sf::st_as_sf(mpa_poly)

  #project mpa polygon
  mpaproj = sf::st_transform(mpasf, CRS("+init=epsg:3163")) #NC projection

  # Intersection between mpa polygon and points
  # attention: only cells that fall within mpa polygon are returned
  pointswithmpa = sf::st_intersection(x = points, y = mpaproj) # returns non empty intersections

  # retrieve cells that fall beyond mpa polygon
  all_ids = points$id
  mpa_ids = pointswithmpa$id
  nonmpa_ids = setdiff(all_ids, mpa_ids)

  pointswithoutmpa = points[points$id %in% nonmpa_ids, ]
  pointswithoutmpa[names(mpaproj)[!names(mpaproj) == "geometry"]] = "no_mpa"

  #bind
  res = rbind(pointswithmpa, pointswithoutmpa)

  #clean columns
  res %>%
    dplyr::select(- c("WDPAID", "WDPA_PID", "PA_DEF", "ORIG_NAME", "DESIG", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "INT_CRIT",
                      "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE", "NO_TK_AREA",
                      "OWN_TYPE",   "MANG_AUTH",  "MANG_PLAN",  "VERIF",  "METADATAID", "SUB_LOC",  "PARENT_ISO", "ISO3",
                      "SUPP_INFO",  "CONS_OBJ",  "STATUS",  "STATUS_YR","GOV_TYPE" )) %>%
    dplyr::mutate(NAME = forcats::as_factor(NAME)) %>%
    dplyr::mutate(mpa_status = forcats::fct_recode(NAME, mpa = "PoÃ©")) %>%
    #add new column base on mpa status and coral habitat
    dplyr::mutate(mpa_status_l4_attrib = paste0(mpa_status, "_", l4_attrib)) -> res

  return(res)

}


#' Intersect desnity dataframe with mpa polygon (ie intersection of points with polygons) with data Allen
#'
#' @param df
#' @param mpa_poly
#'
#' @return
#' @export
#'

intersect_df_mpa_allen_poly <- function(df, mpa_poly){

  # convert dataframe to sf object
  points = sf::st_as_sf(x = df,
                        coords = c("lon", "lat"),
                        crs = "+init=epsg:3163")

  # convert mpa polygon to sf object
  mpasf = sf::st_as_sf(mpa_poly)

  #project mpa polygon
  mpaproj = sf::st_transform(mpasf, CRS("+init=epsg:3163")) #NC projection

  # Intersection between mpa polygon and points
  # attention: only cells that fall within mpa polygon are returned
  pointswithmpa = sf::st_intersection(x = points, y = mpaproj) # returns non empty intersections

  # retrieve cells that fall beyond mpa polygon
  all_ids = points$id
  mpa_ids = pointswithmpa$id
  nonmpa_ids = setdiff(all_ids, mpa_ids)

  pointswithoutmpa = points[points$id %in% nonmpa_ids, ]
  pointswithoutmpa[names(mpaproj)[!names(mpaproj) == "geometry"]] = "no_mpa"

  #bind
  res = rbind(pointswithmpa, pointswithoutmpa)

  #clean columns
  res %>%
    dplyr::select(- c("WDPAID", "WDPA_PID", "PA_DEF", "ORIG_NAME", "DESIG", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "INT_CRIT",
                      "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE", "NO_TK_AREA",
                      "OWN_TYPE",   "MANG_AUTH",  "MANG_PLAN",  "VERIF",  "METADATAID", "SUB_LOC",  "PARENT_ISO", "ISO3",
                      "SUPP_INFO",  "CONS_OBJ",  "STATUS",  "STATUS_YR","GOV_TYPE" )) %>%
    dplyr::mutate(NAME = forcats::as_factor(NAME)) %>%
    dplyr::mutate(mpa_status = forcats::fct_recode(NAME, mpa = "PoÃ©"))  -> res

  return(res)

}





#' Barplot of mean density per coral habitat with data Allen
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

barplot_density_allen_coral <- function(df, species){

  #mean density per coral hab for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste("class"))) %>%
    dplyr::select(c(density, coral_hab)) %>%
    dplyr::group_by(coral_hab) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = as.factor(new$coral_hab))

  #rename deep_Sea
  levels(new$coral_hab) <- c("Coral/Algae", "Open Sea", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass")

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = coral_hab, y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = coral_hab, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::xlab("") +
    ggplot2::ylab("Density") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1, size =12),
                   axis.title.y = ggplot2::element_text(size = 15),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"))


  ggplot2::ggsave(here::here(paste0("outputs/barplot_density_allen_coral_", species, ".png")), p, width = 7, height = 5)

}






#' Barplot of mean density per mpa status (in/out mpa)
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

barplot_density_mpa <- function(df, species){

  #mean density and se for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::group_by(mpa_status) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   mpa_status = as.factor(new$mpa_status))

  levels(new$mpa_status) <- c("Inside MPA", "Outside MPA")


  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = mpa_status, y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = mpa_status, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::xlab("") +
    ggplot2::ylab("Density") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1, size =12),
                   axis.title.y = ggplot2::element_text(size = 15),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"))


  ggplot2::ggsave(here::here(paste0("outputs/barplot_density_mpa_", species, ".png")), p, width = 7, height = 5)

}




#' Barplot of mean density per coral habitat with data Allen with image
#'
#' @param df
#' @param species
#' @param img
#'
#' @return
#' @export
#'

barplot_density_allen_coral_image <- function(df, species, img){

  #mean density per coral hab for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste("class"))) %>%
    dplyr::select(c(density, coral_hab)) %>%
    dplyr::group_by(coral_hab) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = as.factor(new$coral_hab))

  #rename deep_Sea
  levels(new$coral_hab) <- c("Coral/Algae", "Open Sea", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass")

  # parameters for image placement
  if (species == "Dugong_certain"){
    ylim = 0.016
    a = 5
    b = 7.8
    c = 0.010
    d = 0.018
    species_title <- "Dugong"
  }
  if (species == "Turtle"){
    ylim = 0.013
    a = 5.4
    b = 8.2
    c = 0.010
    d = 0.014
    species_title <- "Sea turtle"
  }
  if (species == "Shark"){
    ylim = 0.005
    a = 5
    b = 7.8
    c = 0.0035
    d = 0.0055
    species_title <- "Shark"
  }
  if (species == "Round_ray"){
    ylim = 0.012
    a = 5.4
    b = 8.3
    c = 0.0085
    d = 0.0125
    species_title <- "Dasyatidae"
  }
  if (species == "Eagle_ray"){
    ylim = 0.006
    a = 5.4
    b = 8
    c = 0.004
    d = 0.006
    species_title <- "Myliobatidae"
  }


  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = coral_hab, y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = coral_hab, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::xlab("Habitat class") +
    ggplot2::ylim(c(0, ylim)) +
    ggplot2::ylab("Density") +
    ggplot2::ggtitle(species_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1, size =12),
                   axis.title.y = ggplot2::element_text(size = 15),
                   axis.title.x = ggplot2::element_text(size = 15),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "white")) +
   #add megafaune image
   ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)

  ggplot2::ggsave(here::here(paste0("outputs/barplot_density_allen_coral_", species, "_image.png")), p, width = 7, height = 5)

}







#' Barplot of mean density per mpa status (in/out mpa) with image
#'
#' @param df
#' @param species
#' @param img
#'
#' @return
#' @export
#'

barplot_density_mpa_image <- function(df, species, img){

  #mean density and se for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::group_by(mpa_status) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   mpa_status = as.factor(new$mpa_status))

  levels(new$mpa_status) <- c("Inside MPA", "Outside MPA")

  # parameters for image placement
  if (species == "Dugong_certain"){
    a = 1.8
    b = 2.8
    c = 0.0065
    d = 0.0115
    species_title <- "Dugong"
  }
  if (species == "Turtle"){
    a = 1.8
    b = 2.8
    c = 0.0055
    d = 0.0085
    species_title <- "Sea turtle" 
  }
  if (species == "Shark"){
    a = 1.8
    b = 2.8
    c = 0.002
    d = 0.0035
    species_title <- "Shark"
  }
  if (species == "Round_ray"){
    a = 1.8
    b = 2.8
    c = 0.003
    d = 0.005
    species_title <- "Dasyatidae" 
  }
  if (species == "Eagle_ray"){
    a = 1.8
    b = 2.8
    c = 0.002
    d = 0.0035
    species_title <- "Myliobatidae" 
  }

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = mpa_status, y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = mpa_status, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::xlab("") +
    ggplot2::ylab("Density") +
    ggplot2::ggtitle(species_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size =12),
                   axis.title.y = ggplot2::element_text(size = 15),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white")) +
  #add megafaune image
  ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)


  ggplot2::ggsave(here::here(paste0("outputs/barplot_density_mpa_", species, "_image.png")), p, width = 7, height = 5)

}



#' Barplot of mean density per coral habitat and mpa status
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

barplot_density_allen_coral_mpa <- function(df, species){

  #mean density per mpa status and coral hab for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste("class"))) %>%
    dplyr::group_by(coral_hab, mpa_status) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = as.factor(new$coral_hab),
                   mpa_status = as.factor(new$mpa_status))

  #rename deep_Sea
  levels(new$coral_hab) <- c("Coral/Algae", "Open Sea", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass")
  levels(new$mpa_status) <- c("Inside MPA", "Outside MPA")

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = coral_hab, y = mean_dens, fill = mpa_status)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(x = coral_hab, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, size=1.3,
                           position = ggplot2::position_dodge(0.9), colour = "orange") +
    ggplot2::scale_fill_manual(values = c("Outside MPA" = "grey20", "Inside MPA" = "yellow")) +
    ggplot2::xlab("") +
    ggplot2::ylab("Density") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1, size =12),
                   axis.title.y = ggplot2::element_text(size = 15),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"))

  ggplot2::ggsave(here::here(paste0("outputs/barplot_density_allen_coral_mpa_", species, ".png")), p, width = 7, height = 5)

}






#' Barplot of mean density per coral habitat and mpa status with image
#'
#' @param df
#' @param species
#' @param img
#'
#' @return
#' @export
#'

barplot_density_allen_coral_mpa_image <- function(df, species, img){

  #mean density per mpa status and coral hab for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste("class"))) %>%
    dplyr::group_by(coral_hab, mpa_status) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = as.factor(new$coral_hab),
                   mpa_status = as.factor(new$mpa_status))

  #rename deep_Sea
  levels(new$coral_hab) <- c("Coral/Algae", "Open Sea", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass")
  levels(new$mpa_status) <- c("Inside MPA", "Outside MPA")

  # parameters for image placement
  if (species == "Dugong_certain"){
    a = 5.3
    b = 8
    c = 0.04
    d = 0.067
    species_title <- "Dugong" 
  }
  if (species == "Turtle"){
    a = 0.5
    b = 2
    c = 0.014
    d = 0.021
    species_title <- "Sea turtle" 
  }
  if (species == "Shark"){
    a = 5.3
    b = 8
    c = 0.006
    d = 0.0084
    species_title <- "Shark" 
  }
  if (species == "Round_ray"){
    a = 5.3
    b = 8
    c = 0.011
    d = 0.015
    species_title <- "Dasyatidae" 
  }
  if (species == "Eagle_ray"){
    a = 5.3
    b = 8
    c = 0.0085
    d = 0.012
    species_title <- "Myliobatidae" 
  }

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = coral_hab, y = mean_dens, fill = mpa_status)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(x = coral_hab, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, size=1.3,
                           position = ggplot2::position_dodge(0.9), colour = "orange") +
    ggplot2::scale_fill_manual(values = c("Outside MPA" = "grey20", "Inside MPA" = "yellow")) +
    ggplot2::xlab("Habitat class") +
    ggplot2::ylab("Density") +
    ggplot2::ggtitle(species_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1, size =12),
                   axis.title.x = ggplot2::element_text(size = 15),
                   axis.title.y = ggplot2::element_text(size = 15),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black",size = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white")) +
  #add megafaune image
  ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)

  ggplot2::ggsave(here::here(paste0("outputs/barplot_density_allen_coral_mpa_", species, "_image.png")), p, width = 7, height = 5)

}





#' Test hyp that median densities in mpa and outside mpa are different
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_in_out_mpa <- function(df, species){

  ##http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) -> df

  cat("--------------------------\n")
  cat(species)

  print(wilcox.test(density ~ mpa_status, data = df, exact = FALSE))

  print("if p-value of the test is less than the significance level alpha = 0.05
  We can conclude that the median desnity in mpa is significantly different
  from the median density outside mpa with a p-value of xxx.")

}



#' Test hyp that median densitiy in mpa is greater than outside mpa
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_in_out_mpa_greater <- function(df, species){

  ##http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) -> df

  cat("--------------------------\n")
  cat(species)

  print(wilcox.test(density ~ mpa_status, data = df,  exact = FALSE, alternative = "greater"))

  print("if p-value of the test is less than the significance level alpha = 0.05
  We can conclude that the median desnity in mpa is significantly greater
  than the median density outside mpa with a p-value of xxx")

}



#' Test hyp that median densities between habitat types are different
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_between_habitats <- function(df, species, l_attrib){

  ##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_attrib = get(paste0(l_attrib))) %>%
    dplyr::mutate(coral_attrib = forcats::as_factor(coral_attrib)) -> df

  cat("--------------------------")
  cat(species, "and", l_attrib)

  kruskal.test(density ~ coral_attrib, data = df)

  print("if p-value of the test is less than the significance level alpha = 0.05
  We can conclude that desnities are significantly different between
  habitat types with a p-value of xxx.")

}


#' Test hyp that median densities between habitat types are different with data Allen
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_between_allen_habitats <- function(df, species){

  ##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_class = get(paste0("class"))) %>%
    dplyr::mutate(coral_class = forcats::as_factor(coral_class)) -> df

  cat("--------------------------\n")
  cat(species, "and", "class")

  print(kruskal.test(density ~ coral_class, data = df))

  print("if p-value of the test is less than the significance level alpha = 0.05
  We can conclude that desnities are significantly different between
  habitat types with a p-value of xxx.")

}


#' Test hyp that median densities between habitat types are different pairwise
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_between_habitats_pairwise <- function(df, species, l_attrib){

  ##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_attrib = get(paste0(l_attrib))) %>%
    dplyr::mutate(coral_attrib = forcats::as_factor(coral_attrib)) %>%
    tidyr::drop_na(density)  -> df

  cat("--------------------------")
  cat(species, "and", l_attrib)

  pairwise.wilcox.test(df$density, df$coral_attrib, p.adjust.method = "BH")

  print("if p-value of a pairwise comparison is less than the significance level alpha = 0.05
  We can conclude that these levels are significantly different with a p-value of xxx")

}


#' Test hyp that median densities between habitat types are different pairwise with data Allen
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_between_allen_habitats_pairwise <- function(df, species){

  ##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_class = get(paste0("class"))) %>%
    dplyr::mutate(coral_class = forcats::as_factor(coral_class)) %>%
    tidyr::drop_na(density)  -> df

  cat("--------------------------")
  cat(species, "and", "class")

  print(pairwise.wilcox.test(df$density, df$coral_class, p.adjust.method = "BH"))

  print("if p-value of a pairwise comparison is less than the significance level alpha = 0.05
  We can conclude that these levels are significantly different with a p-value of xxx")

}


#' Test hyp that median densities between habitat types combined with mpa statusare different
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_between_combined_habitats_mpa_status <- function(df, species, l_attrib){

  ##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(mpa_status_coral_hab = forcats::as_factor(get(paste0("mpa_status_", l_attrib))))  %>%
    tidyr::drop_na(density)  -> df

  cat("--------------------------")
  cat(species, "and", l_attrib, "combined with mpa status")

  print(kruskal.test(density ~ mpa_status_coral_hab, data = df))

  print("if p-value of the test is less than the significance level alpha = 0.05
  #We can conclude that desnities are significantly different between
  #combinations of habitat types and mpa status with a p-value of xxx")

  cat("--------------pairwise")
  pairwise.wilcox.test(df$density, df$mpa_status_coral_hab, p.adjust.method = "BH")

  print("if p-value of a pairwise comparison is less than the significance level alpha = 0.05
  We can conclude that these levels are significantly different with a p-value of xxx")

}

#' Boxplot of mean density per mpa status and coral habitat (0s removed) with data Allen and megafauna image LOG
#'
#' @param df
#' @param img
#' @param species

boxplot_density_allen_coral_mpa_with_megafauna_image_log <- function(df, species, img){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species)))  %>%
    #exclude 0s
    dplyr::filter(density1>0) %>%
    dplyr::mutate(density2 = log(density1 * 100)) %>%
    # dplyr::mutate(mpa_status_allen_coral_hab = forcats::as_factor(get(paste0("mpa_status_", "class")))) %>%

    #ordonate class
    dplyr::mutate(class2 = factor(class, levels = c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"))) -> df_no0

  #change decimal number
  # scaleFUN <- function(density) sprintf("%.3f", density)

  df %>%
    dplyr::mutate(nb_ind = get(paste0("n_", species))) %>%
    dplyr::mutate(class2 = factor(class, levels = c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"))) %>%
    dplyr::group_by(mpa_status, class2) %>%
    dplyr::summarise(nb_ind_tot = sum(nb_ind)) -> df_nb_ind

  # tbl_nb_ind <- table(df_nb_ind$mpa_status, df_nb_ind$class2)

  ##test
  df_nb_ind_tot <- data.frame(reserve = df_nb_ind$mpa_status,
                        class = df_nb_ind$class2,
                        nb_ind = df_nb_ind$nb_ind_tot)

  #conditions
  if (species == "Dugong_certain") {
    a = 4
    b = 6.6
    c = 3.60
    d = 6.55
    species_title = "Dugong"
    X1 = NULL
    X2 = NULL
    X3 = NULL
    X4 = NULL
    X5 = NULL
    X6 = 3.8
    X7 = 4.8
    X8 = 1
    X9 = 2
    X10 = 3
    X11 = NULL
    X12 = NULL
    X13 = 4.2
    X14 = 5.2
    X15 = 5.5
    Y1 = NULL
    Y2 = NULL
    Y3 = NULL
    Y4 = NULL
    Y5 = NULL
    Y6 = 5.66
    Y7 = -1.1
    Y8 = -0.3
    Y9 = 0.4
    Y10 = 0.8
    Y11 = NULL
    Y12 = NULL
    Y13 = 0.3
    Y14 = -0.5
    Y15 = 6
  }
  if (species == "Turtle") {
    a = 5.8
    b = 8.3
    c = 1.2
    d = 6.1
    species_title = "Sea Turtle"
    X1 = 0.8
    X2 = 1.8
    X3 = 2.8
    X4 = NULL
    X5 = 4.8
    X6 = 5.8
    X7 = 6.8
    X8 = 1.2
    X9 = 2.2
    X10 = 3.2
    X11 = 4
    X12 = 5.2
    X13 =6.2
    X14 = 7.2
    X15 = 8
    Y1 = 3.1
    Y2 = 2.1
    Y3 = 2.75
    Y4 = NULL
    Y5 = 3.55
    Y6 = 2.35
    Y7 = 2
    Y8 = 2.1
    Y9 = 1.35
    Y10 = 2.3
    Y11 = 0.5
    Y12 = 3.1
    Y13 = 1.6
    Y14 = 2.1
    Y15 = 4
  }
  if (species == "Shark") {
    a = 6
    b = 8.3
    c = 1.2
    d = 3.9
    species_title = "Shark"
    X1 = 1
    X2 = 1.8
    X3 = 2.8
    X4 = NULL
    X5 = 4.8
    X6 = 5.8
    X7 = 6.8
    X8 = NULL
    X9 = 2.2
    X10 = 3.2
    X11 = 4
    X12 = 5.2
    X13 = 6.2
    X14 = 7.2
    X15 = 8
    Y1 = 1.5
    Y2 = 2.1
    Y3 = 1.2
    Y4 = NULL
    Y5 = 1.6
    Y6 = -0.5
    Y7 = 1.5
    Y8 = NULL
    Y9 = -0.4
    Y10 = 0.5
    Y11 = 0.1
    Y12 = -0.3
    Y13 = -0.38
    Y14 = 1.4
    Y15 = 3
  }
  if (species == "Round_ray") {
    a = 6
    b = 7.9
    c = 2.
    d = 3.3
    species_title = "Dasyatidae ray"
    X1 = 1
    X2 = 1.8
    X3 = 2.8
    X4 = NULL
    X5 = 4.8
    X6 = NULL
    X7 = 7
    X8 = NULL
    X9 = 2.2
    X10 = 3.2
    X11 = 4
    X12 = 5.2
    X13 = 6
    X14 = NULL
    X15 = 8
    Y1 = 1.3
    Y2 = 1.6
    Y3 = 0.5
    Y4 = NULL
    Y5 = 2.85
    Y6 = NULL
    Y7 = 1.5
    Y8 = NULL
    Y9 = 2.3
    Y10 = 2.1
    Y11 = 0.45
    Y12 = 2.4
    Y13 = 0.1
    Y14 = NULL
    Y15 = 3
  }
  if (species == "Eagle_ray") {
    a = 5.475
    b = 7.335
    c = 2.7
    d = 4.9
    species_title = "Myliobatidae ray"
    X1 = NULL
    X2 = 0.8
    X3 = 1.8
    X4 = 2.8
    X5 = 3.8
    X6 = 4.8
    X7 = 5.8
    X8 = NULL
    X9 = 1.2
    X10 = 2.2
    X11 = 3.2
    X12 = 4.2
    X13 = 5.2
    X14 = 6.2
    X15 = 7
    Y1 = NULL
    Y2 = 1.7
    Y3 = 2.2
    Y4 = 0.65
    Y5 = 1.65
    Y6 = 1.95
    Y7 = 3.3
    Y8 = NULL
    Y9 = 0.8
    Y10 = 0.2
    Y11 = 0.9
    Y12 = 1.6
    Y13 = 1.5
    Y14 = 1
    Y15 = 4
  }


  # p = ggplot2::ggplot(data = df_no0, ggplot2::aes(x = density, y = class)) + ggplot2::geom_boxplot(ggplot2::aes(col=class)) +
  #   ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = 4, xmax = 6, ymin = 0.9, ymax = 1.4)

  p = ggpubr::ggboxplot(df_no0, x = "class2", y = "density2", xlab = "", ylab = "Log density (indiv / 100 ha)",fill = "mpa_status")

  p = ggpubr::ggpar(p, orientation = "horizontal")
  #legend
  p = ggpubr::ggpar(p,legend = "right", legend.title = "", font.legend = c(10, "plain", "black"))
  #Title
  p = ggpubr::ggpar(p,
        title = species_title,
        font.main = c(14, face = "bold"),
        font.x = c(14, "plain", "black"),
        font.y = c(14, "plain", "black"))

  p = p + ggplot2::annotate("text", x = X1, y = Y1, label = paste("n =", df_nb_ind_tot[1,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X2, y = Y2, label = paste("n =",df_nb_ind_tot[2,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X3, y = Y3, label = paste("n =",df_nb_ind_tot[3,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X4, y = Y4, label = paste("n =",df_nb_ind_tot[4,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X5, y = Y5 , label = paste("n =",df_nb_ind_tot[5,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X6, y = Y6, label = paste("n =",df_nb_ind_tot[6,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X7, y = Y7 , label = paste("n =",df_nb_ind_tot[7,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X8, y = Y8, label = paste("n =",df_nb_ind_tot[8,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X9, y = Y9, label = paste("n =",df_nb_ind_tot[9,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X10, y = Y10 , label = paste("n =",df_nb_ind_tot[10,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X11, y = Y11, label = paste("n =",df_nb_ind_tot[11,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X12, y = Y12 , label = paste("n =",df_nb_ind_tot[12,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X13, y = Y13, label = paste("n =",df_nb_ind_tot[13,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X14, y = Y14, label = paste("n =",df_nb_ind_tot[14,3]), size = 4.2, fontface = "italic") +
    ggplot2::annotate("text", x = X15, y = Y15, label = "", size = 4.2, fontface = "italic")


  #add megafaune image
  p = p + ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)

  #it's possible to add ggplot2 function to ggpubr
  p = p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
                         legend.text = ggplot2::element_text(size = 11),
                         legend.title = ggplot2::element_text(size = 13))

  #rename legend + color
  p = p + ggplot2::scale_fill_manual(labels = c("MPA", "No MPA"), values = c("yellow", "steelblue"))



  #rename class Allen
  p = p + ggplot2::scale_x_discrete(breaks=c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"),
                           labels=c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal mats", "          Open sea", "Coral/Algae"))

  # #graduations
  # p = p + ggplot2::scale_y_continuous(labels = scaleFUN)

ggplot2::ggsave(here::here(paste0("outputs/boxplot_density_allen_coral_", "mpa_", species, "_with_megafauna_image_LOG", ".png")), p, width = 7, height = 5)


}


#' Make two-way anova for density per mpa status and coral habitat and barplot of the result
#'
#' @param df
#' @param species
#' @param coral_attrib
#'
#' @return
#' @export
#'

make_anova_barplot_coral_mpa <- function(df, species, coral_attrib){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste(coral_attrib))) -> df

  #remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  df %>%
    dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df

  #eliminate outliers
  outliers <- boxplot(df$density[df$density > 0], plot=FALSE)$out
  df %>%
    dplyr::filter(!density %in% outliers) ->  df

  #anova for unbalanced design
  #http://www.sthda.com/english/wiki/two-way-anova-test-in-r#assumptions-of-two-way-anova-test

  anova <- aov(density ~ mpa_status * coral_hab, data = df)
  res <- car::Anova(anova, type = "II") #https://towardsdatascience.com/anovas-three-types-of-estimating-sums-of-squares-don-t-make-the-wrong-choice-91107c77a27a
  # type = "III" returns error "there are aliased coefficients in the model" (aliases are linearly dependent terms)

  print(res)


  ###IMPORTANT check that anova assumptions are met

  # 1. Homogeneity of variances
  print("-------------Homogeneity of variances")
  plot(anova, 1)

  print(car::leveneTest(density ~ mpa_status * l4_attrib, data = df))
  print("if pval > 0.05 homogeneity of variance assumed")
  # if the p-value is not less than the significance level of 0.05 there is no evidence to suggest
  # that the variance across groups is statistically significantly different.
  # and we can assume the homogeneity of variances in the different treatment groups.

  # 2. Normality
  print("-------------Normality of residuals")
  plot(anova, 2)

  # Extract the residuals
  aov_residuals <- residuals(object = anova)
  # Run Shapiro-Wilk test
  print(shapiro.test(x = aov_residuals))
  print("if pval > 0.05 no violation normality")
  # if pvalue > 0.05 there is no indication that normality is violated.
  #->violated


  # make dataframe for plotting
  new = data.frame(var = c("mpa", "coral", "mpa * coral"),
                   fscore =  res$`F value`[1:3],
                   pval = res$`Pr(>F)`[1:3])

  #add significance symbol
  new$signif = ifelse(new$pval < 0.05, "*", "ns")

  #max fscore for plotting
  maxfscore = max(new$fscore)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("mpa", "coral", "mpa * coral")


  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = var , y = fscore)) +
    ggplot2::geom_col() +
    ggplot2::ylab("F-score") +
    ggplot2::xlab("") +
    ggplot2::annotate("text", x = 1, y = maxfscore + 0.1, label = signif[1], size = 6) +
    ggplot2::annotate("text", x = 2, y = maxfscore + 0.1, label = signif[2], size = 6) +
    ggplot2::annotate("text", x = 3, y = maxfscore + 0.1, label = signif[3], size = 6) +
    ggplot2::labs(title=paste("2-way ANOVA for", species)) +
    ggplot2::scale_x_discrete(limits = positions) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=14),
            axis.title = ggplot2::element_text(size=14,face="bold"),
            plot.title = ggplot2::element_text(hjust = 0.5))

    ggplot2::ggsave(here::here(paste0("outputs/anova_barplot_coral_", coral_attrib, "_mpa_", species, ".png")), p, width = 7, height = 5)


}



#' Make two-way non parametric test (scheirerRayHare test) for density per mpa status and coral habitat and barplot of the result
#'
#' @param df
#' @param species
#' @param coral_attrib
#'
#' @return
#' @export
#'

make_twoway_test_barplot_coral_mpa <- function(df, species, coral_attrib){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste(coral_attrib))) -> df

  # remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  df %>%
    dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df

  # https://rcompanion.org/handbook/F_14.html
  # The Scheirer Ray Hare test is a nonparametric test used for a two-way factorial experiment.

  res = rcompanion::scheirerRayHare(density ~ mpa_status + coral_hab, data = df)

  print(res)

  # make dataframe for plotting
  new = data.frame(var = c("mpa", "coral", "mpa * coral"),
                   effect =  res$H[1:3],
                   pval = res$p.value[1:3])

  #add significance symbol
  new$signif = ifelse(new$pval < 0.05, "*", "ns")

  #max effect for plotting
  maxeffect = max(new$effect)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("mpa", "coral", "mpa * coral")


  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = var , y = effect)) +
    ggplot2::geom_col() +
    ggplot2::ylab("effect") +
    ggplot2::xlab("") +
    ggplot2::annotate("text", x = 1, y = maxeffect + 0.2, label = signif[1], size = 6) +
    ggplot2::annotate("text", x = 2, y = maxeffect + 0.2, label = signif[2], size = 6) +
    ggplot2::annotate("text", x = 3, y = maxeffect + 0.2, label = signif[3], size = 6) +
    ggplot2::labs(title=paste("2-way test for", species)) +
    ggplot2::scale_x_discrete(limits = positions) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=14),
                   axis.title = ggplot2::element_text(size=14,face="bold"),
                   plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::theme_bw()

  ggplot2::ggsave(here::here(paste0("outputs/twoway_test_barplot_coral_", coral_attrib, "_mpa_", species, ".png")), p, width = 7, height = 5)

}


#' Make two-way non parametric test (scheirerRayHare test) for density per mpa status and Allen coral habitat and barplot of the result
#'
#' @param df
#' @param species
#' @param coral_attrib
#'
#' @return
#' @export
#'

make_twoway_test_barplot_allen_coral_mpa <- function(df, species){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(class2 = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df

  # remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  # df %>%
  #   dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df

  # https://rcompanion.org/handbook/F_14.html
  #The Scheirer Ray Hare test is a nonparametric test used for a two-way factorial experiment.
  res = rcompanion::scheirerRayHare(density ~ mpa_status + class2, data = df)

  print(res)

  #make dataframe for plotting
  new = data.frame(var = c("Mpa", "Habitat", "Mpa * Habitat"),
                   effect =  res$H[1:3],
                   pval = res$p.value[1:3])

  #add significance symbol
  new$signif = ifelse(new$pval < 0.05, "*", "ns")

  #max effect for plotting
  maxeffect = max(new$effect)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Mpa", "Habitat", "Mpa * Habitat")


  plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = var , y = effect)) +
    # ggplot2::geom_col(fill = c("yellow", "darkgreen", "chartreuse4"))  +
    ggplot2::geom_col(fill = c("yellow", "darkgreen", "chartreuse4"), color = "black") +
    ggplot2::ylab("Effect") +
    ggplot2::xlab("") +
    ggplot2::annotate("text", x = 1, y = maxeffect + 0.5, label = signif[1], size = 6) +
    ggplot2::annotate("text", x = 2, y = maxeffect + 0.5, label = signif[2], size = 6) +
    ggplot2::annotate("text", x = 3, y = maxeffect + 0.5, label = signif[3], size = 6) +
    ggplot2::labs(title=paste("2-way test")) +
    ggplot2::scale_x_discrete(limits = positions) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 13),
                   axis.text.x = ggplot2::element_text(size = 13, color = "black"),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"),
                   plot.title = ggplot2::element_text(hjust = 0.5))

  ggplot2::ggsave(here::here(paste0("outputs/twoway_test_barplot_allen_coral_mpa_", species, ".png")), p, width = 7, height = 5)

}



#' Make permanova for density per mpa status and coral habitat
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

make_permanova_allen_coral_mpa <- function(df, species){

  df %>%
    dplyr::mutate(allen_coral_hab = as.factor(get(paste("class")))) %>%
    # delete na
    dplyr::filter(!is.na(density_Dugong_certain), !is.na(density_Turtle), !is.na(density_Shark), !is.na(density_Round_ray), !is.na(density_Eagle_ray)) %>%
    as.data.frame() -> df

  #density taxa
  if (species == "Dugong_certain") {
    density_taxa = df$density_Dugong_certain
  }
  if (species == "Turtle") {
    density_taxa = df$density_Turtle
  }
  if (species == "Shark") {
    density_taxa = df$density_Shark
  }
  if (species == "Round_ray") {
    density_taxa = df$density_Round_ray
  }
  if (species == "Eagle_ray") {
    density_taxa = df$density_Eagle_ray
  }

  df %>%
    dplyr::select(id, allen_coral_hab, mpa_status) -> df_env

  #permanova
  permanova <- vegan::adonis(density_taxa ~ allen_coral_hab * mpa_status, data = df_env, permutations = 999, method = "euclidean")

  print(permanova)

  return(permanova)

}






#' Barplot with all species : Permanova habitat + mpa
#'
#' @param res1
#' @param res2
#' @param res3
#' @param res4
#' @param res5
#' @param df
#'
#' @return
#' @export
#'

make_permanova_barplot_allen_coral_mpa_with_all_species <- function(df, res1, res2, res3, res4, res5){

  # make new dataframe, table of results
  new = data.frame(Species = c("Dugong_certain", "Dugong_certain", "Dugong_certain", "Turtle", "Turtle", "Turtle", "Shark", "Shark", "Shark", "Round_ray", "Round_ray", "Round_ray", "Eagle_ray", "Eagle_ray", "Eagle_ray"),
                   var = rep(c("Habitat","Reserve","Habitat * Reserve"), 5),
                   effect =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                   pval = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                   signif = rep(NA, length(15)))

  #add significance symbol
  for (i in 1:length(new$pval)){
    if(new$pval[i] > 0.05) {new$signif[i] <- "ns"}
    if(new$pval[i] <= 0.05 & new$pval[i] > 0.01) {new$signif[i] <- "*"}
    if(new$pval[i] <= 0.01 & new$pval[i] > 0.001) {new$signif[i] <- "**"}
    if(new$pval[i] <= 0.001) {new$signif[i] <- "***"}
  }


  #max effect for plotting
  maxeffect = max(new$effect)

  #effect variable
  effect = new$effect

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Habitat","Reserve","Habitat * Reserve")

  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))

  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Habitat","Reserve","Habitat * Reserve"))

  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyatidae", "Myliobatidae")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col()+
    ggplot2::scale_fill_manual(values = c("black","yellow", "darkgrey"), labels = c("Habitat","MPA", "Habitat * MPA")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    ggplot2::ylab("F-score") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 17),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y =  ggplot2::element_text(size = 15),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 16),
                   legend.key.size = ggplot2::unit(1.5,"line"),
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 0.6, size = 6)

  # make dataframe export
  new2 = data.frame(Species = c("Dugong_certain", "Dugong_certain", "Dugong_certain", "Turtle", "Turtle", "Turtle", "Shark", "Shark", "Shark", "Round_ray", "Round_ray", "Round_ray", "Eagle_ray", "Eagle_ray", "Eagle_ray"),
                    Variables = c("Habitat","Reserve","Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve", "Habitat", "Reserve", "Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve"),
                    Df =  c(res1$aov.tab$Df[1:3], res2$aov.tab$Df[1:3], res3$aov.tab$Df[1:3], res4$aov.tab$Df[1:3], res5$aov.tab$Df[1:3]),
                    SumsOfSqs = c(res1$aov.tab$SumsOfSqs[1:3], res2$aov.tab$SumsOfSqs[1:3], res3$aov.tab$SumsOfSqs[1:3], res4$aov.tab$SumsOfSqs[1:3], res5$aov.tab$SumsOfSqs[1:3]),
                    F_score =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                    "p.value" = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                    Signifiance = signif)

  # convert new2 to csv
  write.csv2(new2, here::here("outputs", "permanova_results.csv"), row.names = FALSE )


  ggplot2::ggsave(here::here("outputs", "permanova_barplot_allen_coral_mpa_with_all_species_habitat_mpa.png"), p, width = 7, height = 5)

}





#' Make pairwise comparisons for density per coral habitat
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

make_pairwise_comparison_allen_coral <- function(df, species){

  df %>%
    dplyr::mutate(allen_coral_hab = as.factor(get(paste("class")))) %>%
    # delete na
    dplyr::filter(!is.na(density_Dugong_certain), !is.na(density_Turtle), !is.na(density_Shark), !is.na(density_Round_ray), !is.na(density_Eagle_ray)) %>%
    as.data.frame() -> df

  #density taxa
  if (species == "Dugong_certain") {
    density_taxa = df$density_Dugong_certain
  }
  if (species == "Turtle") {
    density_taxa = df$density_Turtle
  }
  if (species == "Shark") {
    density_taxa = df$density_Shark
  }
  if (species == "Round_ray") {
    density_taxa = df$density_Round_ray
  }
  if (species == "Eagle_ray") {
    density_taxa = df$density_Eagle_ray
  }

  df %>%
    dplyr::select(id, allen_coral_hab) -> df_env

  #need to load library for the function to work (installed from devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis"))
  library(pairwiseAdonis)

  #pairwise comp (need to replicate density_taxa column for the function to work)
  comp <- pairwise.adonis2(cbind(density_taxa,density_taxa) ~ allen_coral_hab, data = df_env, permutations = 999, method = "euclidean")

  print(comp)

  return(comp)

}





#' Make pairwise comparisons for density per coral habitat and mpa
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

make_pairwise_comparison_allen_coral_mpa <- function(df, species){

  df %>%
    dplyr::mutate(allen_coral_hab = as.factor(get(paste("class")))) %>%
    # delete na
    dplyr::filter(!is.na(density_Dugong_certain), !is.na(density_Turtle), !is.na(density_Shark), !is.na(density_Round_ray), !is.na(density_Eagle_ray)) %>%
    as.data.frame() -> df

  #density taxa
  if (species == "Dugong_certain") {
    density_taxa = df$density_Dugong_certain
  }
  if (species == "Turtle") {
    density_taxa = df$density_Turtle
  }
  if (species == "Shark") {
    density_taxa = df$density_Shark
  }
  if (species == "Round_ray") {
    density_taxa = df$density_Round_ray
  }
  if (species == "Eagle_ray") {
    density_taxa = df$density_Eagle_ray
  }

  df %>%
    dplyr::select(id, allen_coral_hab, mpa_status) -> df_env

  #need to load library for the function to work (installed from devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis"))
  library(pairwiseAdonis)

  #pairwise comp (need to replicate density_taxa column for the function to work)
  comp <- pairwise.adonis2(cbind(density_taxa,density_taxa) ~ allen_coral_hab * mpa_status, data = df_env, permutations = 999, method = "euclidean")

  print(comp)

  return(comp)

}
