
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
    dplyr::mutate(mpa_status = forcats::fct_recode(NAME, mpa = "PoÃ©")) %>%
    #add new column base on mpa status and coral habitat
    dplyr::mutate(mpa_status_l4_attrib = paste0(mpa_status, "_", l4_attrib)) -> res

  return(res)

}


#' Barplot of mean density per coral habitat
#'
#' @param df
#' @param coral_attrib
#' @param species
#'
#' @return
#' @export
#'

barplot_density_coral <- function(df, coral_attrib, species){

  #mean density per coral hab for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste(coral_attrib))) %>%
    dplyr::select(c(density, coral_hab)) %>%
    dplyr::group_by(coral_hab) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  print(new)

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = new$coral_hab)

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = reorder(coral_hab, mean_dens), y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(coral_hab, mean_dens), ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per coral habitat for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_coral_", coral_attrib, "_", species, ".png")), p, width = 7, height = 5)

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

  print(new)

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = new$coral_hab)

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = reorder(coral_hab, mean_dens), y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(coral_hab, mean_dens), ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per coral habitat for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_allen_coral_", species, ".png")), p, width = 7, height = 5)

}


#' Barplot of mean density per mpa status (in/out mpa)
#'
#' @param df
#' @param coral_attrib
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

  print(new)

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   mpa_status = new$mpa_status)

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = reorder(mpa_status, mean_dens), y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(mpa_status, mean_dens), ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per mpa status for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_mpa_", species, ".png")), p, width = 7, height = 5)

}





#' Barplot of mean density per coral habitat and mpa status
#'
#' @param df
#' @param coral_attrib
#' @param species
#'
#' @return
#' @export
#'

barplot_density_coral_mpa <- function(df, coral_attrib, species){

  #mean density per mpa status and coral hab for given species
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(coral_hab = get(paste0("mpa_status_", coral_attrib))) %>%
    dplyr::select(c(density, coral_hab)) %>%
    dplyr::group_by(coral_hab) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  print(new)

####****FINISH
  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = new$coral_hab)

  #reorder levels
  sizes <- factor(c("small", "large", "large", "small", "medium"),
                  levels = c("small", "medium", "large"))

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = coral_hab, y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = coral_hab, ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per coral habitat for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_coral_", coral_attrib, "_mpa_", species, ".png")), p, width = 7, height = 5)

}



#' Barplot of mean density per mpa status (in/out mpa) per date
#'
#' @param df
#' @param coral_attrib
#' @param species
#'
#' @return
#' @export
#'

barplot_density_mpa_per_date <- function(df, species){

  #pivot longer
  df %>%
    dplyr::select(c(id, mpa_status, tidyselect::starts_with(paste0("density_", species)))) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("density"),
                        names_to = "date",
                        values_to = "density") %>%
    dplyr::mutate(date = stringr::str_replace_all(date, paste0("density_", species, "_"), "")) %>%
    dplyr::mutate(date = stringr::str_replace_all(date, paste0("density_", species), "all_days")) -> df2

  #mean density and se for given species
  df2 %>%
    dplyr::group_by(mpa_status, date) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   date = new$date,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   mpa_status = new$mpa_status)

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = reorder(mpa_status, mean_dens), y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(mpa_status, mean_dens), ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::facet_wrap(~date) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per mpa status for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_mpa_per_date_", species, ".png")), p, width = 7, height = 5)

}



#' Barplot of mean density in coral habitat per date
#'
#' @param df
#' @param coral_attrib
#' @param species
#'
#' @return
#' @export
#'

barplot_density_coral_per_date <- function(df, coral_attrib, species){

  #pivot longer
  df %>%
    dplyr::mutate(coral_hab = get(paste(coral_attrib))) %>%
    dplyr::select(c(id, coral_hab, tidyselect::starts_with(paste0("density_", species)))) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("density"),
                        names_to = "date",
                        values_to = "density") %>%
    dplyr::mutate(date = stringr::str_replace_all(date, paste0("density_", species, "_"), "")) %>%
    dplyr::mutate(date = stringr::str_replace_all(date, paste0("density_", species), "all_days")) -> df2

  #mean density and se for given species
  df2 %>%
    dplyr::group_by(coral_hab, date) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   date = new$date,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = new$coral_hab)

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = reorder(coral_hab, mean_dens), y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(coral_hab, mean_dens), ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::facet_wrap(~date) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per coral_hab for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_coral_hab_per_date_", species, ".png")), p, width = 7, height = 5)

}


#' Barplot of mean density in coral habitat per date with data Allen
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

barplot_density_allen_coral_per_date <- function(df, species){

  #pivot longer
  df %>%
    dplyr::mutate(coral_hab = get(paste("class"))) %>%
    dplyr::select(c(id, coral_hab, tidyselect::starts_with(paste0("density_", species)))) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("density"),
                        names_to = "date",
                        values_to = "density") %>%
    dplyr::mutate(date = stringr::str_replace_all(date, paste0("density_", species, "_"), "")) %>%
    dplyr::mutate(date = stringr::str_replace_all(date, paste0("density_", species), "all_days")) -> df2

  #mean density and se for given species
  df2 %>%
    dplyr::group_by(coral_hab, date) %>%
    dplyr::summarize(mean = mean(density, na.rm=T),
                     sd = sd(density, na.rm=T),
                     se = sd / sqrt(length(density))) -> new

  # convert back to spatial object for plotting
  new = sf::as_Spatial(new)

  # make dataframe for plotting
  new = data.frame(mean_dens = new$mean,
                   date = new$date,
                   sd_dens = new$sd,
                   se_dens = new$se,
                   coral_hab = new$coral_hab)

  # plot
  p = ggplot2::ggplot(new, ggplot2::aes(x = reorder(coral_hab, mean_dens), y = mean_dens)) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(x = reorder(coral_hab, mean_dens), ymin = mean_dens-se_dens, ymax = mean_dens+se_dens), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    ggplot2::facet_wrap(~date) +
    ggplot2::coord_flip() +
    ggplot2::labs(title=paste("Density per coral_hab for", species)) +
    ggplot2::xlab("") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust=1))

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/barplot_density_allen_coral_hab_per_date_", species, ".png")), p, width = 7, height = 5)

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

  cat("--------------------------")
  cat(species)

  wilcox.test(density ~ mpa_status, data = df, exact = FALSE)

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

  cat("--------------------------")
  cat(species)

  wilcox.test(density ~ mpa_status, data = df,  exact = FALSE, alternative = "greater")

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

  cat("--------------------------")
  cat(species, "and", "class")

  kruskal.test(density ~ coral_class, data = df)

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

  pairwise.wilcox.test(df$density, df$coral_class, p.adjust.method = "BH")

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


#' Test hyp that median densities between habitat types combined with mpa statusare different with data Allen
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

compare_densities_between_combined_allen_habitats_mpa_status <- function(df, species){

  ##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(mpa_status_allen_coral_hab = forcats::as_factor(get(paste0("mpa_status_", "class"))))  %>%
    tidyr::drop_na(density)  -> df

  cat("--------------------------")
  cat(species, "and", "class", "combined with mpa status")

  print(kruskal.test(density ~ mpa_status_allen_coral_hab, data = df))

  print("if p-value of the test is less than the significance level alpha = 0.05
  #We can conclude that desnities are significantly different between
  #combinations of habitat types and mpa status with a p-value of xxx")

  cat("--------------pairwise")
  pairwise.wilcox.test(df$density, df$mpa_status_allen_coral_hab, p.adjust.method = "BH")

  print("if p-value of a pairwise comparison is less than the significance level alpha = 0.05
  We can conclude that these levels are significantly different with a p-value of xxx")

}


#' Boxplot of mean density per mpa status (in/out mpa) (0s removed)
#'
#' @param df
#' @param species

boxplot_density_mpa <- function(df, species, img){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    #exclude 0s
    dplyr::filter(density>0) -> df_no0

  p = ggpubr::ggboxplot(df_no0, x = "mpa_status", y = paste0("density_", species))
  p = ggpubr::ggpar(p, rotate = TRUE)

  #add megafauna image
  patchwork::inset_element(p = img, left = 0.5, bottom = 0.55, right = 1, top = 1)

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_mpa_", species, ".png")), p, width = 7, height = 5)

}





#' Boxplot of mean density per coral habitat (0s removed)
#'
#' @param df
#' @param species

boxplot_density_coral <- function(df, species, coral_attrib){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    #exclude 0s
    dplyr::filter(density>0) -> df_no0

  p = ggpubr::ggboxplot(df_no0, x = paste0(coral_attrib), y = paste0("density_", species))
  p = ggpubr::ggpar(p, rotate = TRUE)

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_coral_", coral_attrib, "_", species, ".png")), p, width = 7, height = 5)

}


#' Boxplot of mean density per coral habitat (0s removed) with data Allen
#'
#' @param df
#' @param species

boxplot_density_allen_coral <- function(df, species){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    #exclude 0s
    dplyr::filter(density>0) -> df_no0

  p = ggpubr::ggboxplot(df_no0, x = paste0("class"), y = paste0("density_", species))
  p = ggpubr::ggpar(p, rotate = TRUE)

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_allen_coral_", species, ".png")), p, width = 7, height = 5)

}


#' Boxplot of mean density per mpa status and coral habitat (0s removed)
#'
#' @param df
#' @param species

boxplot_density_coral_mpa <- function(df, species, coral_attrib){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    dplyr::mutate(mpa_status_coral_hab = forcats::as_factor(get(paste0("mpa_status_", coral_attrib)))) %>%
    #exclude 0s
    dplyr::filter(density>0) -> df_no0

  p = ggpubr::ggboxplot(df_no0, x = coral_attrib, y = paste0("density_", species), fill = "mpa_status")

  p = ggpubr::ggpar(p, orientation = "horizontal")

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_coral_", coral_attrib, "_mpa_", species, ".png")), p, width = 7, height = 5)

}


#' Boxplot of mean density per mpa status and coral habitat (0s removed) with data Allen
#'
#' @param df
#' @param species

boxplot_density_allen_coral_mpa <- function(df, species){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    # dplyr::mutate(mpa_status_allen_coral_hab = forcats::as_factor(get(paste0("mpa_status_", "class")))) %>%
    #exclude 0s
    dplyr::filter(density>0) -> df_no0

  p = ggpubr::ggboxplot(df_no0, x = "class", y = paste0("density_", species), fill = "mpa_status", palette = c("yellow", "steelblue"))

  p = ggpubr::ggpar(p, orientation = "horizontal")

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_allen_coral_", "mpa_", species, ".png")), p, width = 7, height = 5)

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
  p = p + ggplot2::scale_fill_manual(labels = c("MPA", "No MPA"), values = c("yellow", "skyblue3"))



  #rename class Allen
  p = p + ggplot2::scale_x_discrete(breaks=c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"),
                           labels=c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal mats", "          Open sea", "Coral/Algae"))

  # #graduations
  # p = p + ggplot2::scale_y_continuous(labels = scaleFUN)

ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_allen_coral_", "mpa_", species, "_with_megafauna_image_LOG", ".png")), p, width = 7, height = 5)


}


#' Boxplot of mean density per mpa status and coral habitat (0s removed) with data Allen and megafauna image no LOG
#'
#' @param df
#' @param img
#' @param species

boxplot_density_allen_coral_mpa_with_megafauna_image <- function(df, species, img){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species)))  %>%
    dplyr::mutate(density2 = density1 * 100) %>%
    # dplyr::mutate(mpa_status_allen_coral_hab = forcats::as_factor(get(paste0("mpa_status_", "class")))) %>%
    #exclude 0s
    dplyr::filter(density2>0) %>%
    #ordonate class
    dplyr::mutate(class2 = factor(class, levels = c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"))) -> df_no0

  #change decimal number
  scaleFUN <- function(density2) sprintf("%.3f", density2)

  tbl <- table(df_no0$mpa_status, df_no0$class2)

  #conditions
  if (species == "Dugong_certain") {
    a = 3.9
    b = 6.7
    c = log(0.10)
    d = log(4.4)
    species_title = "Dugong"
    X1 = 1.2
    X2 = NULL
    X3 = NULL
    X4 = 2
    X5 = 3
    X6 = 4
    X7 = 5.2
    X8 = 0.8
    X9 = NULL
    X10 = NULL
    X11 = NULL
    X12 = NULL
    X13 = NULL
    X14 = 4.8
    Y1 = 0.0062
    Y2 = NULL
    Y3 = NULL
    Y4 = 0.017
    Y5 = 0.013
    Y6 = 0.0062
    Y7 = 0.0105
    Y8 = 0.0027
    Y9 = NULL
    Y10 = NULL
    Y11 = NULL
    Y12 = NULL
    Y13 = NULL
    Y14 = 2
  }
  if (species == "Turtle") {
    a = 5.
    b = 8.8
    c = log(0.0400)
    d = log(0.7000)
    species_title = "Sea Turtle"
    X1 = 1.2
    X2 = 2.2
    X3 = 3
    X4 = 4.2
    X5 = 5.2
    X6 = 6.2
    X7 = 7.2
    X8 = 0.8
    X9 = 1.8
    X10 = NULL
    X11 =3.8
    X12 = 4.8
    X13 =5.8
    X14 = 6.8
    Y1 = 0.063
    Y2 = 0.21
    Y3 = 0.0156
    Y4 = 0.088
    Y5 = 0.038
    Y6 = 0.08
    Y7 = 0.042
    Y8 = 0.063
    Y9 = 0.35
    Y10 = NULL
    Y11 = 0.15
    Y12 = 0.075
    Y13 = 0.21
    Y14 = 0.1
  }
  if (species == "Shark") {
    a = 5.9
    b = 8.4
    c = log(0.0035)
    d = log(0.32950)
    species_title = "Shark"
    X1 = 1.2
    X2 = 2.2
    X3 = 3
    X4 = 4.2
    X5 = 5.2
    X6 = NULL
    X7 = 7.2
    X8 = 0.8
    X9 = 1.8
    X10 = NULL
    X11 = 3.8
    X12 = 4.8
    X13 = 6
    X14 = 6.8
    Y1 = 0.039
    Y2 = 0.007
    Y3 = 0.0099
    Y4 = 0.014
    Y5 = 0.007
    Y6 = NULL
    Y7 = 0.006
    Y8 = 0.048
    Y9 = 0.048
    Y10 = NULL
    Y11 = 0.03
    Y12 = 0.076
    Y13 = 0.046
    Y14 = 0.006
  }
  if (species == "Round_ray") {
    a = 6
    b = 8
    c = log(0.0130)
    d = log(0.2100)
    species_title = "Myliobatidae ray"
    X1 = NULL
    X2 = 2.2
    X3 = 3
    X4 = 4.2
    X5 = 5.2
    X6 = NULL
    X7 = 7
    X8 = 1
    X9 = 1.8
    X10 = NULL
    X11 = 3.8
    X12 = 4.8
    X13 = 6
    X14 = NULL
    Y1 = NULL
    Y2 = 0.11
    Y3 = 0.0151
    Y4 = 0.087
    Y5 = 0.09
    Y6 = NULL
    Y7 = 0.0109
    Y8 = 0.042
    Y9 = 0.203
    Y10 = NULL
    Y11 = 0.0154
    Y12 = 0.053
    Y13 = 0.035
    Y14 = NULL
  }
  if (species == "Eagle_ray") {
    a = 5
    b = 6.8
    c = log(0.0325)
    d = log(0.3025)
    species_title = "Dasyatidae ray"
    X1 = 1.2
    X2 = 2.2
    X3 = 3.2
    X4 = 4.2
    X5 = 5.2
    X6 = NULL
    X7 = 6.2
    X8 = 0.8
    X9 = 1.8
    X10 = 2.8
    X11 = 3.8
    X12 = 4.8
    X13 = NULL
    X14 = 5.8
    Y1 = 0.03
    Y2 = 0.055
    Y3 = 0.022
    Y4 = 0.0125
    Y5 = 0.022
    Y6 = NULL
    Y7 = 0.050
    Y8 = 0.31
    Y9 = 0.057
    Y10 = 0.018
    Y11 = 0.087
    Y12 = 0.067
    Y13 = NULL
    Y14 = 0.072
  }

  # p = ggplot2::ggplot(data = df_no0, ggplot2::aes(x = density, y = class)) + ggplot2::geom_boxplot(ggplot2::aes(col=class)) +
  #   ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = 4, xmax = 6, ymin = 0.9, ymax = 1.4)

  p = ggpubr::ggboxplot(df_no0, x = "class2", y = "density2", xlab = "", ylab = "Density (indiv/100ha)" ,fill = "mpa_status", palette = c("yellow", "steelblue"))
  #to convert log
  # p = ggpubr::ggpar(p, yscale = "log2")

  p = ggpubr::ggpar(p, orientation = "horizontal")
  #legend
  p = ggpubr::ggpar(p,legend = "right", legend.title = "Mpa status", font.legend = c(10, "plain", "black"))
  #Title
  p = ggpubr::ggpar(p,
                    title = species_title,
                    font.main = c(14,"bold", "black"),
                    font.x = c(14, "plain", "black"),
                    font.y = c(14, "plain", "black"))

  # p = p + ggplot2::annotate("text", x = X1, y = Y1, label = paste("n =", tbl[2,1]), size = 3.5) +
  #   ggplot2::annotate("text", x = X2, y = Y2, label = paste("n =",tbl[2,2]), size = 3.5) +
  #   ggplot2::annotate("text", x = X3, y = Y3, label = paste("n =",tbl[2,3]), size = 3.5) +
  #   ggplot2::annotate("text", x = X4, y = Y4, label = paste("n =",tbl[2,4]), size = 3.5) +
  #   ggplot2::annotate("text", x = X5, y = Y5 , label = paste("n =",tbl[2,5]), size = 3.5) +
  #   ggplot2::annotate("text", x = X6, y = Y6, label = paste("n =",tbl[2,6]), size = 3.5) +
  #   ggplot2::annotate("text", x = X7, y = Y7 , label = paste("n =",tbl[2,7]), size = 3.5) +
  #   ggplot2::annotate("text", x = X8, y = Y8, label = paste("n =",tbl[1,1]), size = 3.5) +
  #   ggplot2::annotate("text", x = X9, y = Y9, label = paste("n =",tbl[1,2]), size = 3.5) +
  #   ggplot2::annotate("text", x = X10, y = Y10 , label = paste("n =",tbl[1,3]), size = 3.5) +
  #   ggplot2::annotate("text", x = X11, y = Y11, label = paste("n =",tbl[1,4]), size = 3.5) +
  #   ggplot2::annotate("text", x = X12, y = Y12 , label = paste("n =",tbl[1,5]), size = 3.5) +
  #   ggplot2::annotate("text", x = X13, y = Y13, label = paste("n =",tbl[1,6]), size = 3.5) +
  #   ggplot2::annotate("text", x = X14, y = Y14 , label = paste("n =",tbl[1,7]), size = 3.5)


  #add megafaune image
  p = p + ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)
  #it's possible to add ggplot2 function to ggpubr
  p = p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  # #graduations
  # p = p + ggplot2::scale_y_continuous(labels = scaleFUN)


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_allen_coral_", "mpa_", species, "_with_megafauna_image", ".png")), p, width = 7, height = 5)


}


#' Boxplot of mean density per mpa status and coral habitat (0s removed) with data Allen and megafauna image with zero + LOG
#'
#' @param df
#' @param img
#' @param species

boxplot_density_allen_coral_mpa_with_megafauna_image_zero_log <- function(df, species, img){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species))) %>%
    dplyr::mutate(density2 = log((density1 + 0.000001) * 100)) %>%
    # dplyr::mutate(mpa_status_allen_coral_hab = forcats::as_factor(get(paste0("mpa_status_", "class")))) %>%

    #ordonate class
    dplyr::mutate(class2 = factor(class, levels = c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"))) -> df_2

  tbl <- table(df_2$mpa_status, df_2$class2)

  #conditions
  if (species == "Dugong_certain") {
    a = 3.9
    b = 6.7
    c = log(0.10)
    d = log(4.4)
    species_title = "Dugong"
  }
  if (species == "Turtle") {
    a = 5.6
    b = 8.2
    c = log(0.0150)
    d = log(0.3700)
    species_title = "Sea Turtle"
  }
  if (species == "Shark") {
    a = 5.9
    b = 8.4
    c = log(0.0035)
    d = log(0.32950)
    species_title = "Shark"
  }
  if (species == "Round_ray") {
    a = 6
    b = 8
    c = log(0.0030)
    d = log(0.2000)
    species_title = "Myliobatidae ray"
  }
  if (species == "Eagle_ray") {
    a = 5
    b = 6.8
    c = log(0.0125)
    d = log(0.2525)
    species_title = "Dasyatidae ray"
  }

  # p = ggplot2::ggplot(data = df_no0, ggplot2::aes(x = density, y = class)) + ggplot2::geom_boxplot(ggplot2::aes(col=class)) +
  #   ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = 4, xmax = 6, ymin = 0.9, ymax = 1.4)

  p = ggpubr::ggboxplot(df_2, x = "class2", y = "density2", xlab = "", ylab = "Log Density (indiv / 100ha) with zero" ,fill = "mpa_status", palette = c("yellow", "steelblue"))

  p = ggpubr::ggpar(p, orientation = "horizontal")
  #legend
  p = ggpubr::ggpar(p,legend = "right", legend.title = "Mpa status", font.legend = c(10, "plain", "black"))
  #Title
  p = ggpubr::ggpar(p,
                    title = species_title,
                    font.main = c(14,"bold", "black"),
                    font.x = c(14, "plain", "black"),
                    font.y = c(14, "plain", "black"))

  #to convert log
  # p = ggpubr::ggpar(p, yscale = "log2")

  #add megafaune image
  p = p + ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)
  #it's possible to add ggplot2 function to ggpubr
  p = p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # p = p + ggplot2::annotate("text", x = X1, y = Y, label = paste("n =", tbl[2,7]), size = 4)
  #   ggplot2::annotate("text", x = X2, y = Y, label = tbl[2,6], size = 4) +
  #   ggplot2::annotate("text", x = X3, y = Y , label = tbl[2,5], size = 4) +
  #   ggplot2::annotate("text", x = X4, y = Y, label = tbl[2,4], size = 4) +
  #   ggplot2::annotate("text", x = X5, y = Y , label = tbl[2,3], size = 4) +
  #   ggplot2::annotate("text", x = X6, y = Y, label = tbl[2,2], size = 4) +
  #   ggplot2::annotate("text", x = X7, y = Y , label = tbl[2,1], size = 4) +
  #   ggplot2::annotate("text", x = X8, y = Y, label = tbl[1,6], size = 4) +
  #   ggplot2::annotate("text", x = X9, y = Y , label = tbl[1,5], size = 4) +
  #   ggplot2::annotate("text", x = X10, y = Y, label = tbl[1,4], size = 4) +
  #   ggplot2::annotate("text", x = X11, y = Y , label = tbl[1,3], size = 4) +
  #   ggplot2::annotate("text", x = X12, y = Y, label = tbl[1,2], size = 4) +
  #   ggplot2::annotate("text", x = X13, y = Y , label = tbl[1,1], size = 4)

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_allen_coral_", "mpa_", species, "_with_megafauna_image_zero_log", ".png")), p, width = 7, height = 5)


}


#' Boxplot of mean density per mpa status and coral habitat (0s removed) with data Allen and megafauna image with zero
#'
#' @param df
#' @param img
#' @param species

boxplot_density_allen_coral_mpa_with_megafauna_image_zero <- function(df, species, img){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species))) %>%
    dplyr::mutate(density2 = density1 * 100) %>%
    # dplyr::mutate(mpa_status_allen_coral_hab = forcats::as_factor(get(paste0("mpa_status_", "class")))) %>%

    #ordonate class
    dplyr::mutate(class2 = factor(class, levels = c("Seagrass", "Sand", "Rubble", "Rock", "Microalgal Mats", "Deep_sea", "Coral/Algae"))) -> df_2

  tbl <- table(df_2$mpa_status, df_2$class2)

  #conditions
  if (species == "Dugong_certain") {
    a = 3.9
    b = 6.7
    c = log(0.10)
    d = log(4.4)
    species_title = "Dugong"
  }
  if (species == "Turtle") {
    a = 5.6
    b = 8.2
    c = log(0.0150)
    d = log(0.3700)
    species_title = "Sea Turtle"
  }
  if (species == "Shark") {
    a = 5.9
    b = 8.4
    c = log(0.0035)
    d = log(0.32950)
    species_title = "Shark"
  }
  if (species == "Round_ray") {
    a = 6
    b = 8
    c = log(0.0030)
    d = log(0.2000)
    species_title = "Myliobatidae ray"
  }
  if (species == "Eagle_ray") {
    a = 5
    b = 6.8
    c = log(0.0125)
    d = log(0.2525)
    species_title = "Dasyatidae ray"
  }

  # p = ggplot2::ggplot(data = df_no0, ggplot2::aes(x = density, y = class)) + ggplot2::geom_boxplot(ggplot2::aes(col=class)) +
  #   ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = 4, xmax = 6, ymin = 0.9, ymax = 1.4)

  p = ggpubr::ggboxplot(df_2, x = "class2", y = "density2", xlab = "", ylab = "Density (indiv / 100ha) with zero" ,fill = "mpa_status", palette = c("yellow", "steelblue"))

  p = ggpubr::ggpar(p, orientation = "horizontal")
  #legend
  p = ggpubr::ggpar(p,legend = "right", legend.title = "Mpa status", font.legend = c(10, "plain", "black"))
  #Title
  p = ggpubr::ggpar(p,
                    title = species_title,
                    font.main = c(14,"bold", "black"),
                    font.x = c(14, "plain", "black"),
                    font.y = c(14, "plain", "black"))

  #add megafaune image
  p = p + ggplot2::annotation_custom(grid::rasterGrob(img, interpolate=TRUE), xmin = a, xmax = b, ymin = c, ymax = d)
  #it's possible to add ggplot2 function to ggpubr
  p = p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # p = p + ggplot2::annotate("text", x = X1, y = Y, label = paste("n =", tbl[2,7]), size = 4)
  #   ggplot2::annotate("text", x = X2, y = Y, label = tbl[2,6], size = 4) +
  #   ggplot2::annotate("text", x = X3, y = Y , label = tbl[2,5], size = 4) +
  #   ggplot2::annotate("text", x = X4, y = Y, label = tbl[2,4], size = 4) +
  #   ggplot2::annotate("text", x = X5, y = Y , label = tbl[2,3], size = 4) +
  #   ggplot2::annotate("text", x = X6, y = Y, label = tbl[2,2], size = 4) +
  #   ggplot2::annotate("text", x = X7, y = Y , label = tbl[2,1], size = 4) +
  #   ggplot2::annotate("text", x = X8, y = Y, label = tbl[1,6], size = 4) +
  #   ggplot2::annotate("text", x = X9, y = Y , label = tbl[1,5], size = 4) +
  #   ggplot2::annotate("text", x = X10, y = Y, label = tbl[1,4], size = 4) +
  #   ggplot2::annotate("text", x = X11, y = Y , label = tbl[1,3], size = 4) +
  #   ggplot2::annotate("text", x = X12, y = Y, label = tbl[1,2], size = 4) +
  #   ggplot2::annotate("text", x = X13, y = Y , label = tbl[1,1], size = 4)

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/boxplot_density_allen_coral_", "mpa_", species, "_with_megafauna_image_zero", ".png")), p, width = 7, height = 5)


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

    ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/anova_barplot_coral_", coral_attrib, "_mpa_", species, ".png")), p, width = 7, height = 5)


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

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/twoway_test_barplot_coral_", coral_attrib, "_mpa_", species, ".png")), p, width = 7, height = 5)

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

  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/twoway_test_barplot_allen_coral_mpa_", species, ".png")), p, width = 7, height = 5)

}


#' Barplot with all species : Make two-way non parametric test (scheirerRayHare test) for density per mpa status and Allen coral habitat and barplot of the result
#'
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#' @param df
#'
#' @return
#' @export
#'

make_twoway_test_barplot_allen_coral_mpa_with_all_species_mpa_class2 <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(class2 = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df

  # remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  # df %>%
  #   dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df
  #
  # https://rcompanion.org/handbook/F_14.html
  # The Scheirer Ray Hare test is a nonparametric test used for a two-way factorial experiment.

  res1 = rcompanion::scheirerRayHare(density1 ~ mpa_status + class2, data = df)
  res2 = rcompanion::scheirerRayHare(density2 ~ mpa_status + class2, data = df)
  res3 = rcompanion::scheirerRayHare(density3 ~ mpa_status + class2, data = df)
  res4 = rcompanion::scheirerRayHare(density4 ~ mpa_status + class2, data = df)
  res5 = rcompanion::scheirerRayHare(density5 ~ mpa_status + class2, data = df)

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)



  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
                   effect =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
                   pval = c(res1$p.value[1:3], res2$p.value[1:3], res3$p.value[1:3], res4$p.value[1:3], res5$p.value[1:3]),
                   signif = rep(NA, length(15)))
                   # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")


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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Reserve", "Habitat", "Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Reserve", "Habitat", "Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col() +
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("yellow", "black", "darkgrey"), labels = c("MPA", "Habitat", "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("Effect") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
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
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 3, size = 7)

    # make dataframe for plotting with all species
  new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
                   Signifiance = signif)
  #convert new2 to csv
  write.csv2(new2, file = "mycsvnew2")



  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/twoway_test_barplot_allen_coral_mpa_with_all_species_mpa_class2.png")), p, width = 7, height = 5)

}


#' Barplot with all species : Make two-way non parametric test (scheirerRayHare test) for density per mpa status and Allen coral habitat and barplot of the result
#'
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#' @param df
#'
#' @return
#' @export
#'

make_twoway_test_barplot_allen_coral_mpa_with_all_species_class2_mpa <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(class2 = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df

  # remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  # df %>%
  #   dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df
  #
  # https://rcompanion.org/handbook/F_14.html
  # The Scheirer Ray Hare test is a nonparametric test used for a two-way factorial experiment.

  res1 = rcompanion::scheirerRayHare(density1 ~ class2 + mpa_status, data = df)
  res2 = rcompanion::scheirerRayHare(density2 ~ class2 + mpa_status, data = df)
  res3 = rcompanion::scheirerRayHare(density3 ~ class2 + mpa_status, data = df)
  res4 = rcompanion::scheirerRayHare(density4 ~ class2 + mpa_status, data = df)
  res5 = rcompanion::scheirerRayHare(density5 ~ class2 + mpa_status, data = df)

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)



  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat"),
                   effect =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
                   pval = c(res1$p.value[1:3], res2$p.value[1:3], res3$p.value[1:3], res4$p.value[1:3], res5$p.value[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")


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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Habitat", "Reserve", "Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Habitat", "Reserve",  "Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col() +
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("black", "yellow",  "darkgrey"), labels = c("Habitat", "MPA",  "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("Effect") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
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
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 3, size = 7)

  # # make dataframe for plotting with all species
  # new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
  #                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
  #                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
  #                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
  #                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
  #                   Signifiance = signif)
  # #convert new2 to csv
  # write.csv2(new2, file = "mycsvnew2")
  #


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/twoway_test_barplot_allen_coral_mpa_with_all_species_class2_mpa.png")), p, width = 7, height = 5)

}


####without zero

#' Barplot with all species : Make two-way non parametric test (scheirerRayHare test) for density per mpa status and Allen coral habitat and barplot of the result
#'
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#' @param df
#'
#' @return
#' @export
#'

make_twoway_test_barplot_allen_coral_mpa_with_all_species_mpa_class2_without_zero <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(class2 = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df


  #filter na
  df %>%
    dplyr::filter(!is.na(density1), !is.na(density2), !is.na(density3), !is.na(density4), !is.na(density5)) -> df

  # remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  # df %>%
  #   dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df
  #
  # https://rcompanion.org/handbook/F_14.html
  # The Scheirer Ray Hare test is a nonparametric test used for a two-way factorial experiment.

  res1 = rcompanion::scheirerRayHare(density1 ~ mpa_status + class2, data = df)
  res2 = rcompanion::scheirerRayHare(density2 ~ mpa_status + class2, data = df)
  res3 = rcompanion::scheirerRayHare(density3 ~ mpa_status + class2, data = df)
  res4 = rcompanion::scheirerRayHare(density4 ~ mpa_status + class2, data = df)
  res5 = rcompanion::scheirerRayHare(density5 ~ mpa_status + class2, data = df)

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)



  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
                   effect =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
                   pval = c(res1$p.value[1:3], res2$p.value[1:3], res3$p.value[1:3], res4$p.value[1:3], res5$p.value[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")


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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Reserve", "Habitat", "Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Reserve", "Habitat", "Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col() +
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("yellow", "black", "darkgrey"), labels = c("MPA", "Habitat", "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("Effect") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
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
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 3, size = 7)

  # make dataframe for plotting with all species
  new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                    Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
                    Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
                    "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
                    H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
                    "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
                    Signifiance = signif)
  #convert new2 to csv
  write.csv2(new2, file = "mycsvnew2")



  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/twoway_test_barplot_allen_coral_mpa_with_all_species_mpa_class2_without_zero.png")), p, width = 7, height = 5)

}


#' Barplot with all species : Make two-way non parametric test (scheirerRayHare test) for density per mpa status and Allen coral habitat and barplot of the result
#'
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#' @param df
#'
#' @return
#' @export
#'

make_twoway_test_barplot_allen_coral_mpa_with_all_species_class2_mpa_without_zero <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(class2 = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df

  #filter na
  df %>%
    dplyr::filter(!is.na(density1), !is.na(density2), !is.na(density3), !is.na(density4), !is.na(density5)) -> df


  # remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  # df %>%
  #   dplyr::filter(!coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df
  #
  # https://rcompanion.org/handbook/F_14.html
  # The Scheirer Ray Hare test is a nonparametric test used for a two-way factorial experiment.

  res1 = rcompanion::scheirerRayHare(density1 ~ class2 + mpa_status, data = df)
  res2 = rcompanion::scheirerRayHare(density2 ~ class2 + mpa_status, data = df)
  res3 = rcompanion::scheirerRayHare(density3 ~ class2 + mpa_status, data = df)
  res4 = rcompanion::scheirerRayHare(density4 ~ class2 + mpa_status, data = df)
  res5 = rcompanion::scheirerRayHare(density5 ~ class2 + mpa_status, data = df)

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)



  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat", "Habitat", "Reserve", "Reserve * Habitat"),
                   effect =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
                   pval = c(res1$p.value[1:3], res2$p.value[1:3], res3$p.value[1:3], res4$p.value[1:3], res5$p.value[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")


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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Habitat", "Reserve", "Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Habitat", "Reserve",  "Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col() +
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("black", "yellow",  "darkgrey"), labels = c("Habitat", "MPA",  "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("Effect") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
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
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 3, size = 7)

  # # make dataframe for plotting with all species
  # new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
  #                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
  #                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
  #                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
  #                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
  #                   Signifiance = signif)
  # #convert new2 to csv
  # write.csv2(new2, file = "mycsvnew2")
  #


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/twoway_test_barplot_allen_coral_mpa_with_all_species_class2_mpa_without_zero.png")), p, width = 7, height = 5)

}




#' Make permanova for density per mpa status and coral habitat and barplot of the result with data Allen
#'
#' @param df
#' @param species
#'
#' @return
#' @export
#'

make_permanova_barplot_allen_coral_mpa <- function(df, species){



  df %>%
    dplyr::mutate(allen_coral_hab = as.factor(get(paste("class")))) %>%
    # delete na
    dplyr::filter(!is.na(density_Dugong_certain), !is.na(density_Turtle), !is.na(density_Shark), !is.na(density_Round_ray), !is.na(density_Eagle_ray)) %>%
    as.data.frame() -> df


  #remove cells corresponding to habitats that do not have both occurrences of mpa_status following
  # table(df$mpa_status, df$coral_hab)
  # channel, deep lagoon, main land, pass reef flat, pass
  # df %>%
  #   dplyr::filter(!allen_coral_hab %in% c("channel", "deep lagoon", "main land", "pass reef flat", "pass")) ->  df

  # #eliminate outliers
  # outliers <- boxplot(df$density[df$density > 0], plot=FALSE)$out
  # df %>%
  #   dplyr::filter(!density %in% outliers) ->  df

  #convert log
  # df  %>%
  #   dplyr::mutate(log_density_Dugong_certain = log(density_Dugong_certain))

  #convert data frame (help)
  # df %>%
  #   dplyr::select(id, density_Dugong_certain, density_Turtle, density_Shark, density_Round_ray, density_Eagle_ray) -> df_response


  #convert data frame (help)
  # df %>%
  #   dplyr::select(id, density_Shark) -> df_response

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

  #convert data frame (help)
  df %>%
    dplyr::select(id, allen_coral_hab, mpa_status) -> df_env

  #permanova
  permanova <- vegan::adonis(density_taxa ~ allen_coral_hab * mpa_status, data = df_env, permutations = 999, method = "euclidean")

  # #test
  # permtest <- vegan::adonis2(dist(df_response) ~ allen_coral_hab*mpa_status, data= df_env, method = "euclidean",permutations = 999)


  print(permanova)

}



#' Barplot with all species : Permanova _mpa_habitat_without_zero
#'
#' @param df
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#'
#' @return
#' @export
#'
#' @examples
permanova_barplot_allen_coral_mpa_with_all_species_mpa_habitat_without_zero <- function(df, species1, species2, species3, species4, species5){

   df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::filter(density1>0) %>%
    dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df1
  df %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::filter(density2>0) %>%
  dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df2
  df %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::filter(density3>0) %>%
  dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df3
  df %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::filter(density4>0) %>%
  dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df4
  df %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::filter(density5>0) %>%
  dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df5


    #convert data frame (help)
  df1 %>%
    dplyr::select(id, density1, class, mpa_status) -> df_env1
  df2 %>%
    dplyr::select(id, density2, class, mpa_status) -> df_env2
  df3 %>%
    dplyr::select(id, density3, class, mpa_status) -> df_env3
  df4 %>%
    dplyr::select(id, density4, class, mpa_status) -> df_env4
  df5 %>%
    dplyr::select(id, density5, class, mpa_status) -> df_env5

   # permanova
  res1 = permanova <- vegan::adonis(density1 ~ mpa_status * class, data = df_env1, permutations = 999, method = "binomial")
  res2 = permanova <- vegan::adonis(density2 ~ mpa_status * class, data = df_env2, permutations = 999, method = "binomial")
  res3 = permanova <- vegan::adonis(density3 ~ mpa_status * class, data = df_env3, permutations = 999, method = "binomial")
  res4 = permanova <- vegan::adonis(density4 ~ mpa_status * class, data = df_env4, permutations = 999, method = "binomial")
  res5 = permanova <- vegan::adonis(density5 ~ mpa_status * class, data = df_env5, permutations = 999, method = "binomial")

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)


  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
                   effect =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                   pval = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")
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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Reserve", "Habitat", "Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Reserve", "Habitat", "Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col()+
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("yellow", "black", "darkgrey"), labels = c("MPA", "Habitat", "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("F-score") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
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
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 0.6, size = 6)

  # make dataframe for plotting with all species
  # new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
  #                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
  #                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
  #                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
  #                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
  #                   Signifiance = signif)
  # #convert new2 to csv
  # write.csv2(new2, file = "mycsvnew2")


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/permanova_barplot_allen_coral_mpa_with_all_species_mpa_habitat_without_zero.png")), p, width = 7, height = 5)

}


#' Barplot with all species : Permanova _habitat_mpa_without_zero
#'
#' @param df
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#'
#' @return
#' @export
#'
#' @examples
permanova_barplot_allen_coral_mpa_with_all_species_habitat_mpa_without_zero <- function(df, species1, species2, species3, species4, species5){
  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::filter(density1>0) %>%
    dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df1
  df %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::filter(density2>0) %>%
    dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df2
  df %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::filter(density3>0) %>%
    dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df3
  df %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::filter(density4>0) %>%
    dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df4
  df %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::filter(density5>0) %>%
    dplyr::mutate(class = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea")))-> df5


  #convert data frame (help)
  df1 %>%
    dplyr::select(id, density1, class, mpa_status) -> df_env1
  df2 %>%
    dplyr::select(id, density2, class, mpa_status) -> df_env2
  df3 %>%
    dplyr::select(id, density3, class, mpa_status) -> df_env3
  df4 %>%
    dplyr::select(id, density4, class, mpa_status) -> df_env4
  df5 %>%
    dplyr::select(id, density5, class, mpa_status) -> df_env5

  # permanova
  res1 = permanova <- vegan::adonis(density1 ~ class * mpa_status, data = df_env1, permutations = 999, method = "euclidean")
  res2 = permanova <- vegan::adonis(density2 ~ class * mpa_status, data = df_env2, permutations = 999, method = "euclidean")
  res3 = permanova <- vegan::adonis(density3 ~ class * mpa_status, data = df_env3, permutations = 999, method = "euclidean")
  res4 = permanova <- vegan::adonis(density4 ~ class * mpa_status, data = df_env4, permutations = 999, method = "euclidean")
  res5 = permanova <- vegan::adonis(density5 ~ class * mpa_status, data = df_env5, permutations = 999, method = "euclidean")

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)


  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat"),
                   effect =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                   pval = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")
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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Habitat","Reserve","Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Habitat","Reserve","Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col()+
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("black","yellow", "darkgrey"), labels = c("Habitat","MPA", "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("F-score") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y =  ggplot2::element_text(size = 15),
                   axis.ticks.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_rect(fill="white"),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 16),
                   legend.key.size = ggplot2::unit(1.5,"line"),
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 0.6, size = 6)

  # make dataframe for plotting with all species
  # new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
  #                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
  #                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
  #                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
  #                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
  #                   Signifiance = signif)
  # #convert new2 to csv
  # write.csv2(new2, file = "mycsvnew2")


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/permanova_barplot_allen_coral_mpa_with_all_species_habitat_mpa_without_zero.png")), p, width = 7, height = 5)

}



#### with zero

#' Barplot with all species : Permanova _mpa_habitat
#'
#' @param df
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#'
#' @return
#' @export
#'
#' @examples
permanova_barplot_allen_coral_mpa_with_all_species_mpa_habitat <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(Habitat = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df


  #filter na
  df %>%
    dplyr::filter(!is.na(density1), !is.na(density2), !is.na(density3), !is.na(density4), !is.na(density5)) -> df
  #
  #convert data frame (help)
  df %>%
    dplyr::select(id, Habitat, mpa_status) -> df_env


  # permanova
  res1 = permanova <- vegan::adonis(df$density1 ~ mpa_status * Habitat, data = df_env, permutations = 999, method = "euclidean")
  res2 = permanova <- vegan::adonis(df$density2 ~ mpa_status * Habitat, data = df_env, permutations = 999, method = "euclidean")
  res3 = permanova <- vegan::adonis(df$density3 ~ mpa_status * Habitat, data = df_env, permutations = 999, method = "euclidean")
  res4 = permanova <- vegan::adonis(df$density4 ~ mpa_status * Habitat, data = df_env, permutations = 999, method = "euclidean")
  res5 = permanova <- vegan::adonis(df$density5 ~ mpa_status * Habitat, data = df_env, permutations = 999, method = "euclidean")

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)


  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
                   effect =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                   pval = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")
  for (i in 1:length(new$pval)){
    if(new$pval[i] > 0.05) {new$signif[i] <- ""}
    if(new$pval[i] <= 0.05 & new$pval[i] > 0.01) {new$signif[i] <- "*"}
    if(new$pval[i] <= 0.01 & new$pval[i] > 0.001) {new$signif[i] <- "**"}
    if(new$pval[i] <= 0.001) {new$signif[i] <- "***"}
  }


  #max effect for plotting
  maxeffect = max(new$effect)
  #effect variable
  effect = new$effect
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Reserve", "Habitat", "Reserve * Habitat")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Reserve", "Habitat", "Reserve * Habitat"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col()+
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("yellow", "black", "darkgrey"), labels = c("MPA", "Habitat", "MPA * Habitat")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
    ggplot2::ylab("F-score") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
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
                   strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
    ggplot2::geom_text(data = new, label = signif, nudge_y = 0.6, size = 6)

  # make dataframe for plotting with all species
  # new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
  #                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
  #                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
  #                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
  #                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
  #                   Signifiance = signif)
  # #convert new2 to csv
  # write.csv2(new2, file = "mycsvnew2")


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/permanova_barplot_allen_coral_mpa_with_all_species_mpa_habitat.png")), p, width = 7, height = 5)

}


#' Barplot with all species : Permanova habitat + mpa
#'
#' @param df
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#'
#' @return
#' @export
#'
#' @examples
permanova_barplot_allen_coral_mpa_with_all_species_habitat_mpa <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(Habitat = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df


  #filter na
  df %>%
    dplyr::filter(!is.na(density1), !is.na(density2), !is.na(density3), !is.na(density4), !is.na(density5)) -> df

  #convert data frame (help)
  df %>%
    dplyr::select(id, Habitat, mpa_status) -> df_env


  # permanova
  res1 = permanova <- vegan::adonis(df$density1 ~ Habitat * mpa_status, data = df_env, permutations = 999, method = "euclidean")
  res2 = permanova <- vegan::adonis(df$density2 ~ Habitat * mpa_status, data = df_env, permutations = 999, method = "euclidean")
  res3 = permanova <- vegan::adonis(df$density3 ~ Habitat * mpa_status, data = df_env, permutations = 999, method = "euclidean")
  res4 = permanova <- vegan::adonis(df$density4 ~ Habitat * mpa_status, data = df_env, permutations = 999, method = "euclidean")
  res5 = permanova <- vegan::adonis(df$density5 ~ Habitat * mpa_status, data = df_env, permutations = 999, method = "euclidean")

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)


  # make new dataframe, table of results
  new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                   var = c("Habitat","Reserve","Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve", "Habitat", "Reserve", "Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve"),
                   effect =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                   pval = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                   signif = rep(NA, length(15)))
  # signif = rep(NA, length(new$pval))

  #add significance symbol
  # new$signif = ifelse(new$pval < 0.05, "*", "ns")
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
  # #max effect per effect
  # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)

  #significance vector for plotting
  signif = new$signif

  #defined positions for correct order of bars
  positions <- c("Habitat","Reserve","Habitat * Reserve")
  #defined positions for correct order of species
  new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  #defined positions for correct order of var
  new$var = factor(new$var, levels = c("Habitat","Reserve","Habitat * Reserve"))
  #defined facet wrap names
  new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")

  # # Create a grouped bar graph
  p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
    ggplot2::geom_col()+
    # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
    ggplot2::scale_fill_manual(values = c("black","yellow", "darkgrey"), labels = c("Habitat","MPA", "Habitat * MPA")) +
    ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
    ggplot2::scale_x_discrete(limits = positions) +
    # ggplot2::scale_y_continuous(position = "right") +
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

  # make dataframe for plotting with all species
  new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
                    Variables = c("Habitat","Reserve","Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve", "Habitat", "Reserve", "Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve", "Habitat","Reserve","Habitat * Reserve"),
                    Df =  c(res1$aov.tab$Df[1:3], res2$aov.tab$Df[1:3], res3$aov.tab$Df[1:3], res4$aov.tab$Df[1:3], res5$aov.tab$Df[1:3]),
                    F_score =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
                    "p.value" = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
                    Signifiance = signif)
  # #convert new2 to csv
  write.csv2(new2, here::here("outputs","data_permanova.csv"),row.names = FALSE )


  ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/permanova_barplot_allen_coral_mpa_with_all_species_habitat_mpa.png")), p, width = 7, height = 5)

}




###### test LMperm

#' Barplot with all species : LMperm
#'
#' @param df
#' @param species1
#' @param species2
#' @param species3
#' @param species4
#' @param species5
#'
#' @return
#' @export
#'
#' @examples
lmperm_test_habitat_mpa <- function(df, species1, species2, species3, species4, species5){

  df %>%
    dplyr::mutate(density1 = get(paste0("density_", species1))) %>%
    dplyr::mutate(density2 = get(paste0("density_", species2))) %>%
    dplyr::mutate(density3 = get(paste0("density_", species3))) %>%
    dplyr::mutate(density4 = get(paste0("density_", species4))) %>%
    dplyr::mutate(density5 = get(paste0("density_", species5))) %>%
    dplyr::mutate(Habitat = factor(class, levels = c("Coral/Algae", "Microalgal Mats", "Rock", "Rubble", "Sand", "Seagrass", "Deep_sea"))) -> df

  # #mutate Na to 0
  # df %>%
  #   dplyr::mutate_at(df, c("density1", "density2", "density3", "density4", "density5"), ~replace(., is.na(.), 0)) -> df


  #filter na
  # df %>%
  #   dplyr::filter(!is.na(density1), !is.na(density2), !is.na(density3), !is.na(density4), !is.na(density5)) -> df

  #convert data frame (help)
  df %>%
    dplyr::select(id, Habitat, mpa_status) -> df_env


  # LMperm
  res1 = lmperm <- permuco::lmperm(df$density1 ~ Habitat * mpa_status, data = df_env, np = 5000, method = "freedman_lane", type = "permutation")
  res2 = lmperm <- permuco::lmperm(df$density2 ~ Habitat * mpa_status, data = df_env, np = 5000, method = "freedman_lane", type = "permutation")
  res3 = lmperm <- permuco::lmperm(df$density3 ~ Habitat * mpa_status, data = df_env, np = 5000, method = "freedman_lane", type = "permutation")
  res4 = lmperm <- permuco::lmperm(df$density4 ~ Habitat * mpa_status, data = df_env, np = 5000, method = "freedman_lane", type = "permutation")
  res5 = lmperm <- permuco::lmperm(df$density5 ~ Habitat * mpa_status, data = df_env, np = 5000, method = "freedman_lane", type = "permutation")

  print(res1)
  print(res2)
  print(res3)
  print(res4)
  print(res5)
  #
  #
  # # make new dataframe, table of results
  # new = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                  var = c("Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat", "Habitat","Reserve","Reserve * Habitat"),
  #                  effect =  c(res1$aov.tab$F.Model[1:3], res2$aov.tab$F.Model[1:3], res3$aov.tab$F.Model[1:3], res4$aov.tab$F.Model[1:3], res5$aov.tab$F.Model[1:3]),
  #                  pval = c(res1$aov.tab$`Pr(>F)`[1:3], res2$aov.tab$`Pr(>F)`[1:3], res3$aov.tab$`Pr(>F)`[1:3], res4$aov.tab$`Pr(>F)`[1:3], res5$aov.tab$`Pr(>F)`[1:3]),
  #                  signif = rep(NA, length(15)))
  # # signif = rep(NA, length(new$pval))
  #
  # #add significance symbol
  # # new$signif = ifelse(new$pval < 0.05, "*", "ns")
  # for (i in 1:length(new$pval)){
  #   if(new$pval[i] > 0.05) {new$signif[i] <- "ns"}
  #   if(new$pval[i] <= 0.05 & new$pval[i] > 0.01) {new$signif[i] <- "*"}
  #   if(new$pval[i] <= 0.01 & new$pval[i] > 0.001) {new$signif[i] <- "**"}
  #   if(new$pval[i] <= 0.001) {new$signif[i] <- "***"}
  # }
  #
  #
  # #max effect for plotting
  # maxeffect = max(new$effect)
  # #effect variable
  # effect = new$effect
  # # #max effect per effect
  # # effect_per_effect = (new$effect[species=="species1"&var=="Mpa"] + 2)
  #
  # #significance vector for plotting
  # signif = new$signif
  #
  # #defined positions for correct order of bars
  # positions <- c("Habitat","Reserve","Reserve * Habitat")
  # #defined positions for correct order of species
  # new$Species = factor(new$Species, levels = c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray"))
  # #defined positions for correct order of var
  # new$var = factor(new$var, levels = c("Habitat","Reserve","Reserve * Habitat"))
  # #defined facet wrap names
  # new_sp_names = c("Dugong", "Sea turtle", "Shark", "Dasyat. ray", "Myliob. ray")
  # names(new_sp_names) <- c("Dugong_certain", "Turtle", "Shark", "Round_ray", "Eagle_ray")
  #
  # # # Create a grouped bar graph
  # p = ggplot2::ggplot(new, ggplot2::aes(x = var, y = effect, fill = var)) +
  #   ggplot2::geom_col()+
  #   # ggplot2::scale_fill_manual(values = c("yellow", "dodgerblue4", "skyblue3")) +
  #   ggplot2::scale_fill_manual(values = c("black","yellow", "darkgrey"), labels = c("Habitat","MPA", "MPA * Habitat")) +
  #   ggplot2::facet_wrap(~Species, nrow = 1, scales = "free_x", labeller = ggplot2::labeller(Species = new_sp_names)) +
  #   ggplot2::scale_x_discrete(limits = positions) +
  #   # ggplot2::scale_y_continuous(position = "right") +
  #   ggplot2::ylab("F-score") +
  #   ggplot2::xlab("") +
  #   ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold", size = 15),
  #                  axis.text.x = ggplot2::element_blank(),
  #                  axis.text.y =  ggplot2::element_text(size = 15),
  #                  axis.ticks.x = ggplot2::element_blank(),
  #                  panel.background = ggplot2::element_rect(fill = "white", colour = "white",size = 0.5, linetype = "solid"),
  #                  panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',colour = "white"),
  #                  panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',colour = "white"),
  #                  plot.title = ggplot2::element_text(hjust = 0.5),
  #                  legend.position = "bottom",
  #                  legend.title = ggplot2::element_blank(),
  #                  legend.text = ggplot2::element_text(size = 16),
  #                  legend.key.size = ggplot2::unit(1.5,"line"),
  #                  strip.text.x = ggplot2::element_text(size = 15.5, color="black", face="bold")) +
  #   ggplot2::geom_text(data = new, label = signif, nudge_y = 0.6, size = 6)
  #
  # make dataframe for plotting with all species
  # new2 = data.frame(Species = c(species1, species1, species1, species2, species2, species2, species3, species3, species3, species4, species4, species4, species5, species5, species5),
  #                   Variables = c("Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat", "Reserve", "Habitat", "Reserve * Habitat","Reserve", "Habitat", "Reserve * Habitat"),
  #                   Df =  c(res1$Df[1:3], res2$Df[1:3], res3$Df[1:3], res4$Df[1:3], res5$Df[1:3]),
  #                   "Sum Sq" =  c(res1$"Sum Sq"[1:3], res2$"Sum Sq"[1:3], res3$"Sum Sq"[1:3], res4$"Sum Sq"[1:3], res5$"Sum Sq"[1:3]),
  #                   H =  c(res1$H[1:3], res2$H[1:3], res3$H[1:3], res4$H[1:3], res5$H[1:3]),
  #                   "p.value" = c(res1$"p.value"[1:3], res2$"p.value"[1:3], res3$"p.value"[1:3], res4$"p.value"[1:3], res5$"p.value"[1:3]),
  #                   Signifiance = signif)
  # #convert new2 to csv
  # write.csv2(new2, file = "mycsvnew2")


  # ggplot2::ggsave(here::here(paste0("outputs/poe_on_effort/permanova_barplot_allen_coral_mpa_with_all_species_habitat_mpa.png")), p, width = 7, height = 5)
  #
}
