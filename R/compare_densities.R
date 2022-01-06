
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




#' Boxplot of mean density per mpa status (in/out mpa) (0s removed)
#'
#' @param df
#' @param species

boxplot_density_mpa <- function(df, species){

  df %>%
    dplyr::mutate(density = get(paste0("density_", species))) %>%
    #exclude 0s
    dplyr::filter(density>0) -> df_no0

  p = ggpubr::ggboxplot(df_no0, x = "mpa_status", y = paste0("density_", species))
  p = ggpubr::ggpar(p, rotate = TRUE)

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

