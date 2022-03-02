
#' Read telemetry data
#'
#' @return
#' @export
#'

read_telem <- function(){

  #list all csv files in the telemetry folder
  filenames = list.files(here::here("data/telemetry"), pattern="csv")

  telemlist = list()

  for (i in 1:length(filenames)) {
    #read telemetry data from filename
    telem = readr::read_delim(here::here("data/telemetry", filenames[i]),
                              delim = ";",
                              local = readr::locale(decimal_mark = "."),
                              col_types = list(readr::col_double(), readr::col_double(), readr::col_double(), readr::col_double(),
                                               readr::col_character(), readr::col_character(), readr::col_character(), readr::col_character(),
                                               readr::col_character()))
    #extract video_id from filename
    video_id = stringr::str_sub(filenames[i], 1, 8)

    #format telemetry data
    telem %>%
      #add video_id as column
      dplyr::mutate(video_id = video_id) %>%
      #make image_id based on the video_id
      dplyr::mutate(image_id = paste(video_id, frame, sep="_")) -> telem_new

    telemlist[[i]] = telem_new # add it to list
  }

  #append all telemetry data
  telem = dplyr::bind_rows(telemlist)

  #return
  return(telem)

}

#' Clean telemetry data
#'
#' @param telem
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return
#' @export
#'

clean_telem <- function(telem, lat1, lon1, lat2, lon2){

  #remove points outside region
  telem %>%
    dplyr::filter(lon > lon1 & lon < lon2 & lat < lat1 & lat > lat2) -> telem_new
    #select columns
    #dplyr::select(image_id, video_id, lat, lon, alt, object) -> telem_new

  return(telem_new)

}



#' Clean duplicated observations in telemetry
#'
#' @param overl
#' @param im_height
#' @param telem
#'
#' @return
#' @export
#'

clean_duplicated_obs_telemetry <- function(telem, overl, im_height){

  #### format telemetry

  telem %>%
    #calculate y coordinate of object center
    dplyr::mutate(center_y = as.numeric(start_y) + (as.numeric(end_y) - as.numeric(start_y))/2) %>%
    #drops lines with no object
    tidyr::drop_na(object) %>%
    #drops coral and plane shadow
    dplyr::filter(!object %in% c("Coral", "Plane_shadow"))  -> telem2

  #define overlap height window
  #ie height in pixels that corresponds to overlapping portions of consecutive images
  window = overl * im_height


  #### initialize

  i=1

  #get image_id for consecutive images
  img = telem2$image_id[i]
  img2 = paste0(sub("_.*", "", img), "_", as.numeric(sub(".*_", "", img))+1)

  #get corresponding data subsets
  sub_1 = subset(telem2, telem2$image_id == img)
  sub_2 = subset(telem2, telem2$image_id == img2)

  #loops on data subsets
  for (j in 1:nrow(sub_2)){
    print(j)

    for (k in 1:nrow(sub_1)){
      print(k)

      #if the same object is annotated in consecutive images
      if (sub_2$object[j] == sub_1$object[k]){

        #if y_center of the object in image2 is within that window of y_center of the object image1
        #the object in image2 is a duplicate
        if (sub_2$center_y[j] >  sub_1$center_y[k] - window &
            sub_2$center_y[j] <  sub_1$center_y[k] + window){

           print("object within window")
           sub_1$duplicate[k] = "no"
           sub_2$duplicate[j] = "yes"

        }
      }
    }
  }

  #bind
  sub = rbind(sub_1, sub_2)



  ###### loop

  for (i in 2:(nrow(telem2)-1)){

    cat("------------", i, "\n")

    #get image_id for consecutive images
    img = telem2$image_id[i]
    img2 = paste0(sub("_.*", "", img), "_", as.numeric(sub(".*_", "", img))+1)

    #get corresponding data subsets
    sub_1 = subset(telem2, telem2$image_id == img)
    sub_2 = subset(telem2, telem2$image_id == img2)

    #apply treatment if sub_2 exists (ie image_id are consecutive)
    if (nrow(sub_2) !=0) {

      cat("consecutive image_id \n")

      #loops on data subsets
      for (j in 1:nrow(sub_2)){
        print(j)

        for (k in 1:nrow(sub_1)){
          print(k)

          #initialize
          sub_1$duplicate[k] = "no"

          #if the same object is annotated in consecutive images
          if (sub_2$object[j] == sub_1$object[k]){
            print("same object")

            #if y_center of the object in image2 is within that window of y_center of the object image1
            #the object in image2 is a duplicate
            if (sub_2$center_y[j] >  sub_1$center_y[k] - window &
                sub_2$center_y[j] <  sub_1$center_y[k] + window){

              print("object within window")
              sub_2$duplicate[j] = "yes"

            }else{ #####added

              print("object not within window")
              sub_2$duplicate[j] = "no"
            }

          }else{

            print("different object")
            sub_2$duplicate[j] = "no"

          }
        }
      }

      #bind
      sub = rbind(sub, sub_1, sub_2)

    }else{

      cat("non consecutive image_id \n")

      sub_1$duplicate = "no"
      sub = rbind(sub, sub_1)

    }
  }

  #### remove supplementary row from processing

  #the processing may lead to supplementary rows with exact same columns
  #we remove all supplementary rows

  sub %>%
    dplyr::distinct() -> sub1

  #the processing leads to supplementary row with duplicate = "no" after the same row with duplicate = "yes"
  #we remove the first row (with duplicate = "no")
  #.keep_all: If TRUE, keep all variables in .data. If a combination of ... is not distinct, this keeps the first row of values.

  sub1 %>%
    dplyr::distinct(frame, lat, lon, alt, object, start_x, start_y, end_x, end_y, video_id, image_id, .keep_all = TRUE) -> sub2

  #### print table
  print(table(sub2$duplicate, sub2$object))


  #### finally remove object duplicates

  sub2 %>%
    dplyr::filter(duplicate == "no") -> sub_unique

  return(sub_unique)

}



#' select observations for Poe
#'
#' @param telem_obs
#' @param ls_poe_vids
#'
#' @return
#' @export
#'

select_obs_telemetry_poe <- function(telem_obs, ls_poe_vids){

  telem_obs %>%
    dplyr::filter(video_id %in% ls_poe_vids) -> telem_obs_new

  return(telem_obs_new)

}



#' Read video information
#'
#' @return
#' @export
#'

read_video_info <- function(){

  vid = readxl::read_excel(here::here("data/telemetry/", "informations vidéo_ulm_news.xlsx"))

  return(vid)

}



#' Clean video information
#'
#' @return
#' @export
#'

clean_video_info <- function(vid){

  vid %>%
    dplyr::select(DATE, VIDEO_ID, EFFORT_POE) %>%
    #convert column names to lower case
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(date = as.factor(date)) %>%
    tidyr::drop_na(date) -> vid_new

  return(vid_new)

}



#' Get list of on-effort Poe videos
#'
#' @param vid
#'
#' @return
#' @export
#'

list_poe_videos <- function(vid){

  vid %>%
    dplyr::filter(effort_poe == "OUI") -> vid_poe

  ls = vid_poe$video_id

  return(ls)
}




#' Read off effort portion Poe
#'
#' @return
#' @export
#'

read_off_effort_poe <- function(){

  off = readxl::read_excel(here::here("data/telemetry/", "Transects Poe.xlsx"), col_types = c("date", "text", rep("numeric", 4), "text"), col_names = TRUE)

  return(off)

}



#' Clean off effort portion related to transits Poe
#'
#' @param off
#'
#' @return
#' @export
#'

clean_off_effort_transit_poe <- function(off){

  off %>%
    #rename
    dplyr::mutate(start_transit_image_id = Début_transit,
                  end_transit_image_id = Fin_transit) %>%
    #convert column names to lower case
    dplyr::rename_with(tolower) %>%
    #select
    dplyr::select(c(start_transit_image_id, end_transit_image_id, video_id)) %>%
    #drop nas
    tidyr::drop_na() -> off_new

  return(off_new)

}



#' Clean off effort portion related to loops Poe
#'
#' @param off
#'
#' @return
#' @export
#'

clean_off_effort_loop_poe <- function(off){

  off %>%
    #rename
    dplyr::mutate(start_loop_image_id = Début_boucle,
                  end_loop_image_id = Fin_boucle) %>%
    #convert column names to lower case
    dplyr::rename_with(tolower) %>%
    #select
    dplyr::select(c(start_loop_image_id, end_loop_image_id, video_id)) %>%
    #drop nas
    tidyr::drop_na() -> off_new

  return(off_new)

}


#' Select on effort data (telemetry or observations) Poe
#'
#' @param telem_poe
#' @param off
#'
#' @return
#' @export
#'

select_on_effort_poe <- function(telem_poe, off_transit, off_loop){

  #get list of off effort points related to loop
  off_loop = mapply(function(x, y, z) paste0(x, "_", y:z),
                    off_loop$video_id,
                    off_loop$start_loop_image_id,
                    off_loop$end_loop_image_id)

  #get list of off effort points related to transit
  off_transit = mapply(function(x, y, z) paste0(x, "_", y:z),
                       off_transit$video_id,
                       off_transit$start_transit_image_id,
                       off_transit$end_transit_image_id)

  telem_poe %>%
    #filter off effort points related to loop
    dplyr::filter(!image_id %in% unlist(off_loop)) %>%
    #filter off effort points related to transit
    dplyr::filter(!image_id %in% unlist(off_transit)) -> telem_poe_on_effort

  return(telem_poe_on_effort)

}



#' Select Poe videos
#'
#' @param vid
#' @param list
#'
#' @return
#' @export
#'

select_poe_videos <- function(vid, list){

  vid %>%
    dplyr::filter(video_id %in% list) -> vid_poe

  return(vid_poe)

}




#' Join video information to telemetry
#'
#' @return
#' @export
#'

join_video_info_telem <- function(vid, telem){

    joined = dplyr::left_join(telem, vid, by = "video_id")

    return(joined)

}



#' Join video information to telemetry for Poe data
#'
#' @return
#' @export
#'

join_video_poe_info_telem <- function(vid, telem){

  dplyr::right_join(telem, vid, by = "video_id") %>%
    dplyr::select(-c(date.y, effort_poe.y)) %>%
    dplyr::rename(date = date.x) %>%
    dplyr::rename(effort_poe = effort_poe.x) -> joined

  return(joined)

}




#' Read transect points
#'
#' @return
#' @export
#'
#' @examples

read_transects_points <- function(shapefile){

  pts <- rgdal::readOGR(dsn = here::here("data/transects/"), layer = shapefile)

  #rename columns
  if (shapefile != "megafauna1_points_latlon") {
    colnames(pts@data) = c("id", "xcoord", "ycoord", "lon_dms",  "lat_dms")
  }

  return(pts)

}


#' Make transect lines from points per sector
#'
#' @param pts
#' @param sector
#'
#' @return
#'

make_transect_lines <- function(pts, sector){

  #reproject to wgs84
  crs_wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  pts2 <- sp::spTransform(pts, crs_wgs84)

  #order based on id
  pts2@data$id = as.numeric(pts2@data$id)
  new = pts2@data[order(pts2@data$id),]
  coordinates(new) <- ~ xcoord + ycoord

  #add id_line
  if (sector == "1") {
    new$id_line = paste0("1_", c(rep(1:24, each=2)))
  }
  if (sector == "2") {
    new$id_line = paste0("2_", c(rep(1:33, each=2), 35, 36, 35, 36, 37, 38, 37, 38, 39, 40, 39, 40, 41, 42, 41, 42,
                                43, 44, 43, 44, 45, 46, 45, 46, 47, 48, 47, 48, 49, 50, 49, 50,
                                51, 52, 51, 52, 53, 54, 53, 54, 55, 56, 55, 56,
                                57, 58, 57, 58, 59, 60, 59, 60, 61))
  }
  if (sector == "3") {
    new$id_line = paste0("3_", rep(1:168, each=2))
  }

  #list of Lines per id, each with one Line in a list ********does not work inside function********
  x <- lapply(split(new, new$id_line), function(x) Lines(list(Line(coordinates(x))), x$id_line[1L]))

  #make spatial lines
  lns <- SpatialLines(x)

  #make dataframe for the lines groupes by id
  data <- data.frame(id = unique(new$id_line))
  rownames(data) <- data$id

  #make spatial lines data frame
  l <- SpatialLinesDataFrame(lns, data)

  return(l)

}




#' Merge transect lines of 3 sectors
#'
#' @param lns1
#' @param lns2
#' @param lns3
#'
#' @return
#' @export
#'

merge_transect_lines <- function(lns1, lns2, lns3){

  lns = raster::union(lns1, lns2)
  lns = raster::union(lns, lns3)

  return(lns)

}




#' Read New Caledonia coral shapefile
#'
#' @return
#' @export
#'

read_coralnc <- function(){

  # read shapefile
  shp = rgdal::readOGR(here::here("data/geomorpho", "ae182c30-5b06-420a-bce6-52712658dff02020413-1-1lk3qk7.n062k.shp"))
  # Shapefile reprojection
  coral <- sp::spTransform(shp,"+proj=longlat +datum=WGS84")
  return(coral)

}


#' Read and convert New Caledonia Allen coral shapefile benthic
#'
#' @param lon1
#' @param lon2
#' @param lat2
#' @param lat1
#'
#' @return
#' @export
#'

read_crop_and_convert_allen_coralnc_benthic <- function(lon1, lon2, lat2, lat1){

  # read shapefile
  data_allen = sf::st_read(dsn = "data/allen/benthic_sm.gpkg", stringsAsFactors = FALSE)

  #crop
  data_allen_crop = sf::st_crop(data_allen, xmin = lon1, ymin = lat2, xmax = lon2, ymax = lat1)

  #convert sf to spatialPolygonsDataFrame
  sf_allen_coral_poly = sf:::as_Spatial(data_allen_crop)

  return(sf_allen_coral_poly)

}


#' Read and convert New Caledonia Allen coral shapefile geomorphic
#'
#' @param lon1
#' @param lon2
#' @param lat2
#' @param lat1
#'
#' @return
#' @export
#'

read_crop_and_convert_allen_coralnc_geomorphic <- function(lon1, lon2, lat2, lat1){

  # read shapefile
  data_allen = sf::st_read(dsn = "data/allen/geomorphic.gpkg", stringsAsFactors = FALSE)

  #crop
  data_allen_crop = sf::st_crop(data_allen, xmin = lon1, ymin = lat2, xmax = lon2, ymax = lat1)

  #convert sf to spatialPolygonsDataFrame
  sf_allen_coral_poly = sf:::as_Spatial(data_allen_crop)

  return(sf_allen_coral_poly)

}


#' Make coral cover raster
#'
#' @param cor
#' @param rast
#'
#' @return
#' @export
#'

make_coral_cover_raster <- function(cor, rast){

  # check if there are invalid polygon geometries & correct that
  rgeos::gIsValid(cor) #returns FALSE
  cor2 = rgeos::gBuffer(cor, byid = TRUE, width = 0) #corrects invalid geometries
  rgeos::gIsValid(cor2) #returns TRUE

  # select reef polygons
  cor3 = cor2[cor2@data$reef == 1,]

  # Crop polygon to the desired extent, then plot
  out = raster::crop(cor3, raster::extent(rast))

  # fraction of each cell covered by reef
  # The rasterize function divides the raster cell into 100 subunits,
  # and the output is the proportion of those that were covered by the polygon.
  r = raster::rasterize(out, rast, getCover = T)

  #write raster
  #raster::writeRaster(r, here::here("/predictors/coral_cover.grd"), overwrite=TRUE)

  return(r)
}


#' Read New Caedonia MPA shapefile
#'
#'
#' @return
#' @export
#'

read_mpanc <- function(){

  # Read 3 shapefiles of new caledonia mpas
  shp0 <- rgdal::readOGR(dsn = here::here("data/mpas/shp_0", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp1 <- rgdal::readOGR(dsn = here::here("data/mpas/shp_1", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))
  shp2 <- rgdal::readOGR(dsn = here::here("data/mpas/shp_2", "WDPA_WDOECM_Mar2021_Public_NCL_shp-polygons.shp"))

  # Merge
  shp = rbind(shp0, shp1, shp2)

  return(shp)

}



#' Extract Poe MPA
#'
#' @param pa
#'
#' @return
#' @export
#'

extract_mpa_poe <- function(pa){

  pa_poe = pa[pa$NAME=="PoÃ©",]

  return(pa_poe)

}



#' Make raster for study area with given resolution
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#' @param res
#'
#' @return
#' @export
#'

make_area_raster <- function(lat1, lon1, lat2, lon2, res){

  # create raster for study area
  r = raster::raster(ext = raster::extent(lon1, lon2, lat2, lat1), resolution = res) #resolution in degrees
  raster::values(r) = 1:raster::ncell(r)

  # project raster
  # rproj = raster::projectRaster(r, crs="+init=epsg:3163") #NC projection

  return(r)

}




#' Convert telemetry to spatialpoints dataframe
#'
#' @param telem
#'
#' @return
#' @export
#'

convert_telemetry_spatial <- function(telem){

  telem %>%
    tidyr::drop_na(lon)  %>%
    tidyr::drop_na(lat) -> telem2

  xy = telem2[,c("lon", "lat")]
  telem_sp = sp::SpatialPointsDataFrame(coords = xy, data = telem2,
                                        proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  return(telem_sp)

}





#' create buffer of given width in meters around transect lines
#'
#' @param lns
#' @param buf_width
#'
#' @return
#' @export
#'

create_buffer_transects <- function(lns, buf_width){

  #assign projection to lines
  proj4string(lns) = CRS("+proj=longlat")

  #project to Lambert NC
  lns_proj = sp::spTransform(lns, "+init=epsg:3163")

  #make buffer
  lns_buf = rgeos::gBuffer(lns_proj[-81,], width = buf_width) #width in meters
  #gbuffer gives an error for line 81 which seems empty, so we remove this line

  #reproject to lat/lon
  lns_proj <- sp::spTransform(lns_buf, "+proj=longlat")

  return(lns_proj)

}




#' select telemetry points in buffer
#'
#' @param telem_sp
#' @param buf
#'
#' @return
#' @export
#'

select_telemetry_in_buffer <- function(telem_sp, buf){

  telem_effort = rgeos::gDifference(telem_sp, buf)

  return(telem_effort)

}


#' Read megafauna image in png format
#'
#' @param species
#'
#' @return
#' @export
#'

read_megafauna_image <- function(species){

  img = png::readPNG(paste0(here::here("data/images"),"/", species, ".png"))

  return(img)

}
