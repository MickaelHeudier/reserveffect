
#load all functions
devtools::load_all()


######################################## DEFINE STUDY AREA ########################################

#Define study area coordnates
#small region
lat1_sm =  -21.52 ; lat2_sm = -21.65
lon1_sm = 165.225 ; lon2_sm = 165.45
# lat1_sm =  -21.51 ; lat2_sm = -21.66
# lon1_sm = 165.21 ; lon2_sm = 165.45
#big region
lat1_bg =  -21.2 ; lat2_bg = -22
lon1_bg = 164.6 ; lon2_bg = 166

######################################## CALCULATE FLIGHT PARAMETERS ########################################

#image footprint
mean_altitude = 47 # in meters (in ft: 155) from ulm altimeter (gopro altitude unreliable)
fov = 86.7 # field of view in degrees from gopro hero 7 black manual
footprint_width = 2 * tanpi(fov / (2*180)) * mean_altitude # in meters, trigonometry formula
image_height = 1520 #in pixels (2.7k gopro format)
image_width = 2704 #in pixels (2.7k gopro format)
footprint_height = footprint_width * image_height/image_width #in meters

#overlap between successive images (given hyp of linear flight)
mean_speed = 30.55 # in m/s (in km/h: 110) from ulm
frame_per_s = 3 # extraction rate in megafauna-project
overlap = (mean_speed / frame_per_s) / footprint_height


######################################## READ and CLEAN TELEMETRY DATA ########################################

#read telemetry data
telemetry_all = reserveffect::read_telem()

#clean telemetry data
telemetry = reserveffect::clean_telem(telemetry_all, lat1_bg, lon1_bg, lat2_bg, lon2_bg)

#Read video information
videos = reserveffect::read_video_info()

#Clean video information
videos = reserveffect::clean_video_info(videos)

#Join video information to telemetry
telemetry = reserveffect::join_video_info_telem(videos, telemetry)

#Clean duplicated obs from telemetry (takes about 20 minutes to process)
telemetry_obs = reserveffect::clean_duplicated_obs_telemetry(telemetry, overlap, image_height)

#Get list of Poe videos
ls_poe_videos = reserveffect::list_poe_videos(videos)

#select video information for Poe
videos_poe = reserveffect::select_poe_videos(videos, ls_poe_videos)

#Join video information to telemetry
telemetry_poe = reserveffect::join_video_poe_info_telem(videos_poe, telemetry)

#select obs for Poe
telemetry_obs_poe = reserveffect::select_obs_telemetry_poe(telemetry_obs, ls_poe_videos)

#count total number of individuals observed per species
telemetry_obs_poe %>%
  dplyr::group_by(object) %>%
  dplyr::summarise(n_tot = dplyr::n())

# # #read images png
img_Dugong_certain = reserveffect::read_megafauna_image("Dugong_certain")
img_Turtle = reserveffect::read_megafauna_image("Turtle")
img_Shark = reserveffect::read_megafauna_image("Shark")
img_Round_ray = reserveffect::read_megafauna_image("Round_ray")
img_Eagle_ray = reserveffect::read_megafauna_image("Eagle_ray")

######################################## READ CORAL DATA ########################################

# read coral geomorpology polygons
#adapt news polygons
coral_poly = reserveffect::read_coralnc()

# make study area raster (resolution 0.001 degrees)
rast_sm = reserveffect::make_area_raster(lat1_sm, lon1_sm, lat2_sm, lon2_sm, 0.0025)

# make coral cover raster
coral_cover_sm = reserveffect::make_coral_cover_raster(coral_poly, rast_sm)


######################################## READ MPA DATA ########################################

# read mpa polygon
mpa = reserveffect::read_mpanc()

# extract mpa polygon for poe
mpa_poe = reserveffect::extract_mpa_poe(mpa)



######################################## READ TRANSECT DATA ########################################

points1 = reserveffect::read_transects_points("megafauna1_points_latlon")

library(sp)
lines1 = reserveffect::make_transect_lines(points1, "1")
#lapply does not work in function *****RUN OUTSIDE FUNCTION
pts = points1
sector = "1"
lines1 = l

####### OSM maps
maplatlon_sm = reserveffect::osm_map(lat1_sm, lon1_sm, lat2_sm, lon2_sm) #bing (satellite view)

#map transects
#with scalebar and mpa
reserveffect::map_transects_scalebar_mpa(maplatlon_sm, lines1, mpa_poe, "small", lat1_sm, lon1_sm, lat2_sm, lon2_sm,
                                         dist = 2, offset_lon = 0.07, offset_lat = 0.01)


############################################ MAKE BASIC MAPS #############################################################

###### Map coral polygons
reserveffect::map_coral_poly(maplatlon_sm, coral_poly, "l1_attrib", "small")
reserveffect::map_coral_poly(maplatlon_sm, coral_poly, "l2_attrib", "small")
reserveffect::map_coral_poly(maplatlon_sm, coral_poly, "l3_attrib", "small")
reserveffect::map_coral_poly(maplatlon_sm, coral_poly, "l4_attrib", "small")

###### Map coral cover
reserveffect::map_coral_cover(maplatlon_sm, coral_cover_sm, "small")

##### Map MPAs
reserveffect::map_mpa(maplatlon_sm, mpa, "small")

##### Map MPA POE
reserveffect::map_mpa_poe(maplatlon_sm, mpa_poe, "small")



# # ~~~~~~~~~~~~~~~~~~~~~~~~ all data ~~~~~~~~~~~~~~~~~~~~~~~~
#
# ###### telemetry only
# reserveffect::map_telemetry(maplatlon_sm, telemetry, "small")
# reserveffect::map_telemetry(maplatlon_bg, telemetry, "big")
#
# ###### telemetry with one color per date
# reserveffect::map_telemetry_date(maplatlon_sm, telemetry, "small")
# reserveffect::map_telemetry_date(maplatlon_bg, telemetry, "big")
#
# ###### telemetry with separate map per date
# reserveffect::map_telemetry_date_separate(maplatlon_sm, telemetry, "small")
# reserveffect::map_telemetry_date_separate(maplatlon_bg, telemetry, "big")
#
# ####### telemetry with all species
# reserveffect::map_all_species_telemetry(maplatlon_sm, telemetry, telemetry_obs, "small")
# reserveffect::map_all_species_telemetry(maplatlon_bg, telemetry, telemetry_obs, "big")
#
# ####### telemetry with individual species
# reserveffect::map_indiv_species_telemetry(maplatlon_sm, telemetry, telemetry_obs, "small")
# reserveffect::map_indiv_species_telemetry(maplatlon_bg, telemetry, telemetry_obs, "big")
#
# ####### telemetry with individual species with separate map per species
# #reserveffect::map_indiv_species_telemetry_separate(maplatlon_sm, telemetry, telemetry_obs, "small")
# reserveffect::map_indiv_species_telemetry_separate(maplatlon_bg, telemetry, telemetry_obs, "big")
#
# ###### telemetry and transects lines
# reserveffect::map_telemetry_transects(maplatlon_sm, telemetry, lines, "small")
# reserveffect::map_telemetry_transects(maplatlon_bg, telemetry, lines, "big")



# # ~~~~~~~~~~~~~~~~~~~~~~~~ only poe data ~~~~~~~~~~~~~~~~~~~~~~~~
#
# ###### telemetry only
# reserveffect::map_telemetry(maplatlon_sm, telemetry_poe, "small", TRUE)
#
# ###### telemetry with one color per date
# reserveffect::map_telemetry_date(maplatlon_sm, telemetry_poe, "small", TRUE)
#
# ###### telemetry with separate map per date
# reserveffect::map_telemetry_date_separate(maplatlon_sm, telemetry_poe, "small", TRUE)
#
# ###### telemetry with separate map per video id
# reserveffect::map_telemetry_video_id_separate(maplatlon_sm, telemetry_poe, "small", TRUE)
#
# ####### telemetry with all species
# reserveffect::map_all_species_telemetry(maplatlon_sm, telemetry_poe, telemetry_obs_poe, "small", TRUE)
#
# ####### telemetry with individual species
# reserveffect::map_indiv_species_telemetry(maplatlon_sm, telemetry_poe, telemetry_obs_poe, "small", TRUE)
#
# ####### telemetry with individual species with separate map per species
# reserveffect::map_indiv_species_telemetry_separate(maplatlon_sm, telemetry_poe, telemetry_obs_poe, "small", TRUE)
#
# ###### telemetry and transects lines
# reserveffect::map_telemetry_transects(maplatlon_sm, telemetry_poe, lines, "small", TRUE)


###################################### SELECT ON EFFORT TELEMETRY FOR POE #####################################

#read off effort portions Poe
off_effort = reserveffect::read_off_effort_poe()

#clean effort portions related to transit Poe
off_effort_transit = reserveffect::clean_off_effort_transit_poe(off_effort)

#clean effort portions related to loop Poe
off_effort_loop = reserveffect::clean_off_effort_loop_poe(off_effort)

#select on effort telemetry and observations Poe
telemetry_poe_on = reserveffect::select_on_effort_poe(telemetry_poe, off_effort_transit, off_effort_loop)
telemetry_obs_poe_on = reserveffect::select_on_effort_poe(telemetry_obs_poe, off_effort_transit, off_effort_loop)

#Convert telemetry (all) to spatialpoints dataframe
telemetry_poe_sp = reserveffect::convert_telemetry_spatial(telemetry_poe)

#Convert telemetry (on effort) to spatialpoints dataframe
telemetry_poe_on_sp = reserveffect::convert_telemetry_spatial(telemetry_poe_on)

#quick plot
sp::plot(telemetry_poe_sp, cex = 0.3, col = 2)
sp::plot(telemetry_poe_on_sp, cex = 0.3, add = TRUE)

#map telemetry with separate map per date
map_telemetry_date_separate_on_off(maplatlon_sm, telemetry_poe, telemetry_poe_on, "small", just_poe = TRUE)


#### Redo maps for Poe on effort

###### telemetry with all species Poe on effort
reserveffect::map_all_species_telemetry_poe_on(maplatlon_sm, telemetry_poe_on, telemetry_obs_poe_on)

###### telemetry with individual species Poe on effort
reserveffect::map_indiv_species_telemetry_poe_on(maplatlon_sm, telemetry_poe_on, telemetry_obs_poe_on)

###### telemetry with individual species with separate map per species Poe on effort
reserveffect::map_indiv_species_telemetry_separate_poe_on(maplatlon_sm, telemetry_poe_on, telemetry_obs_poe_on)

###### telemetry with one color per date Poe on effort
reserveffect::map_telemetry_date_poe_on(maplatlon_sm, telemetry_poe_on)

###### telemetry with separate map per date Poe on effort
reserveffect::map_telemetry_date_separate_poe_on(maplatlon_sm, telemetry_poe_on)

###### telemetry with separate map per video id Poe on effort
reserveffect::map_telemetry_video_id_separate_poe_on(maplatlon_sm, telemetry_poe_on)


############################################ MAKE DENSITY MAPS ON REGULAR GRID POE ON EFFORT #############################################################


#project osm for density mapping
maplatlon_sm_proj = reserveffect::osm_mapproj(maplatlon_sm)

#make study area grid
grid_sm = reserveffect::make_grid(rast_sm)
#grid cell area
#sf::st_area(grid_sm)

#restrict telemetry to dates
dates = c("2021-07-24", "2021-07-29", "2021-08-08", "2021-08-21", '2021-09-05', "2021-09-12",
          "2021-09-14", "2021-10-06", "2021-10-18", "2021-10-23", "2021-10-31", "2021-11-06", "2021-11-10",
          "2021-11-11", "2021-11-14", "2021-11-16", "2021-11-23", "2021-11-24", "2021-11-26", "2021-11-29")
telemetry_poe_on = reserveffect::restrict_telem_dates(telemetry_poe_on, dates)

#convert telemetry points to lines
#lapply does not work in function ****RUN OUTSIDE FUNCTION
list_lines = reserveffect::convert_telemetry_points_to_lines(telemetry_poe_on)
telem = telemetry_poe_on
list_lines = listlines

### Track length

#Sum length of tracks (m) in grid cells per date
grid_tracks_per_date = reserveffect::sum_length_per_grid_per_date(grid_sm, list_lines, dates)

#Total surveyed length
sum(grid_tracks_per_date$length, na.rm=T) #1553071 m

#Mean surveyed length per survey date
sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #77654 m

#Total surveyed area
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) # 137813538 m2

#Mean surveyed area per survey date
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #6890677 m

#Map length of tracks per grid cell per date
reserveffect::map_tracklen_per_grid_per_date_poe(maplatlon_sm_proj, grid_tracks_per_date)

#Map length of tracks per grid cell (all dates)
reserveffect::map_tracklen_per_grid_poe(maplatlon_sm_proj, grid_tracks_per_date)


### Observations

#Count total number of observations per grid cell per date
grid_obs_per_date = reserveffect::count_obs_per_grid_per_date(grid_sm, telemetry_obs_poe_on)

#Map number of species observations per grid cell per date
reserveffect::map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Dugong_certain")
reserveffect::map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Turtle")
reserveffect::map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Shark")
reserveffect::map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Round_ray")
reserveffect::map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Eagle_ray")

#Map number of species observations per grid cell (all dates)
reserveffect::map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Dugong_certain")
reserveffect::map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Turtle")
reserveffect::map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Shark")
reserveffect::map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Round_ray")
reserveffect::map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Eagle_ray")


### Densities

#Map species densities per grid cell (per date)
reserveffect::map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain")
reserveffect::map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle")
reserveffect::map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark")
reserveffect::map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray")
reserveffect::map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray")

#Map species densities per grid cell (all dates) (with mpa overlaid)
reserveffect::map_dens_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", mpa_poe)
reserveffect::map_dens_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", mpa_poe)
reserveffect::map_dens_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", mpa_poe)
reserveffect::map_dens_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", mpa_poe)
reserveffect::map_dens_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", mpa_poe)

#Map species densities per grid cell (all dates) with mpa overlaid and with megafauna image
reserveffect::map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", mpa_poe, img_Dugong_certain)
reserveffect::map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", mpa_poe, img_Turtle)
reserveffect::map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", mpa_poe, img_Shark)
reserveffect::map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", mpa_poe, img_Round_ray)
reserveffect::map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", mpa_poe, img_Eagle_ray)


############################################ MAKE DENSITY MAPS ON CORAL GRID POE ON EFFORT #############################################################


### Observations

#Count total number of observations per coral polygon per date
coral_obs_per_date = reserveffect::count_obs_per_coral_poly_per_date(coral_poly, telemetry_obs_poe_on)

#Map number of species observations per coral polygon (all dates)
#***need to add open sea polygon
reserveffect::map_obs_per_coral_poly_species_poe(maplatlon_sm_proj, coral_obs_per_date, "Dugong_certain")
reserveffect::map_obs_per_coral_poly_species_poe(maplatlon_sm_proj, coral_obs_per_date, "Turtle")
reserveffect::map_obs_per_coral_poly_species_poe(maplatlon_sm_proj, coral_obs_per_date, "Shark")
reserveffect::map_obs_per_coral_poly_species_poe(maplatlon_sm_proj, coral_obs_per_date, "Round_ray")
reserveffect::map_obs_per_coral_poly_species_poe(maplatlon_sm_proj, coral_obs_per_date, "Eagle_ray")


#Map species densities per coral polygon (all dates)
#***need to add open sea polygon
#*** make function


############################################ DENSITY COMPARISONS #############################################################

#Make dataframe per grid cell centers (including empty cell centers) of observations, track length and densities per date and per species for POe
df_all_species = reserveffect::make_df_all_species_poe(grid_obs_per_date, grid_tracks_per_date, footprint_width)

# intersect density dataframe with coral and mpa status
# NB dataframe points are intersected with polygons

df_all_species_coral = reserveffect::intersect_df_coral_poly(df_all_species, coral_poly)
df_all_species_coral_mpa = reserveffect::intersect_df_mpa_poly(df_all_species_coral, mpa_poe)


###barplots

#habitat
reserveffect::barplot_density_coral(df_all_species_coral, "l4_attrib", "Dugong_certain")
reserveffect::barplot_density_coral(df_all_species_coral, "l4_attrib", "Turtle")
reserveffect::barplot_density_coral(df_all_species_coral, "l4_attrib", "Shark")
reserveffect::barplot_density_coral(df_all_species_coral, "l4_attrib", "Round_ray")
reserveffect::barplot_density_coral(df_all_species_coral, "l4_attrib", "Eagle_ray")

#mpa
reserveffect::barplot_density_mpa(df_all_species_coral_mpa, "Dugong_certain")
reserveffect::barplot_density_mpa(df_all_species_coral_mpa, "Turtle")
reserveffect::barplot_density_mpa(df_all_species_coral_mpa, "Shark")
reserveffect::barplot_density_mpa(df_all_species_coral_mpa, "Round_ray")
reserveffect::barplot_density_mpa(df_all_species_coral_mpa, "Eagle_ray")


###barplots per date

#habitat
reserveffect::barplot_density_coral_per_date(df_all_species_coral, "l4_attrib", "Dugong_certain")
reserveffect::barplot_density_coral_per_date(df_all_species_coral, "l4_attrib", "Turtle")
reserveffect::barplot_density_coral_per_date(df_all_species_coral, "l4_attrib", "Shark")
reserveffect::barplot_density_coral_per_date(df_all_species_coral, "l4_attrib", "Round_ray")
reserveffect::barplot_density_coral_per_date(df_all_species_coral, "l4_attrib", "Eagle_ray")

#mpa
reserveffect::barplot_density_mpa_per_date(df_all_species_coral_mpa, "Dugong_certain")
reserveffect::barplot_density_mpa_per_date(df_all_species_coral_mpa, "Turtle")
reserveffect::barplot_density_mpa_per_date(df_all_species_coral_mpa, "Shark")
reserveffect::barplot_density_mpa_per_date(df_all_species_coral_mpa, "Round_ray")
reserveffect::barplot_density_mpa_per_date(df_all_species_coral_mpa, "Eagle_ray")


###compare densities in/out mpa

#Test hyp that median densities in mpa and outside mpa are different
reserveffect::compare_densities_in_out_mpa(df_all_species_coral_mpa, "Dugong_certain")
reserveffect::compare_densities_in_out_mpa(df_all_species_coral_mpa, "Turtle")
reserveffect::compare_densities_in_out_mpa(df_all_species_coral_mpa, "Shark")
reserveffect::compare_densities_in_out_mpa(df_all_species_coral_mpa, "Round_ray")
reserveffect::compare_densities_in_out_mpa(df_all_species_coral_mpa, "Eagle_ray")

#Test hyp that median densitiy in mpa is greater than outside mpa
reserveffect::compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Dugong_certain")
reserveffect::compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Turtle")
reserveffect::compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Shark")
reserveffect::compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Round_ray")
reserveffect::compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Eagle_ray")

###compare densities between habitat types

#Test hyp that median densities between habitat types are different
reserveffect::compare_densities_between_habitats(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib")
reserveffect::compare_densities_between_habitats(df_all_species_coral_mpa, "Turtle", "l4_attrib")
reserveffect::compare_densities_between_habitats(df_all_species_coral_mpa, "Shark", "l4_attrib")
reserveffect::compare_densities_between_habitats(df_all_species_coral_mpa, "Round_ray", "l4_attrib")
reserveffect::compare_densities_between_habitats(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib")

#Test hyp that median densities between habitat types are different pairwise
reserveffect::compare_densities_between_habitats_pairwise(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib")
reserveffect::compare_densities_between_habitats_pairwise(df_all_species_coral_mpa, "Turtle", "l4_attrib")
reserveffect::compare_densities_between_habitats_pairwise(df_all_species_coral_mpa, "Shark", "l4_attrib")
reserveffect::compare_densities_between_habitats_pairwise(df_all_species_coral_mpa, "Round_ray", "l4_attrib")
reserveffect::compare_densities_between_habitats_pairwise(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib")


###compare densities between habitat types and in/out mpa

compare_densities_between_combined_habitats_mpa_status(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib")
compare_densities_between_combined_habitats_mpa_status(df_all_species_coral_mpa, "Turtle", "l4_attrib")
compare_densities_between_combined_habitats_mpa_status(df_all_species_coral_mpa, "Shark", "l4_attrib")
compare_densities_between_combined_habitats_mpa_status(df_all_species_coral_mpa, "Round_ray", "l4_attrib")
compare_densities_between_combined_habitats_mpa_status(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib")

###Boxplots (0s removed)

#mpa
reserveffect::boxplot_density_mpa(df_all_species_coral_mpa, "Dugong_certain")
reserveffect::boxplot_density_mpa(df_all_species_coral_mpa, "Turtle")
reserveffect::boxplot_density_mpa(df_all_species_coral_mpa, "Shark")
reserveffect::boxplot_density_mpa(df_all_species_coral_mpa, "Round_ray")
reserveffect::boxplot_density_mpa(df_all_species_coral_mpa, "Eagle_ray")

#habitat
reserveffect::boxplot_density_coral(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib")
reserveffect::boxplot_density_coral(df_all_species_coral_mpa, "Turtle", "l4_attrib")
reserveffect::boxplot_density_coral(df_all_species_coral_mpa, "Shark", "l4_attrib")
reserveffect::boxplot_density_coral(df_all_species_coral_mpa, "Round_ray", "l4_attrib")
reserveffect::boxplot_density_coral(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib")

#habitat and mpa
reserveffect::boxplot_density_coral_mpa(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib")
reserveffect::boxplot_density_coral_mpa(df_all_species_coral_mpa, "Turtle", "l4_attrib")
reserveffect::boxplot_density_coral_mpa(df_all_species_coral_mpa, "Shark", "l4_attrib")
reserveffect::boxplot_density_coral_mpa(df_all_species_coral_mpa, "Round_ray", "l4_attrib")
reserveffect::boxplot_density_coral_mpa(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib")


### Make two-way anova for density per mpa status and coral habitat and barplot of the result
#cf https://stats.stackexchange.com/questions/41934/non-parametric-alternative-for-2-way-anova
make_anova_barplot_coral_mpa(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib") # violation of normality
make_anova_barplot_coral_mpa(df_all_species_coral_mpa, "Turtle", "l4_attrib") # violation of variance homogeneity and normality
make_anova_barplot_coral_mpa(df_all_species_coral_mpa, "Shark", "l4_attrib")  # violation of normality
make_anova_barplot_coral_mpa(df_all_species_coral_mpa, "Round_ray", "l4_attrib") # violation of variance homogeneity and normality
make_anova_barplot_coral_mpa(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib") # violation of normality


# Make two-way non parametric test (scheirerRayHare test) for density per mpa status and coral habitat and barplot of the result
# https://rcompanion.org/handbook/F_14.html
make_twoway_test_barplot_coral_mpa(df_all_species_coral_mpa, "Dugong_certain", "l4_attrib")
make_twoway_test_barplot_coral_mpa(df_all_species_coral_mpa, "Turtle", "l4_attrib")
make_twoway_test_barplot_coral_mpa(df_all_species_coral_mpa, "Shark", "l4_attrib")
make_twoway_test_barplot_coral_mpa(df_all_species_coral_mpa, "Round_ray", "l4_attrib")
make_twoway_test_barplot_coral_mpa(df_all_species_coral_mpa, "Eagle_ray", "l4_attrib")


##*** test other effects (month? confinement? tide?) on desnities
##*** account for differences in surface of polygons?

##*** need to correct densities
##*** how to deal with unregular survey frequency? (more frequent surveys in november)
