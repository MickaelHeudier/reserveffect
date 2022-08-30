
#load all functions
devtools::load_all()




######################################## DEFINE STUDY AREA ########################################

#Define study area coordinates
#small region
lat1_sm =  -21.52 ; lat2_sm = -21.65
lon1_sm = 165.225 ; lon2_sm = 165.45
#big region
lat1_bg =  -21.2 ; lat2_bg = -22
lon1_bg = 164.6 ; lon2_bg = 166
#australia + nc
lat1_nc = -42 ; lat2_nc = -8.5
lon1_nc = 115 ; lon2_nc = 170




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
telemetry_all = read_telem()

#clean telemetry data
telemetry = clean_telem(telemetry_all, lat1_bg, lon1_bg, lat2_bg, lon2_bg)

#Read video information from google sheets (informations_video_ulm_news)
videos = read_video_info()

#Clean video information: select only DATE, VIDEO_ID, EFFORT_POE
videos = clean_video_info(videos)

#Join video information to telemetry
telemetry = join_video_info_telem(videos, telemetry)

#Clean duplicated obs from telemetry
#telemetry_obs = clean_duplicated_obs_telemetry(telemetry, overlap, image_height) (this code takes about 20 minutes to process)
#alternatively read the Rdata to avoid long processing
load("telemetry_obs.RData") 

#Get list of Poe videos
ls_poe_videos = list_poe_videos(videos)

#select video information for Poe
videos_poe = select_poe_videos(videos, ls_poe_videos)

#Join video information to telemetry
telemetry_poe = join_video_poe_info_telem(videos_poe, telemetry)

#select obs for Poe
telemetry_obs_poe = select_obs_telemetry_poe(telemetry_obs, ls_poe_videos)

#count total number of individuals observed per species
telemetry_obs_poe %>%
  dplyr::group_by(object) %>%
  dplyr::summarise(n_tot = dplyr::n())

#read images png
#black
img_Dugong_certain = read_megafauna_image("Dugong_certain")
img_Turtle = read_megafauna_image("Turtle")
img_Shark = read_megafauna_image("Shark")
img_Round_ray = read_megafauna_image("Round_ray")
img_Eagle_ray = read_megafauna_image("Eagle_ray")

#grey
img_Dugong_certain_grey = read_megafauna_image_grey("Dugong_certain")
img_Turtle_grey = read_megafauna_image_grey("Turtle")
img_Shark_grey = read_megafauna_image_grey("Shark")
img_Round_ray_grey = read_megafauna_image_grey("Round_ray")
img_Eagle_ray_grey = read_megafauna_image_grey("Eagle_ray")







######################################## READ CORAL DATA ########################################

#read Allen coral benthic polygon
allen_coral_poly = read_crop_and_convert_allen_coralnc_benthic(lon1_sm, lon2_sm, lat2_sm, lat1_sm)

# make study area raster (resolution 0.0025 degrees)
rast_sm = make_area_raster(lat1_sm, lon1_sm, lat2_sm, lon2_sm, 0.0025)






######################################## READ MPA DATA ########################################

# read mpa polygon
mpa = read_mpanc()

# extract mpa polygon for poe
mpa_poe = extract_mpa_poe(mpa)





######################################## READ TRANSECT DATA ########################################

points1 = read_transects_points("megafauna1_points_latlon")

library(sp)
lines1 = make_transect_lines(points1, "1")
#lapply does not work in function make_transect_lines() SO NEED TO RUN OUTSIDE FUNCTION with following parameters
pts = points1
sector = "1" #for Poé sector
#rename function output
lines1 = l

#OSM map
maplatlon_sm = osm_map(lat1_sm, lon1_sm, lat2_sm, lon2_sm) #bing (=satellite view)

#map transects: Figure 1
map_transects_scalebar_mpa(maplatlon_sm, lines1, mpa_poe, "small", lat1_sm, lon1_sm, lat2_sm, lon2_sm,
                            dist = 2, offset_lon = 0.07, offset_lat = 0.01)








############################################ MAKE BASIC MAPS #############################################################

##### Map New Caledonia with Poé location: Figure 1
map_new_caledonia()

###### Map Allen coral polygons benthic with satellite view
map_allen_coral_poly_satellite(maplatlon_sm, mpa_poe, allen_coral_poly, "small", lon1_sm, lon2_sm, lat1_sm, lat2_sm, dist = 2, offset_lon = 0.07, offset_lat = 0.01)

###### Make map of Allen coral geomorphology polygons with added open sea category: Figure 3A
map_allen_coral_poly_coastline_with_open_sea(maplatlon_sm, mpa_poe, allen_coral_poly, "small", lon1_sm, lon2_sm, lat1_sm, lat2_sm)







###################################### SELECT ON EFFORT TELEMETRY FOR POE #####################################

#read off effort portions Poe
off_effort = read_off_effort_poe()

#clean effort portions related to transit Poe
off_effort_transit = clean_off_effort_transit_poe(off_effort)

#clean effort portions related to loop Poe
off_effort_loop = clean_off_effort_loop_poe(off_effort)

#select on effort telemetry and observations Poe
telemetry_poe_on = select_on_effort_poe(telemetry_poe, off_effort_transit, off_effort_loop)
telemetry_obs_poe_on = select_on_effort_poe(telemetry_obs_poe, off_effort_transit, off_effort_loop)

#Convert telemetry (all) to spatialpoints dataframe
telemetry_poe_sp = convert_telemetry_spatial(telemetry_poe)

#Convert telemetry (on effort) to spatialpoints dataframe
telemetry_poe_on_sp = convert_telemetry_spatial(telemetry_poe_on)


#### Redo maps for Poe on effort

###### telemetry with all species Poe on effort
map_all_species_telemetry_poe_on(maplatlon_sm, telemetry_poe_on, telemetry_obs_poe_on)

###### telemetry with individual species Poe on effort
map_indiv_species_telemetry_poe_on(maplatlon_sm, telemetry_poe_on, telemetry_obs_poe_on)

###### telemetry with individual species with separate map per species Poe on effort
map_indiv_species_telemetry_separate_poe_on(maplatlon_sm, telemetry_poe_on, telemetry_obs_poe_on)

###### telemetry with one color per date Poe on effort
map_telemetry_date_poe_on(maplatlon_sm, telemetry_poe_on)

###### telemetry with separate map per date Poe on effort
map_telemetry_date_separate_poe_on(maplatlon_sm, telemetry_poe_on)

###### telemetry with separate map per video id Poe on effort
map_telemetry_video_id_separate_poe_on(maplatlon_sm, telemetry_poe_on)






############################################ MAKE DENSITY MAPS ON REGULAR GRID POE ON EFFORT #############################################################


#project osm for density mapping
maplatlon_sm_proj = osm_mapproj(maplatlon_sm)

#make study area grid
grid_sm = make_grid(rast_sm)

#restrict telemetry to dates of surveys in Poé
dates = c("2021-07-24", "2021-07-29", "2021-08-08", "2021-08-21", '2021-09-05', "2021-09-12",
          "2021-09-14", "2021-10-06", "2021-10-18", "2021-10-23", "2021-10-31", "2021-11-06", "2021-11-10",
          "2021-11-11", "2021-11-14", "2021-11-16", "2021-11-23", "2021-11-24", "2021-11-26", "2021-11-29")
telemetry_poe_on = restrict_telem_dates(telemetry_poe_on, dates)

#convert telemetry points to lines
list_lines = convert_telemetry_points_to_lines(telemetry_poe_on)
#lapply does not work in function convert_telemetry_points_to_lines() SO NEED TO RUN OUTSIDE FUNCTION with following parameter
telem = telemetry_poe_on
#rename function output
list_lines = listlines



### Track length

#Sum length of tracks (m) in grid cells per date
grid_tracks_per_date = sum_length_per_grid_per_date(grid_sm, list_lines, dates)

#Total surveyed length
sum(grid_tracks_per_date$length, na.rm=T) #1553071 m

#Mean surveyed length per survey date
sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #77654 m

#Total surveyed area
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) # 137813538 m2

#Mean surveyed area per survey date
footprint_width * sum(grid_tracks_per_date$length, na.rm=T) / length(dates) #6890677 m

#Map length of tracks per grid cell per date
map_tracklen_per_grid_per_date_poe(maplatlon_sm_proj, grid_tracks_per_date)

#Map length of tracks per grid cell (all dates)
map_tracklen_per_grid_poe(maplatlon_sm_proj, grid_tracks_per_date)



### Observations

#Count total number of observations per grid cell per date
grid_obs_per_date = count_obs_per_grid_per_date(grid_sm, telemetry_obs_poe_on)

#Map number of species observations per grid cell per date
map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Dugong_certain")
map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Turtle")
map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Shark")
map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Round_ray")
map_obs_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Eagle_ray")

#Map number of species observations per grid cell (all dates)
map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Dugong_certain")
map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Turtle")
map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Shark")
map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Round_ray")
map_obs_per_grid_species_poe(maplatlon_sm_proj, grid_obs_per_date, "Eagle_ray")



### Densities

#Map species densities per grid cell (per date)
map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain")
map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle")
map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark")
map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray")
map_dens_per_grid_per_date_species_poe(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray")

#Map species densities per grid cell (all dates) with mpa overlaid and with megafauna image
map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", mpa_poe, img_Dugong_certain)
map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", mpa_poe, img_Turtle)
map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", mpa_poe, img_Shark)
map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", mpa_poe, img_Round_ray)
map_dens_per_grid_species_poe_with_megafauna_image(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", mpa_poe, img_Eagle_ray)

#Map species densities per grid cell (all dates) with mpa overlaid and with megafauna image LOG with zero: Figure 4 A-E
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", mpa_poe, img_Dugong_certain_grey)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", mpa_poe, img_Turtle_grey)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", mpa_poe, img_Shark_grey)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", mpa_poe, img_Round_ray_grey)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero(maplatlon_sm_proj, grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", mpa_poe, img_Eagle_ray_grey)

#Map species densities per grid cell (all dates) with mpa overlaid and with megafauna image LOG with zero
#with coastline instead of OSM
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero_coastline(grid_obs_per_date, grid_tracks_per_date, footprint_width, "Dugong_certain", mpa_poe, img_Dugong_certain)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero_coastline(grid_obs_per_date, grid_tracks_per_date, footprint_width, "Turtle", mpa_poe, img_Turtle)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero_coastline(grid_obs_per_date, grid_tracks_per_date, footprint_width, "Shark", mpa_poe, img_Shark)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero_coastline(grid_obs_per_date, grid_tracks_per_date, footprint_width, "Round_ray", mpa_poe, img_Round_ray)
map_dens_per_grid_species_poe_with_megafauna_image_log_with_zero_coastline(grid_obs_per_date, grid_tracks_per_date, footprint_width, "Eagle_ray", mpa_poe, img_Eagle_ray)

#Map nbr survey with sightings per grid with image: Appendix 3
map_nbr_survey_with_sightings_per_grid_image(maplatlon_sm_proj, grid_tracks_per_date, grid_obs_per_date, "Dugong_certain", mpa_poe, img_Dugong_certain_grey)
map_nbr_survey_with_sightings_per_grid_image(maplatlon_sm_proj, grid_tracks_per_date, grid_obs_per_date, "Turtle", mpa_poe, img_Turtle_grey)
map_nbr_survey_with_sightings_per_grid_image(maplatlon_sm_proj, grid_tracks_per_date, grid_obs_per_date, "Shark", mpa_poe, img_Shark_grey)
map_nbr_survey_with_sightings_per_grid_image(maplatlon_sm_proj, grid_tracks_per_date, grid_obs_per_date, "Round_ray", mpa_poe, img_Round_ray_grey)
map_nbr_survey_with_sightings_per_grid_image(maplatlon_sm_proj, grid_tracks_per_date, grid_obs_per_date, "Eagle_ray", mpa_poe, img_Eagle_ray_grey)

#Map nbr survey with sightings per grid with image
#with coastline instead of OSM 
map_nbr_survey_with_sightings_per_grid_image_coastline(grid_tracks_per_date, grid_obs_per_date, "Dugong_certain", mpa_poe, img_Dugong_certain)
map_nbr_survey_with_sightings_per_grid_image_coastline(grid_tracks_per_date, grid_obs_per_date, "Turtle", mpa_poe, img_Turtle)
map_nbr_survey_with_sightings_per_grid_image_coastline(grid_tracks_per_date, grid_obs_per_date, "Shark", mpa_poe, img_Shark)
map_nbr_survey_with_sightings_per_grid_image_coastline(grid_tracks_per_date, grid_obs_per_date, "Round_ray", mpa_poe, img_Round_ray)
map_nbr_survey_with_sightings_per_grid_image_coastline(grid_tracks_per_date, grid_obs_per_date, "Eagle_ray", mpa_poe, img_Eagle_ray)





############################################ DENSITY COMPARISONS #############################################################

#Make dataframe per grid cell centers (including empty cell centers) of observations, track length and densities per date and per species for POe
df_all_species = make_df_all_species_poe(grid_obs_per_date, grid_tracks_per_date, footprint_width)

#intersect density dataframe with coral and mpa status
#NB dataframe points are intersected with polygons
df_all_species_coral = intersect_df_allen_coral_poly(df_all_species, allen_coral_poly)
df_all_species_coral_mpa = intersect_df_mpa_allen_poly(df_all_species_coral, mpa_poe)

#effort per habitat class
df_all_species_coral %>% 
  dplyr::select(length, class) %>% 
  dplyr::group_by(class) %>% 
  dplyr::summarise(sum = sum(length)) -> dat
dat$percent <- 100*(dat$sum / sum(dat$sum))

#effort in/out mpa
df_all_species_coral_mpa %>% 
  dplyr::select(length, mpa_status) %>% 
  dplyr::group_by(mpa_status) %>% 
  dplyr::summarise(sum = sum(length)) -> dat
dat$percent <- 100*(dat$sum / sum(dat$sum))



###barplots

#Allen coral habitat
barplot_density_allen_coral(df_all_species_coral, "Dugong_certain")
barplot_density_allen_coral(df_all_species_coral, "Turtle")
barplot_density_allen_coral(df_all_species_coral, "Shark")
barplot_density_allen_coral(df_all_species_coral, "Round_ray")
barplot_density_allen_coral(df_all_species_coral, "Eagle_ray")


#Allen coral habitat with image
barplot_density_allen_coral_image(df_all_species_coral, "Dugong_certain", img_Dugong_certain)
barplot_density_allen_coral_image(df_all_species_coral, "Turtle", img_Turtle)
barplot_density_allen_coral_image(df_all_species_coral, "Shark", img_Shark)
barplot_density_allen_coral_image(df_all_species_coral, "Round_ray", img_Round_ray)
barplot_density_allen_coral_image(df_all_species_coral, "Eagle_ray", img_Eagle_ray)


#mpa
barplot_density_mpa(df_all_species_coral_mpa, "Dugong_certain")
barplot_density_mpa(df_all_species_coral_mpa, "Turtle")
barplot_density_mpa(df_all_species_coral_mpa, "Shark")
barplot_density_mpa(df_all_species_coral_mpa, "Round_ray")
barplot_density_mpa(df_all_species_coral_mpa, "Eagle_ray")


#mpa with image
barplot_density_mpa_image(df_all_species_coral_mpa, "Dugong_certain", img_Dugong_certain)
barplot_density_mpa_image(df_all_species_coral_mpa, "Turtle", img_Turtle)
barplot_density_mpa_image(df_all_species_coral_mpa, "Shark", img_Shark)
barplot_density_mpa_image(df_all_species_coral_mpa, "Round_ray", img_Round_ray)
barplot_density_mpa_image(df_all_species_coral_mpa, "Eagle_ray", img_Eagle_ray)


#Allen coral habitat and mpa
barplot_density_allen_coral_mpa(df_all_species_coral_mpa, "Dugong_certain")
barplot_density_allen_coral_mpa(df_all_species_coral_mpa, "Turtle")
barplot_density_allen_coral_mpa(df_all_species_coral_mpa, "Shark")
barplot_density_allen_coral_mpa(df_all_species_coral_mpa, "Round_ray")
barplot_density_allen_coral_mpa(df_all_species_coral_mpa, "Eagle_ray")


#Allen coral habitat and mpa with image
barplot_density_allen_coral_mpa_image(df_all_species_coral_mpa, "Dugong_certain", img_Dugong_certain)
barplot_density_allen_coral_mpa_image(df_all_species_coral_mpa, "Turtle", img_Turtle)
barplot_density_allen_coral_mpa_image(df_all_species_coral_mpa, "Shark", img_Shark)
barplot_density_allen_coral_mpa_image(df_all_species_coral_mpa, "Round_ray", img_Round_ray)
barplot_density_allen_coral_mpa_image(df_all_species_coral_mpa, "Eagle_ray", img_Eagle_ray)




###compare densities in/out mpa

#Test hyp that median densities in mpa and outside mpa are different
compare_densities_in_out_mpa(df_all_species_coral_mpa, "Dugong_certain")
compare_densities_in_out_mpa(df_all_species_coral_mpa, "Turtle")
compare_densities_in_out_mpa(df_all_species_coral_mpa, "Shark")
compare_densities_in_out_mpa(df_all_species_coral_mpa, "Round_ray")
compare_densities_in_out_mpa(df_all_species_coral_mpa, "Eagle_ray")


#Test hyp that median densitiy in mpa is greater than outside mpa
compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Dugong_certain")
compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Turtle")
compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Shark")
compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Round_ray")
compare_densities_in_out_mpa_greater(df_all_species_coral_mpa, "Eagle_ray")



###compare densities between habitat types

#Test hyp that median densities between coral habitat types are different
compare_densities_between_allen_habitats(df_all_species_coral_mpa, "Dugong_certain")
compare_densities_between_allen_habitats(df_all_species_coral_mpa, "Turtle")
compare_densities_between_allen_habitats(df_all_species_coral_mpa, "Shark")
compare_densities_between_allen_habitats(df_all_species_coral_mpa, "Round_ray")
compare_densities_between_allen_habitats(df_all_species_coral_mpa, "Eagle_ray")

#Test hyp that median densities between habitat types are different pairwise
compare_densities_between_allen_habitats_pairwise(df_all_species_coral_mpa, "Dugong_certain")
compare_densities_between_allen_habitats_pairwise(df_all_species_coral_mpa, "Turtle")
compare_densities_between_allen_habitats_pairwise(df_all_species_coral_mpa, "Shark")
compare_densities_between_allen_habitats_pairwise(df_all_species_coral_mpa, "Round_ray")
compare_densities_between_allen_habitats_pairwise(df_all_species_coral_mpa, "Eagle_ray")



###Boxplots (0s removed)

#habitat and mpa with data Allen and megafauna image LOG
boxplot_density_allen_coral_mpa_with_megafauna_image_log(df_all_species_coral_mpa, "Dugong_certain", img_Dugong_certain)
boxplot_density_allen_coral_mpa_with_megafauna_image_log(df_all_species_coral_mpa, "Turtle", img_Turtle)
boxplot_density_allen_coral_mpa_with_megafauna_image_log(df_all_species_coral_mpa, "Shark", img_Shark)
boxplot_density_allen_coral_mpa_with_megafauna_image_log(df_all_species_coral_mpa, "Round_ray", img_Round_ray)
boxplot_density_allen_coral_mpa_with_megafauna_image_log(df_all_species_coral_mpa, "Eagle_ray", img_Eagle_ray)





### Habitat composition

#barplot habitat proportion in and out of mpa: Figure 3 B
barplot_habitat_proportion_in_out_mpa(df_all_species_coral_mpa)

#barplot habitat proportion in whole sampled area
barplot_habitat_proportion(df_all_species_coral_mpa)


### permanova

# Make permanova for density per mpa status and coral habitat
perm_dug = make_permanova_allen_coral_mpa(df_all_species_coral_mpa, "Dugong_certain")
perm_tur = make_permanova_allen_coral_mpa(df_all_species_coral_mpa, "Turtle")
perm_sha = make_permanova_allen_coral_mpa(df_all_species_coral_mpa, "Shark")
perm_rou = make_permanova_allen_coral_mpa(df_all_species_coral_mpa, "Round_ray")
perm_eag = make_permanova_allen_coral_mpa(df_all_species_coral_mpa, "Eagle_ray")



# Barplot of Permanova habitat + mpa: Figure 4F
make_permanova_barplot_allen_coral_mpa_with_all_species(df_all_species_coral_mpa, perm_dug, perm_tur, perm_sha, perm_rou, perm_eag)



### pairwise comparisons for species with signif effects ***add bonferroni corr?

## Dugong interaction
comp_dug = make_pairwise_comparison_allen_coral_mpa(df_all_species_coral_mpa, "Dugong_certain")

## Dasyatidae habitat
comp_rou1 = make_pairwise_comparison_allen_coral(df_all_species_coral_mpa, "Round_ray")

## Dasyatidae interaction
comp_rou2 = make_pairwise_comparison_allen_coral_mpa(df_all_species_coral_mpa, "Round_ray")



### barplots density with pairwise comparision test results

#barplot_density_allen_coral_pairwise_image(df_all_species_coral_mpa, "Dugong_certain", img_Dugong_certain)
