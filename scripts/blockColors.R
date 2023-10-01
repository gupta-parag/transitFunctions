library(data.table)
library(stringr)
library(sf)
library(dplyr)
library(leaflet)
library(lubridate)
library(tigris)

# specify the name of the zip folder
name_of_file <- "miami_transit.zip"

###################################### Route Type definition ################################

route_type_def <- data.frame(route_type = c(0:7, 11:12),
                             Defn = c(c("Tram, Streetcar, Light rail"),
                                      c("Subway, Metro"),
                                      "Rail", "Bus","Ferry", "Cable Tram", "Aerial Lift",
                                      "Funicular", "Trolleybus", "Monorail"))

# 0 - Tram, Streetcar, Light rail. Any light rail or street level system within a metropolitan area.
# 1 - Subway, Metro. Any underground rail system within a metropolitan area.
# 2 - Rail. Used for intercity or long-distance travel.
# 3 - Bus. Used for short- and long-distance bus routes.
# 4 - Ferry. Used for short- and long-distance boat service.
# 5 - Cable tram. Used for street-level rail cars where the cable runs beneath the vehicle, e.g., cable car in San Francisco.
# 6 - Aerial lift, suspended cable car (e.g., gondola lift, aerial tramway). Cable transport where cabins, cars, gondolas or open chairs are suspended by means of one or more cables.
# 7 - Funicular. Any rail system designed for steep inclines.
# 11 - Trolleybus. Electric buses that draw power from overhead wires using poles.
# 12 - Monorail. Railway in which the track consists of a single rail or a beam

####################################################################################################





################################# UNZIPPIG THE FILES ################################
zip_feed <- str_c("C:\\Users\\gupta\\Dropbox\\transit\\GTFS_feed\\zip\\", name_of_file)
unzip_feed <- str_c("C:\\Users\\gupta\\Dropbox\\transit\\GTFS_feed\\unzip\\", 
                    gsub(name_of_file, pattern = ".zip$", replacement = ""))
 unzip_feed <- "/Users/parag.geminigmail.com/Dropbox/transit/GTFS_feed/unzip/miami_transit"
# 
# #unziping the file to the directory
# unzip(zipfile = zip_feed, exdir = unzip_feed)
#####################################################################################

##################### Reading GTFS feed files ################################
feed_path <- list.files(unzip_feed, full.names = T)
feed_file_names <- list.files(unzip_feed)
 
gtfs_feed <- lapply(feed_path, fread)
names(gtfs_feed) <- gsub(feed_file_names, pattern = ".txt$", replacement = "")
######################################################################################

####################### Getting the files in simple data frame ####################################
trips <- gtfs_feed[['trips']]
trips_f <- trips[,c("route_id","service_id",
                    "trip_id", "shape_id")] 

calendar <- gtfs_feed[['calendar']]
cal_f <- calendar[ , c(1:8)] # getting serice ID and all 7 days in week - Monday to Sunday

routes <- gtfs_feed[['routes']]
routes_f <- routes[,c("route_id","route_short_name",
                      "route_type","route_color")]

stops <- gtfs_feed[['stops']]
stops_f <- stops[,c("stop_id","stop_code","stop_lat","stop_lon" )]

stopTimes <- gtfs_feed[['stop_times']]
stopTimes_f <- stopTimes[ , c("trip_id", "arrival_time", "departure_time",
                              "stop_id", "stop_sequence", "shape_dist_traveled")]

shapes <- gtfs_feed[['shapes']]

##########################################################################################

############################## Stops frequency ###################

#getting stop times, with tripID, routeID and calendar
stopsTripsCal <- left_join(stopTimes_f, 
                           trips_f[,c(1:3)], 
                           by = "trip_id") %>%
  left_join(cal_f,by = "service_id")

monday <- stopsTripsCal[stopsTripsCal$monday == 1, ]
monday$aTime <- hms(monday$arrival_time)
monday$aHour <- hour(monday$aTime)

stopFreq <- monday %>% group_by(stop_id) %>%
  summarise(total_trips = n()) %>%
  left_join(stops_f, by = "stop_id")


leaflet() %>% addTiles() %>%
  setView(zoom = 13 , lat = 25.79002 , lng = -80.18895) %>%
  addCircleMarkers(data = stopFreq, lat = stopFreq$stop_lat,
                   lng = stopFreq$stop_lon, weight = 2,
                   label = stopFreq$stop_code,
                   color = "red",
                   radius = sqrt(stopFreq$total_trips))

# add time period, frequency (labels) and color by frequency, and labels
# add filter to select days, weekend and time period
# to visualization for routes


# visualizing blocks / block groups
juris_blocks <- blocks(state = "12", county = "086", year = 2021)
juris_bg <- st_transform(block_groups(state = "12", county = "086", year = 2021),4326)


####### creating stops shapefiles ##########

stops_geom <- lapply(1:nrow(stops), sum)

for ( i in 1:nrow(stops)){
  stops_geom[[i]] <- st_point(c(stops$stop_lon[i], stops$stop_lat[i]))
}

stops$geometry <- st_sfc(stops_geom)
stops_sf <- st_sf(stops, crs = 4326, sf_column_name = "geometry")
##############################################

x <- st_join( stops_sf, juris_bg,join = st_within)
x_f <- x[, c(1,2,3,17)]

z <- stops[stops$stop_id == 31,]

leaflet() %>% addTiles() %>%
  setView(zoom = 13 , lat = 25.79002 , lng = -80.18895) %>%
  addPolygons(data = st_transform(juris_bg,4326),
              weight = 2) %>%
  addCircleMarkers(data = z, lat = z$stop_lat, lng = z$stop_lon)









