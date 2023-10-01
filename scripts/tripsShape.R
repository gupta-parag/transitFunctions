library(data.table)
library(stringr)
library(sf)
library(dplyr)
library(leaflet)




return_stops <- function(name_of_file ="nyc.zip", route_type = 'all' ){
  
  # name_of_file ="nyc.zip"
  # route_type = 'all'
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
zip_feed <- str_c("E:\\Z\\transit_Functions\\GTFS_feed\\zip\\", name_of_file)
unzip_feed <- str_c("E:\\Z\\transit_Functions\\GTFS_feed\\unzip\\", 
                    gsub(name_of_file, pattern = ".zip$", replacement = ""))


#unziping the file to the directory
message('Unzipping files in Stops Files')


unzip(zipfile = zip_feed, exdir = unzip_feed)
message('Finished Unzipping files in Stops Files')
#####################################################################################

##################### Reading GTFS feed files ################################
feed_path <- list.files(unzip_feed, full.names = T)
feed_file_names <- list.files(unzip_feed)

gtfs_feed <- lapply(feed_path, fread)
names(gtfs_feed) <- gsub(feed_file_names, pattern = ".txt$", replacement = "")

message('Read Files from Stops Files')
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
stops_f <- stops[,c("stop_id","stop_name","stop_lat","stop_lon" )]

stopTimes <- gtfs_feed[['stop_times']]
stopTimes_f <- stopTimes[ , c("trip_id", "arrival_time", "departure_time",
                              "stop_id", "stop_sequence")]

shapes <- gtfs_feed[['shapes']]

message('Got files in R')
##########################################################################################


###################### Trying to get shapes data by Maximum number of Stops ##########################
tripsStops <- left_join(trips,stopTimes_f, by = "trip_id")

stopsByRoutes <- tripsStops %>% group_by(trip_id) %>%
  summarise(total_stops = n()) %>%
  arrange(desc(total_stops)) %>%
  left_join(trips_f[,c(1,3,4)] , by = "trip_id") %>%
  left_join(routes_f, by = "route_id") %>%
  group_by(route_id) %>%
  slice(which.max(total_stops)) %>%
  select(1,2,3,5,6,7)%>%
  left_join(stopTimes_f[,c(1,4)], by="trip_id")%>%
  left_join(stops[,c(1:6)], by = "stop_id")

##############################################################################################


########################## CHECK FOR ROUTE TYPE #########################

if(route_type != "all") {
  stopsByRoutes <- stopsByRoutes[ stopsByRoutes$route_type %in% route_type , ]
}

##########################################################################

##################################### Creating the Map ###################################
stopLabels <- sprintf(
  "<strong>Stop ID :</strong> %s </br>
    <strong>Stop Name :</strong> %s </br>",
        stopsByRoutes$stop_id, 
  ifelse(stopsByRoutes$stop_name != "" | 
           is.na(stopsByRoutes$stop_name), 
         stopsByRoutes$stop_name,"No Stop Name")) %>% 
  lapply(htmltools::HTML)


map_grey <-  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data = stopsByRoutes, lat = stopsByRoutes$stop_lat,
                   radius = 3, color = "red", 
                   label = stopLabels,
                   lng = stopsByRoutes$stop_lon)
map_black <-leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addCircleMarkers(data = stopsByRoutes, lat = stopsByRoutes$stop_lat,
                   radius = 3, color = "red", 
                   label = stopLabels,
                   popup = stopLabels,
                   lng = stopsByRoutes$stop_lon)

message('Sending Output of Stops')
 ####################################################################################

return(list(mapGrey = map_grey, mapBlack = map_black, 
            stopsGeo = stopsByRoutes, labels = stopLabels))

}




  