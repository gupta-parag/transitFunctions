library(data.table)
library(stringr)
library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)


source("E:\\Z\\transit_Functions\\scripts\\stopsShapes.R")

#parent function

return_shapefile <- function(route_type="all",name_of_file = "sanFran.zip"){

  #create random colors
return_colors <- function(){
  hexNums <- as.character(c(1:9,0))
  hexLetters <- letters[1:6]
  hexChars <- c(hexNums,hexLetters)
  finalColor <- NULL
  
  for( i in 1:6){
    randomIndex <- round( runif(1,1,length(hexChars)),0)
    finalColor <- c(finalColor, hexChars[randomIndex])
  }
  
  return(paste(finalColor,collapse=''))
}

###################################### Route Type definition ################################

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

route_type_def <- data.frame(route_type = c(0:7, 11:12),
                             Defn = c(c("Tram, Streetcar, Light rail"),
                                      c("Subway, Metro"),
                                      "Rail", "Bus","Ferry", "Cable Tram", "Aerial Lift",
                                      "Funicular", "Trolleybus", "Monorail"))

####################################################################################################


################################# UNZIPPIG THE FILES ################################
zip_feed <- str_c("E:\\Z\\transit_Functions\\GTFS_feed\\zip\\", name_of_file)
unzip_feed <- str_c("E:\\Z\\transit_Functions\\GTFS_feed\\unzip\\", 
                    gsub(name_of_file, pattern = ".zip$", replacement = ""))
 
#unziping the file to the directory
message('Unzipping Files')
unzip(zipfile = zip_feed, exdir = unzip_feed)
#####################################################################################

##################### Reading GTFS feed files ################################
feed_path <- list.files(unzip_feed, full.names = T,pattern = ".txt$")
feed_file_names <- list.files(unzip_feed,pattern = ".txt$")
 
gtfs_feed <- lapply(feed_path, fread)
names(gtfs_feed) <- gsub(feed_file_names, pattern = ".txt$", replacement = "")
######################################################################################

####################### Getting the files in simple data frame ####################################
trips <- gtfs_feed[['trips']]
trips <- trips[!(trips$shape_id==''| is.na(trips$shape_id)) ,]
trips$route_id <-as.character(trips$route_id)
trips_f <- trips[,c("route_id","service_id",
                    "trip_id", "shape_id")] 

calendar <- gtfs_feed[['calendar']]
cal_f <- calendar[ , c(1:8)] # getting serice ID and all 7 days in week - Monday to Sunday

routes <- gtfs_feed[['routes']]
routes$route_id <- as.character(routes$route_id)
routes_f <- routes[,c("route_id","route_short_name",
                      "route_type","route_color")]


stops <- gtfs_feed[['stops']]
stops_f <- stops[,c("stop_id","stop_lat","stop_lon" )]

stopTimes <- gtfs_feed[['stop_times']]
stopTimes_f <- stopTimes[ , c("trip_id", "arrival_time", "departure_time",
                              "stop_id", "stop_sequence")]

shapes <- gtfs_feed[['shapes']]
message('Finished Reading GTFS feed')
##########################################################################################


###################### Trying to get shapes data by Maximum number of Stops ##########################
tripsStops <- left_join(trips,stopTimes_f, by = "trip_id")

xtripsByStops <- tripsStops %>% group_by(trip_id) %>%
  summarise(total_stops = n()) %>%
    arrange(desc(total_stops))%>%
  left_join(trips_f[,c(1,3,4)] , by = "trip_id") %>%
  left_join(routes_f, by = "route_id")%>%
 group_by(route_id) %>%
  slice(which.max(total_stops)) %>%
  left_join(shapes, by = "shape_id")%>%
  select(3,4,5,7,8,9,10)

routeByShapeId <- split(xtripsByStops, xtripsByStops$route_id)

message('Splited shapes by Routes')
#######################################################################################


############## Creating shapes of all routes ################################

routeParentSf <- lapply(1:length(routeByShapeId), sum)
for ( i in 1:length(routeByShapeId)){
  print(i)
  z <- routeByShapeId[[i]]
  route_shape <- st_linestring(as.matrix(z[, c("shape_pt_lon", "shape_pt_lat")]))
  routeParentSf[[i]] <- route_shape
}

#names(by_route_parent) <- NULL
(route_sfc <- st_sfc(routeParentSf)) #paranthese are important to convert the list object into sfc 
st_crs(route_sfc) <- '+proj=longlat +datum=WGS84'


route_sf<- st_sf(route_id = names(routeByShapeId), route_sfc)
routes_f$route_id <- as.character(routes_f$route_id)

routes_sf <- left_join(route_sf, 
                       routes_f, 
                       by = "route_id") %>%
              left_join(route_type_def, by  = "route_type") %>%
  left_join(routes[,c("route_id","route_long_name")], by = "route_id")

message('Finished Creating the shapefile')
###########################################################################

################### Taking care of routes that do not have any color in them########################
if(sum(routes_sf$route_color == '') | sum(is.na(routes_sf$route_color))){
  generate_color <- routes_sf[routes_sf$route_color == '' | is.na(routes_sf$route_color) ,]
  for( i in 1:nrow(generate_color)){
    generate_color$route_color[i]  <- return_colors()
  }
  if(nrow(routes_sf) == sum(is.na(routes_sf$route_color)) | 
     nrow(routes_sf) == sum(routes_sf$route_color == ''))
    {
    routes_sf <- NULL
    routes_sf <- generate_color
  } else {
    routes_sf <- routes_sf[!(routes_sf$route_color == ''),]
    routes_sf <- routes_sf[!(is.na(routes_sf$route_color)),]
    routes_sf <- rbind(routes_sf, generate_color)
  }
 
} else {
  print("Route Colors are there")
}
routes_sf$color_r <- paste0("#", routes_sf$route_color)
message('Added Route colors')
#################################################################

########################## CHECK FOR ROUTE TYPE #########################

if(route_type != "all") {
  routes_sf <- routes_sf[ routes_sf$route_type %in% route_type , ]
}

##########################################################################

###################### CREATE FINAL MAP #############################
map_grey <-leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(minZoom = 10, maxZoom = 16))
map_black <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter,
                   options = providerTileOptions(minZoom = 10, maxZoom = 16))

for( i in 1:nrow(routes_sf)){
  
  print(i)
  
  routeLabels <- sprintf(
    "<strong>Route ID:</strong> %s </br>
    <strong>Route Type:</strong> %s </br>
    <strong>Route Short Name:</strong> %s </br>
    <strong>Route Long Name:</strong> %s </br>",
    routes_sf$route_id[i], routes_sf$Defn[i],
    ifelse(routes_sf$route_short_name[i] != "" | 
             is.na(routes_sf$route_short_name[i]), 
            routes_sf$route_short_name[i],"No short name"),
    ifelse(routes_sf$route_long_name[i] != "" | 
             is.na(routes_sf$route_long_name[i]), 
           routes_sf$route_long_name[i],"No long name")) %>% 
    lapply(htmltools::HTML)
  
  map_grey <- addPolylines(map = map_grey, data= routes_sf[i,],
                          group = routes_sf$route_id[i], 
                          color = routes_sf$color_r[i],
                          label = routeLabels,
                          weight = 3,
                          opacity = 1, 
                          highlightOptions = highlightOptions(bringToFront = TRUE, weight = 7 ))
  map_black <- addPolylines(map = map_black, data= routes_sf[i,],
                          group = routes_sf$route_id[i], 
                          color = routes_sf$color_r[i],
                          label = routeLabels,
                          weight = 3,
                          opacity = 1, 
                          highlightOptions = highlightOptions(bringToFront = TRUE, weight = 7 ))
            
                      
}

############### Adding Stops ################################################
message('Adding Stops')

y <- return_stops(route_type = route_type, name_of_file = name_of_file)

message('Finished getting stops')

stopsByRoutes <- y[["stopsGeo"]]
stopLabels <- y[["labels"]]
#######################################################
  
final_map_grey <-  map_grey %>% 
  addCircleMarkers(data = stopsByRoutes, lat = stopsByRoutes$stop_lat,
                   label = stopLabels,
                   lng = stopsByRoutes$stop_lon,  
                   radius = 2,
                   color = "black",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0,
                   group = "Stops") %>%
   addLayersControl(overlayGroups = c(routes_sf$route_id,"Stops"),
                options = layersControlOptions(collapsed = TRUE))
final_map_black <-  map_black %>% 
         addCircleMarkers(data = stopsByRoutes, lat = stopsByRoutes$stop_lat,
                   label = stopLabels,
                   lng = stopsByRoutes$stop_lon,  
                   radius =10,
                   color = "black",
                   fillColor = 'blue',
                   stroke = TRUE, fillOpacity = 0,
                   group = "Stops") %>%
          addLayersControl(overlayGroups = c(routes_sf$route_id,"Stops"),
                   options = layersControlOptions(collapsed = TRUE))
#############################################################################################
message('Sending all the outputs from Stops')
return(list(map_grey = final_map_grey,map_black = final_map_black, 
            shapeFile = routes_sf, routeDef = route_type_def,
            stopsFunction = y,
            routes_present = unique(routes_sf$Defn)))

}



