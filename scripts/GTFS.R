library(sf)
library(data.table)
library(lehdr)
library(tidycensus)
library(stringr)
library(dplyr)
library(leaflet)
library(tidyr)
library(htmltools)
library(leaflet)
library(rvest)
library(lubridate)

setwd("D:\\PASSION_PROJECTS\\cta")



######## writting own summary function #####

sum_detail <- function(d_frame){
  message(str_c("Total Columns : ", ncol(d_frame),
              " ; Total Rows : ", nrow(d_frame)))
  
  message(str_c(  "Column Names : ", paste0(colnames(d_frame), collapse = " , ")))
  
  
  i <- 1
 x <- lapply(d_frame, function(x){
   
   if(length(unique(x)) >= 3 ) {
     message( str_c( "Name " , i," : ", colnames(d_frame)[i],
                   " ; Length Unique : ",length(unique(x)),
                   " ; Values : ", paste0(unique(x)[1:3], collapse = ",") ))  
   } else {
     message( str_c( "Name " , i," : ", colnames(d_frame)[i],
                   " ; Length Unique  : ",length(unique(x)),
                   " ; Values : ", paste0(unique(x), collapse = ",")))
   }
   i <<- i + 1
 })
}

scrape_stops <- function(){
  routes_f <- routes[ route_type == 3, ]
  stops_list <- lapply(1:nrow(routes_f), sum)
  
 stops_list <-   map(1:nrow(routes_f),possibly(
   function(x){
    print(  str_c(x , " : ", routes_f$route_long_name[x]))
    l <- html_table(read_html(str_c("https://www.transitchicago.com/assets/1/6/stoplist_",
                                    routes_f$route_short_name[x],".htm")), header = T)
    l[[1]]$route_long_name <- routes_f$route_long_name[x]
    l[[1]]$route_short_name <- routes_f$route_short_name[x]
    stops_list[[x]] <- l[[1]]
              }, 
   otherwise = NA_real_))
    
names(stops_list) <- routes_f$route_short_name
 flat_stops <-  setDT(do.call(rbind.data.frame, stops_list))
return(flat_stops)
}



###### reading files #########
transfers <- read.csv("transfers.csv")
files_paths <- list.files("./google_transit", full.names = T)
files_names <- list.files("./google_transit")
files <- lapply(files_paths, fread)
names(files) <- gsub(pattern = "\\.txt$", replacement = "", x = files_names)
cta_data <- files



# assigning all the value to the variables
routes <- cta_data[["routes"]]
trips <- cta_data[["trips"]]
calendar <- cta_data[["calendar"]]
stops <- cta_data[["stops"]]
stop_times <- cta_data[["stop_times"]]
stop_times$arrival_time <- hms(stop_times$arrival_time)
shapes <- cta_data[["shapes"]]

# getting trips by routes and route type

tripRoute <- left_join(trips, routes, by = "route_id")
tripsByRoute <-  tripRoute[ , .(trips = .N), 
                by = list(route_type, route_id, 
            route_short_name, route_long_name, route_url) ]

##### scraping all the stops list from the cta website

stops_list_scrape <- scrape_stops()

stopsByRoute <- stops_list_scrape[ , .(Total_Stops = .N), 
                  by = list(route_short_name, route_long_name) ]
stops_dupl_scrape <- as.data.frame(unique(stops_list_scrape$`Stop ID#`[duplicated(
                        stops_list_scrape$`Stop ID#`)]))
                               

stop_trips <- stop_times[ , .(tripsByStops = .N,
                              arrival_time), by = list(stop_id)]






##### getting the schedule of the trips
t <- unique(trips[routes[route_type == 1], on = .(route_id = route_id)], 
            by = c("route_id","service_id"))

t[ ,.(trips = .N) , by = list(route_id, route_type)]



# unique service ids for subways

 service_subway <- unique(unique(trips[routes[route_type == 1], on = .(route_id = route_id)], 
             by = c("route_id","service_id"))[, .(service_id)])


routes_service <- calendar[service_subway, on = .(service_id = service_id)]

routes_service[, Days := paste0(monday,tuesday,wednesday,
                   thursday,friday,saturday,
                   sunday),]

tr_sub <- trips[routes[route_type == 1], on = .(route_id = route_id)][ , .(route_id, service_id,trip_id)]
stop_times_f <- stop_times[,.(trip_id,arrival_time,departure_time,stop_id,stop_sequence)]
final_table <- stop_times_f[tr_sub, on = .(trip_id = trip_id)]

stats <- final_table[ , .(stops_n = .N ), by = .(route_id, service_id, trip_id)]
x <- stats[, .SD[which.max(stops_n)], by= . (route_id, service_id)]
 #final stops table 
y <- stats[, .SD[which.max(stops_n)], by= . (route_id)]




### shapes lookup table

shapes_lookup <- shapes[ , .(total_points = .N), by = shape_id ]
tbrss <- trips[,  .(trips_n = .N), by = .(route_id,service_id,shape_id, direction)]
by_route_parent <- split(tbrss, factor(tbrss$route_id))

for( i in 1:length(by_route_parent)) {
   print(i)
  x <- left_join(by_route_parent[[i]], shapes_lookup, by = "shape_id")
 # y <- x[ , .SD[ which.max(total_points) ], by = direction]
  y <- x[which.max(total_points)]
#print(x)
  z <- left_join(y, shapes, by = "shape_id")
  #route_by_dir <-  split(z, factor(z$direction))
  # route_by_dir_sf <- lapply(route_by_dir, function(x){
  #   st_linestring(as.matrix(x[, c("shape_pt_lon", "shape_pt_lat")]))
  # })
  route_by_dir_sf <- st_linestring(as.matrix(z[, c("shape_pt_lon", "shape_pt_lat")]))
by_route_parent[[i]] <- route_by_dir_sf
}

#y <- unlist(by_route_parent,recursive = F)
#names(y) <- NULL
route_names <- names(by_route_parent)
names(by_route_parent) <- NULL
(route_sfc <- st_sfc(by_route_parent)) #paranthese are important to convert the list object into sfc 
st_crs(route_sfc) <- '+proj=longlat +datum=WGS84'
  
route_sf<- st_sf(route_id = route_names, route_sfc)
routes_sf <- left_join(route_sf, routes, by = "route_id")
routes_sf$color_r <- paste0("#", routes_sf$route_color)
subway <- routes_sf[routes_sf$route_type == 1,]

 map_sub <- leaflet() %>% addProviderTiles(providers[[113]]) 

for( i in 1:nrow(subway)){
  map_sub <- addPolylines(map = map_sub, data= subway[i,],
                        label = subway$route_long_name[i],
                        group = subway$route_long_name[i], 
                        color = subway$color_r[i],
                        weight = 3,
                        opacity = 1, 
                        highlightOptions = highlightOptions(bringToFront = TRUE, weight = 7 )
                       # highlightOptions = highlightOptions(dashArray = "3",bringToFront = T)
                        )
}
final_map <-  map_sub %>% addLayersControl(overlayGroups = subway$route_long_name,
                              options = layersControlOptions(collapsed = FALSE))


## Fitlreing the files
trips_f <- trips[, .(shape_id,service_id,route_id,trip_id,direction), ]
shapes_f <- shapes[ , .(shape_id,shape_pt_lat ,shape_pt_lon,shape_pt_sequence)]
routes[ , .(route_id, route_short_name, route_long_name, route_type , route_color),]



