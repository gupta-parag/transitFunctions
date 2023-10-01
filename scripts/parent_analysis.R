library(htmlwidgets)
library(htmltools)

source("E:\\Z\\transitFunctions\\scripts\\stopsShapes.R")
source("E:\\Z\\transitFunctions\\scripts\\shapesRoutes.R")

x <- return_shapefile(route_type = 'all', name_of_file = "sanFran.zip")
# y <- return_stops(route_type = "1", name_of_file = "chicago_transit.zip" )
# 
# stopsByRoutes <- y[["stopsGeo"]]
# stopLabels <- y[["labels"]]
x[[1]]

saveWidget(x[[2]], "chicago.html")  

# %>%
#   addCircleMarkers(data = stopsByRoutes, lat = stopsByRoutes$stop_lat,
#                    radius = 3, color = "red", 
#                    label = stopLabels,
#                    lng = stopsByRoutes$stop_lon,  
#                    group = "stops")
