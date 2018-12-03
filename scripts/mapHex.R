#' Creates map of SpatialPolygons Data Frame and one variable
#'
#' Receives and spdf and a variable name. It colours each polygon
#' according to the chosen variable.
#' @param hex_spdf name spdf (Spatial Polygons Data Frame)
#' @param variable variable name (character)
#' @return leaflet map
#' @export

mapHex <- function(hex_spdf, variable){
        # I need to call the variable inside the function differently 
        # to when I use Shiny. I need to explicitly call it with @data
        if(variable == "diff_1" | variable == "diff_2"){
                variablePlot <- c(-max(hex_spdf@data[, variable]), max(hex_spdf@data[, variable]))
                pal <- colorNumeric(
                palette = "RdBu",
                domain = variablePlot)
        } else {
        variablePlot <- hex_spdf@data[, variable]
        pal <- colorNumeric(
                palette = "YlOrRd",
                domain = hex_spdf@data[, variable])
        }
        labels <- sprintf("%g", hex_spdf@data[, variable]) %>% lapply(htmltools::HTML)

        map <- leaflet() %>% 
                addPolygons(data = hex_spdf,
                            fillColor = ~pal(hex_spdf@data[, variable]),
                            color = "transparent",
                            dashArray = "3",
                            fillOpacity = 0.8,
                            label = labels) %>%
                            #addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
                addLegend(pal = pal, values = variablePlot, opacity = 0.7,
                         title = "Values", position = "bottomleft")
        map
}