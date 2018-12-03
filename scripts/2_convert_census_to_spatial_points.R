########################################################################
## Project: VA
## Script purpose: Convert census spatial polygons dataframe to 
##                 Spatial Points Data Frame. 
##
## Date:
## Author:
########################################################################

## Load census data at block level ------------------------------------
census <- readRDS("./data/shpnwRM.rds") 

## Load shapefile of Metropolitan Region ------------------------------
## we´ll use this to subset census data
shp_rm <- readRDS("./data/reg13_prj_con.rds")

# Census subsetting ---------------------------------------------------
pick_communes <- unique(as.character(shp_rm$DESC_COMUN))
census_stgo <- census[census$DES_COMU %in% pick_communes , ]  
census_stgo <- census_stgo[census_stgo$CATEGORIA == "CIUDAD", ]  

delete <- c("LO AGUIRRE", "CIUDAD DEL VALLE", "EL MAITÉN", 
            "LO HERRERA", "EL COLORADO", "LA PARVA")
census_stgo <- census_stgo[!(census_stgo@data$NOMBRE %in% delete), ]

# Get only polygons with census data
census_stgo <- census_stgo[!is.na(census_stgo$gse), ]

# Convert Spatial Polygon Data Frame of Census to Spatial Points Data Frame
blocks_centers <- SpatialPointsDataFrame(gCentroid(census_stgo, byid=TRUE), 
                                      census_stgo@data, match.ID=FALSE)

rm(census, census_stgo, delete, pick_communes)



