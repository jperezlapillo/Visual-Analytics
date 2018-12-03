# Carga de shapefile

reg13 <- readOGR(dsn= "./data/shp-R13", layer="Limite_Urbano_Censal", 
                    encoding = "UTF-8", verbose = FALSE)
				
# Necesito transformarlo a EPSG 4326 pero por alguna razón que desconozco
# no lo puedo lograr sin antes transformalo a EPSG 3857
reg13_prj <- spTransform(reg13, 
                           CRSobj = CRS("+init=epsg:3857"))

reg13_prj <- spTransform(reg13_prj, 
                           CRSobj = CRS("+init=epsg:4326"))
						   

sacar_comuna <- c("COLINA", "ALHUÉ", "BUIN", "CURACAVÍ", "EL MONTE", "ISLA DE MAIPO",
           "LAMPA", "MARÍA PINTO", "MELIPILLA", "PAINE", "PEÑAFLOR", 
           "SAN JOSÉ DE MAIPO", "PIRQUE",
           "TALAGANTE", "TILTIL", "CALERA DE TANGO", "LO AGUIRRE",
           "CIUDAD DEL VALLE") 

sacar_urbano <- c("LO AGUIRRE", "CIUDAD DEL VALLE")

reg13_prj <- reg13_prj[!(reg13_prj@data$DESC_COMUN %in% sacar_comuna), ]

reg13_prj <- reg13_prj[!(reg13_prj@data$DESC_URBAN %in% sacar_urbano), ]

reg13_prj_con <- reg13_prj[reg13_prj$CATEGORIA == "CIUDAD", ]


saveRDS(reg13_prj_con, "./data/reg13_prj_con.rds")
