library(raster)
library(ncdf4)
library(pastecs)
########## Descript PISCOpm  ############
# Resolución: 0.1° x 0.1° (~10 km)
# Data: 1981 - 2016
########## IMPUT ###########
setwd('C:/Users/Kevin/Desktop/UNALM/SextoCiclo/Est_Climat/PISCO') # Establecer environment
lon_lat <- read.csv('Estaciones.csv', sep = ';') # Estaciones
raster_pp_m <- raster::stack('PISCOpm.nc') # modelo: precipitación mensual; 'stack = apilar'
    # el :: es para detallar que se usará la función perteneciente al paquete raster
########### PROCESS ##########
sp::coordinates(lon_lat) <- ~Lon+Lat # Establecer coordenadas: dataframe => spatial port dataframe (spdf)

# Precipitación
raster::projection(lon_lat) <- raster::projection(raster_pp_m) # para que las estaciones tengan igual proyección que PISCO
pp_m_values <- raster::extract(raster_pp_m, lon_lat, method = 'simple') # extracción de datos: rows son estaciones
pp <- data.frame(t(pp_m_values)) # Guardado como dataframe, se usa la traspuesta para volver estaciones a columnas
colnames(pp) <- as.character(lon_lat$Nombre) # reemplazar nombre de columnas
pp_mean <- apply(pp[ , c(1,2,3,4,5,6,7)], 1, mean, na.rm = TRUE)

########## OUTPUT ##########
# nota:  podemos plotear usando
plot(raster_pp_m[[1]])
plot(lon_lat, ad=T) # ad es para que los puntos se ploteen encima del mapa
# detallas el tiempo (t) en doble corchetes son importantes porque si no graficaría todos los t disponibles
write.csv(pp_mean, "pp_mean.csv", quote = F, row.names = F)
