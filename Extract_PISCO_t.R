library(raster)
library(ncdf4)
########## Descript PISCOt_v1.1 ############
# Resolución: 0.1° x 0.1° (~10 km)
# Data: 1981 - 2016

########## IMPUT ###########
setwd('C:/Users/Kevin/Desktop/UNALM/SextoCiclo/Est_Climat/PISCO') # Establecer environment
lon_lat <- read.csv('Estaciones.csv', sep = ';') # Estaciones
raster_tmax_m <- raster::brick('PISCOmtx_v1.1.nc') # modelo: temperatura máxima; 'brick = bloque'
raster_tmin_m <- raster::brick('PISCOmtn_v1.1.nc')

########### PROCESS ##########
sp::coordinates(lon_lat) <- ~Lon+Lat # Establecer coordenadas

# Temperatura máxima
raster::projection(lon_lat) <- raster::projection(raster_tmax_m) # Acoplamos puntos de estaciones con raster
tmax_values <- raster::extract(raster_tmax_m[[1]], lon_lat, cellnumbers = T)[,1] # extracción de tmax
tmax <- data.frame(t(raster_tmax_m[tmax_values])) # Guardado como dataframe
colnames(tmax) <- as.character(lon_lat$Nombre) # reemplazar nombre de columnas
tmax$mean <- apply(tmax[ , c(1,2,3,4,5,6,7)], 1, mean, na.rm = TRUE)


# Temperatura mínima
raster::projection(lon_lat) <- raster::projection(raster_tmin_m) 
tmin_values <- raster::extract(raster_tmin_m[[1]], lon_lat, cellnumbers = T)[,1]
tmin <- data.frame(t(raster_tmin_m[tmin_values]))
colnames(tmin) <- as.character(lon_lat$Nombre)
tmin$mean <- apply(tmin[ , c(1,2,3,4,5,6,7)], 1, mean, na.rm = TRUE)

# Temperatura media
tmean <- data.frame((tmax$mean+tmin$mean)/2)
names(tmean) <- c('tmean')

########## OUTPUT ##########
#write.csv(tmean, "tmean.csv", quote = F, row.names = F)
