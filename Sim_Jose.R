#
#  Andres Sepulveda (andres.sepulveda@gmail.com) 2020/08/04
#  Andres Sepulveda (andres.sepulveda@gmail.com) 2021/10/07
# 

#rm(list=ls())
library(ncdf4)
library(geodist)

setwd("C:/Users/dgeo/R_code")

#inp <-nc_open('Sim_CHONOS.nc')
inp <-nc_open('Sim1.nc')
lat <- ncvar_get(inp,varid ='lat')       # Dimensiones (tiempo,n_particula)
lon <- ncvar_get(inp,varid ='lon')
status <- ncvar_get(inp,varid ='status')
z <- ncvar_get(inp,varid ='z')
ptime <- ncvar_get(inp,varid ='time')
trajectory <- ncvar_get(inp,varid ='trajectory')
nc_close(inp)

latini <- c()
latend <- c()
lonini <- c()
lonend <- c()
t_ini  <- c()

for (part in 1:length(trajectory)) {
  #lat
  auxlat <- lat[,part]
  auxlat2 <-auxlat[auxlat < 1]
  latini[part] <- auxlat2[1]
  latend[part] <- auxlat2[length(auxlat2)]
  # lon
  auxlon <- lon[,part]
  auxlon2 <-auxlon[auxlon < 1]
  lonini[part] <- auxlon2[1]
  lonend[part] <- auxlon2[length(auxlon2)]
  # time
  auxtim <- ptime[which(auxlon < 1)[[1]]]
  t_ini[part] <- auxtim[1]
}

dista <-c()
for (i in 1:length(lonini)) {
  pini <- c(lonini[i],latini[i])
  pend <- c(lonend[i],latend[i])
  #   colnames(pini) <- colnames(pend) <-c("lon","lat")
  dista[i]  <- geodist(pini,pend,measure="haversine")
}

#
# TODO:  Integrar distancia a lo largo de la trayectoria
#

iniend <- cbind(t_ini,lonini,latini,lonend,latend,dista)

write.csv(iniend,'InicialesFinales.csv')

par(mfrow=c(1,2),mar=c(6,5,5,3))
plot(lonini,latini,main="Inicio",xlab="Longitud"
     ,ylab="Latitud",xlim=c(-75, -72),ylim=c(-42,-41.3))
plot(lonend,latend,main="Fin",xlab="Longitud"
     ,ylab="Latitud",xlim=c(-75, -72),ylim=c(-42,-41.3))


par(mfrow=c(1,1))
plot(latini,dista/1000,main="Distancia",xlab="Latitud"
     ,ylab="Distancia [km]",xlim=c(-42,-41.3))

#
# TODO: Las particulas que viajan mucho, quedan cerca de la costa?
#


hist(dista/1000,main="",xlab = "Distancia [km]",ylab = "%",freq = TRUE)
