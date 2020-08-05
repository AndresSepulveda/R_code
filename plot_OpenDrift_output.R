#
#  Andres Sepulveda (andres.sepulveda@gmail.com) 2020/08/04
# 

#rm(list=ls())
library(ncdf4)
library(geodist)

setwd("C:/Users/dgeo/R_code")

inp <-nc_open('20m_E1_av.nc')
  lat <- ncvar_get(inp,varid ='lat')
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
}

dista <-c()
for (i in 1:length(lonini)) {
   pini <- c(lonini[i],latini[i])
   pend <- c(lonend[i],latend[i])
#   colnames(pini) <- colnames(pend) <-c("lon","lat")
   dista[i]  <- geodist(pini,pend,measure="haversine")
}

iniend <- cbind(lonini,latini,lonend,latend,dista)

write.csv(iniend,'InicialesFinales.csv')

par(mfrow=c(1,2),mar=c(6,5,5,3))
plot(lonini,latini,main="Inicio",xlab="Longitud"
     ,ylab="Latitud",xlim=c(-75, -70),ylim=c(-34,-22))
plot(lonend,latend,main="Fin",xlab="Longitud"
     ,ylab="Latitud",xlim=c(-75, -70),ylim=c(-34,-22))


par(mfrow=c(1,1))
plot(latini,dista/1000,main="Distancia",xlab="Latitud"
     ,ylab="Distancia [km]",xlim=c(-34,-22))


hist(dista/1000,main="",xlab = "Distancia [km]",ylab = "%",freq = FALSE)