#
#  Andres Sepulveda (andres.sepulveda@gmail.com) 2020/08/06
# 
options(warn = -1)
#rm(list=ls())
#library(ncdf4)
library(geodist)
library(bazar)

setwd("C:/Users/dgeo/R_code")

iniend <- read.csv(file='InicialesFinales.csv')
#dim(inifin)

#
# ene, lonini, latini, lonend, latend, dista
#
lonini <- iniend[,2]
latini <- iniend[,3]
lonend <- iniend[,4]
latend <- iniend[,5]

amers <- read.csv(file='IFOP_AMERS.txt',sep=" ")
#dim(amers)

lon_a <- amers[,1]
lat_a <- amers[,2]

#
# Calculo origen-destino (a donde van)
#

tutto <- 0
dista    <- c()
mindista <- matrix(, nrow=length(5), ncol=length(lonend))
ori <- c()
lat_ori <- c()
lon_ori <- c()
all_lonlat_ori <- c()
m <- 2
for (i in 1:length(lon_a)) {
    lonlat_ori <- c()
    dista <- geodist_vec(lon_a[i],lat_a[i],lonend,latend,measure="haversine")/1000
    indx <- which(dista < 1) # particulas que terminan a menos de 1 km de esa AMERB
    if (length(indx) > 0){
      iindx <- length(indx) + 1
      lonlat_ori <- matrix(,nrow=iindx,ncol=2)
      lonlat_ori[1,] <- cbind(lon_a[i],lat_a[i])
      for (j in 2:iindx) {
        jm1 <- j -1
        aux <- indx[jm1]
        #
        #  De donde vienen, las que llegaron
        #
        lonlat_ori[j,] <- cbind(lonini[aux],latini[aux]) 
      }
      all_lonlat_ori[[m]] <- lonlat_ori
      m <- m + 1
    }  
    auxtutto <- length(indx)
    tutto = tutto + auxtutto
}
ctutto <- as.character(tutto)
porcen <- (tutto/length(lonend))*100
cporcen <- as.character(porcen)
cat(ctutto," particulas llegaron a una AMERB, un ",cporcen, "%")

#
# TODO ¿Como exportamos la lista all_lonlat_ori?
#

