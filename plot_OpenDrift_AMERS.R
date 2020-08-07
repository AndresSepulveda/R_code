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
for (i in 1:length(lon_a)) {
    dista <- geodist_vec(lon_a[i],lat_a[i],lonend,latend,measure="haversine")/1000
    indx <- which(dista < 1) # particulas que terminan a menos de 1 km de esa AMERB
    auxtutto <- (length(indx))
    tutto = tutto + auxtutto
}
print(tutto)
print((tutto/length(lonend))*100)