#
#  Andres Sepulveda (andres.sepulveda@gmail.com) 2020/08/06
# 

#rm(list=ls())
#library(ncdf4)
library(geodist)

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
# Cálculo origen-destino (a donde van)
#

for (i in 1:length(lon_a)) {
  for (j in 1:length(lonini)) {
    pamer <- c(lon_a[i],lat_a[i])
    pend  <- c(lonend[j],latend[j])
    dista[i]  <- geodist(pend,pamer,measure="haversine")

  }
}