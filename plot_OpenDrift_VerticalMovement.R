#
#  Andres Sepulveda (andres.sepulveda@gmail.com) 2020/08/19
# 

library(ncdf4)

setwd("C:/Users/dgeo/R_code")

inp <-nc_open('20m_E1_av.nc')
lat <- ncvar_get(inp,varid ='lat')       # Dimensiones (tiempo,n_particula)
lon <- ncvar_get(inp,varid ='lon')
status <- ncvar_get(inp,varid ='status')
z <- ncvar_get(inp,varid ='z')
ptime <- ncvar_get(inp,varid ='time')
trajectory <- ncvar_get(inp,varid ='trajectory')
nc_close(inp)

ptime <- ptime - ptime[1]   # Numero de segundos desde el inicio

p <- 1
minh = 50 # numero minimo de horas de deriva
maxp = 10 # máximo numero de partículas a graficar

for (i in 1:length(trajectory)) {
  indx <- which(z[,i] < 1)
  if (length(indx) > minh & p == 1) {  
    plot(ptime[indx]/(3600*24),z[indx,i],main="Movimiento Vertical"
         ,xlab="Tiempo [d]",ylab="Profundidad [m]")
    p <- p + 1
  }
  if (length(indx) > minh & p < maxp) {  
    points(ptime[indx]/(3600*24),z[indx,i])
    p <- p + 1
  }
}
print(m) # Particulas graficadas