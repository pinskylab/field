# Sarah's mapping contour code  


# Sarah's mapping contour code  
library(marmap)
library(mapdata)
library(RColorBrewer)
library(classInt)

dat <- getNOAA.bathy(124.5,125,10.5,11,res=0.01, keep=TRUE) # is this defining the lat long? - changed the res from 1 to 0.01 in order to try to remove grey pixels on land

# Create color palettes
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
plot(dat,
  xlim=c(124,125), # limit long
  ylim=c(10.5,11), # limit lat
  im=TRUE,
  land=TRUE,
  bpal=list(c(min(dat),0,blues),c(0,max(dat),greys)),
  lwd=.09,las=1,cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75 )

map("worldHires", res=0, lwd=0.7, add=TRUE)

# add contour lines
plot(dat, deep=-5, shallow=-5, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE) 
plot(dat, deep=-10, shallow=-10, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-15, shallow=-15, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-20, shallow=-20, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
