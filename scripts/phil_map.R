# Sarah's mapping contour code  


library(marmap) ; library(mapdata);library(RColorBrewer);library(classInt)
dat <- getNOAA.bathy(-78,-64,37,43,res=1, keep=TRUE)
# Create color palettes
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
plot(dat, xlim=c(-76.5,-65.5), ylim=c(38,41.5),im=TRUE, land=TRUE, bpal=list(c(min(dat),0,blues),c(0,max(dat),greys)), lwd=.09, las=1,cex.lab=1.75, cex.axis=1.75, cex.main=1.75, cex.sub=1.75 )
map("worldHires", res=0, lwd=0.7, add=TRUE)
plot(dat, deep=-25, shallow=-25, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-50, shallow=-50, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-75, shallow=-75, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-100, shallow=-100, step=0, lwd=0.1, drawlabel=TRUE, add=TRUE)
