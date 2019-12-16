### OLS data ASLI
mydatap <- read.table("D:/tugas s2/TESIS..... FIGHT!!!/Kode R/data asli.txt",header=T)
head(mydatap)


names(mydatap)
attach(mydatap)

### Scatterplot


myvar=mydatap[c("Y","KP")]
plot(myvar)

myvar2=mydatap[c("Y","H","K","P","TG","S")]
plot(myvar2)


####### creating neighbour map ###########
library(spdep)
library(sp)
library(Matrix)
library(maptools)



DASp<- readShapePoly("D:/tugas s2/SPATIAL/citarum_peta_ordo3/ordo3_all.shp")
DASd<- readShapeSpatial("D:/tugas s2/SPATIAL/citarum_peta_ordo3/ordo3_all.shp")
summary(DASd)
DAS.d.df<- as.data.frame(DASd)
head(DAS.d.df)
summary(DAS.d.df)

################################
### Contiguity neighbour #######
################################
## Queen
class(DASd)
DAS_nb <- poly2nb(DASd,row.names = seq(1,92))
DAS_nb
plot(DASd, col="white", border="grey")
plot(DAS_nb, coordinates(DASd), col="red", add=TRUE)
text(coordinates(DASd), labels=DAS.d.df$IDp, cex=0.7, col="blue",pos=4, offset=0.4)

## ROOK
DAS_nb2 <- poly2nb(DASd, queen=FALSE, row.names = seq(1,92))
DAS_nb2
summary(DAS_nb2)
plot(DASd, col="white", border="grey")
plot(DAS_nb2,coordinates(DASd),col="red",cex=0.3,add=TRUE)
plot(DAS_nb2,coordinates(DASd),col="red",cex=0.5,add=TRUE)
plot(DAS_nb2,coordinates(DASd),col="red",cex=0.7,add=TRUE)
plot(DAS_nb2,coordinates(DASd),col="red",cex=0.8,add=TRUE)
plot(DAS_nb2,coordinates(DASd),col="red",cex=0.9,add=TRUE)
text(coordinates(DASd), labels=DAS.d.df$IDp, cex=0.7, col="blue",pos=4, offset=0.4)

##BISHOP
plot(DASd, col="white", border="grey")
plot(diffnb(DAS_nb, DAS_nb2), coordinates(DASd), col="red", add=TRUE)
text(coordinates(DASd), labels=DAS.d.df$IDp, cex=0.7, col="blue",pos=4, offset=0.4)

### difference queen and Rook
plot(DASd, col="white", border="grey")
plot(DAS_nb, coordinates(DASd), col="dark grey",add=TRUE)
plot(DAS_nb2,coordinates(DASd),col="black",cex=0.9,add=TRUE)
plot(diffnb(DAS_nb, DAS_nb2), coordinates(DASd), col="red", add=TRUE)
text(coordinates(DASd), labels=DAS.d.df$IDp, cex=0.7, col="blue",pos=4, offset=0.4)


#### weight matrix
#QUEEN
DASw <- nb2listw(DAS_nb)
summary(DASw)
#ROOK
DASw2 <- nb2listw(DAS_nb2)
summary(DASw2)


### moran test
moran.test(mydatap$Y,DASw2,randomisation = F,alternative = "two.sided")
moran.plot(mydatap$Y,DASw2, col="blue", xlab="Indeks kekritisan Air", ylab=" spatial lag")

moran.test(log(Y),DASw2)
moran.plot(log(Y),DASw2, col="blue", xlab="Indeks Kritis Air", ylab="lag")

moran.test(log(KP),DASw2,randomisation = F,alternative = "two.sided")
moran.plot(mydatap$KP,DASw2, col="blue", xlab="Kepadatan Penduduk", ylab="lag")

moran.test(mydatap$H,DASw2)
moran.plot(mydatap$H,DASw2, col="blue", xlab="Persentase Luas Hutan", ylab="lag")

moran.test(mydatap$K,DASw2)
moran.plot(mydatap$K,DASw2, col="blue", xlab="Persentase Luas Kebun", ylab="lag")

moran.test(mydatap$TG,DASw2)
moran.plot(mydatap$TG,DASw2, col="blue", xlab="Persentase Luas Tegalan", ylab="lag")

moran.test(mydatap$S,DASw2)
moran.plot(mydatap$S,DASw2, col="blue", xlab="Persentase Luas Sawah", ylab="lag")

moran.test(mydatap$M,DASw2)
moran.plot(mydatap$M,DASw2, col="blue", xlab="Persentase Luas Pemukiman", ylab="lag")

moran.test(mydatap$P,DASw2)
moran.plot(mydatap$P,DASw2, col="blue", xlab="Persentase Luas Kebun", ylab="lag")

##############  Correlogram  MORAN I #############################

mor1<-sp.correlogram(DAS_nb2, mydatap$Y, order = 6, method = "I",
                     style = "W", randomisation = TRUE, zero.policy = NULL, spChk=NULL)
plot(mor1)

mor1<-sp.correlogram(DAS_nb2, mydatap$KP, order = 6, method = "I",
                     style = "W", randomisation = TRUE, zero.policy = NULL, spChk=NULL)
plot(mor1)

mor1<-sp.correlogram(DAS_nb, mydatap$P, order = 6, method = "I",
                     style = "W", randomisation = TRUE, zero.policy = NULL, spChk=NULL)
plot(mor1)

mor1<-sp.correlogram(DAS_nb2, mydatap$H, order = 6, method = "I",
                     style = "W", randomisation = TRUE, zero.policy = NULL, spChk=NULL)
plot(mor1)

