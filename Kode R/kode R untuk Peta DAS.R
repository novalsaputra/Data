options(scipen =2)


################################################################
################## PETA DAS DENGAN LIBRARY SPPLOT ##############
################################################################
library(maptools)
gpclibPermit()
library(sp)
library(foreign)
library(lattice)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(class)
library(e1071)
library(shapefiles)

x <- readShapeSpatial("D:/tugas s2/SPATIAL/citarum_peta_ordo3/ordo3_all.shp")
df.ordo<-as.data.frame(x)
df.ordo
plot(x)
dataDAS<- read.table("D:/tugas s2/TESIS..... FIGHT!!!/Kode R/data asli.txt",header=T)
head(dataDAS)


dataDAS$No<- as.numeric(dataDAS$No)
str(dataDAS)

x@data$row<- as.numeric(row.names(x@data))
x@data$IDp<- as.numeric(row.names(x@data))


temp<- merge(x@data,dataDAS, by="IDp", all.x=T, sort=F)
x@data<- temp[order(temp$row),]
plotvar<- x@data$No
nclr<- 12
plotclr<- brewer.pal(nclr,"Paired")
plotclr<- plotclr[1:nclr]
class<- classIntervals(plotvar, nclr, style="equal")

colcode<- findColours(class, plotclr, digits=4)
plot(x, density=16, col="grey", axes=T, cex.axis=.75)
plot(x, col=colcode, add=T)
text(coordinates(x), labels=x$ORDO3, cex=0.5,col="black")
title(xlab="latitude",ylab='longitude',cex.lab=.8,line=2.25)
title(main=" ",sub="Mapped with R",font.sub=2)


########################### PETA ###################################
##################     DENGAN ggplot   #############################
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)



library(raster)
library(dplyr)
library(ggthemes)
library(ggalt)

set.seed(8000)
############### BACA DATA
mydatap <- read.table("D:/tugas s2/TESIS..... FIGHT!!!/Kode R/data asli.txt",header=T)
head(mydatap)


names(mydatap)
attach(mydatap)



# READ MAP
DAS.shp<- readShapeSpatial("D:/tugas s2/SPATIAL/citarum_peta_ordo3/ordo3_all.shp")
class(DAS.shp)
print(DAS.shp$IDp)

##create (or input) data to plot on map
num.states<-length(DAS.shp$ORDO3)

#fortify shape file to get into dataframe 
DAS.shp.f <- fortify(DAS.shp, region = "IDp")
class(DAS.shp.f)


#####################################################
############# PETA sebaran indeks air ###############
#####################################################

Y<-mydatap$Y

k2df<-as.data.frame(Y)
k2df

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,k2df)
head(mydata)



#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 



summary(Y)

#basic map
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = Y),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "dark blue", mid = "white", high = "dark red", midpoint =4.6 , na.value = "grey50", guide = "colourbar", guide_legend(title="y"))
plotDAS+ggtitle("Indeks Kekritisan Air")

#####################################################
############# PETA sebaran Hutan ###############
#####################################################

k2df<-as.data.frame(H)
k2df

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,k2df)
head(mydata)



#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 



summary(H)

#basic plot
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = H),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "white", mid = "green", high = "dark green", midpoint = 50 , na.value = "grey50", guide = "colourbar", guide_legend(title="H"))
plotDAS+ggtitle("Persentase Luas Hutan")


#plot berdasarkan interval
cutDATA<-cut(H,breaks=c(0,15,30,50,70,100))
cutDATA<-as.data.frame(cutDATA)
summary(cutDATA)
plot(cutDATA,col="dark green",xlab="Persentase Luas Hutan",ylab="frekuensi")
mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,cutDATA)
head(mydata)
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = cutDATA,na.values="white"),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_manual(values=c("#FFFFFF","#CCFFCC","#33CC33","#006600","#003300"),name="H",guide="legend",na.value="#FFFFFF")
plotDAS<-plotDAS+ggtitle("Persentase Luas Hutan")
plotDAS

### CATATAN : "#FFFFFF","#CCFFCC","#33CC33","#006600","#003300" Adalah kode warna. keterangan selengkapnya ada di file kode_warna.docx

#####################################################
############# PETA sebaran kebun ###############
#####################################################

k2df<-as.data.frame(K)
k2df

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,k2df)
head(mydata)



#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 



summary(K)

#basic plot
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = K),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "white", mid = "green", high = "dark green", midpoint = 50 , na.value = "grey50", guide = "colourbar", guide_legend(title="K"))
plotDAS+ggtitle("Persentase Luas kebun")

#berdasarkan interval
cutDATA<-cut(K,breaks=c(0,20,40,60,80,100))
cutDATA<-as.data.frame(cutDATA)
mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,cutDATA)
head(mydata)
summary(cutDATA)
plot(cutDATA,xlab="Persentase kebun",ylab="frekuensi",col="green")
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = cutDATA,na.values="white"),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_manual(values=c("#FFFFFF","#CCFFCC","#33CC33","#006600","#003300"),name="P",guide="legend",na.value="#FFFFFF")
plotDAS+ggtitle("Persentase Luas Kebun")

#####################################################
############# PETA sebaran Perkebunan ###############
#####################################################

k2df<-as.data.frame(P)
k2df

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,k2df)
head(mydata)



#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 



summary(P)

#basic plot
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = P),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "white", mid = "green", high = "dark green", midpoint = 30 , na.value = "grey50", guide = "colourbar", guide_legend(title="P"))
plotDAS+ggtitle("Persentase Luas Perkebunan")

#quantile plot
cutDATA<-cut(P,breaks=c(0,20,40,60,80,100))
cutDATA<-as.data.frame(cutDATA)
mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,cutDATA)
head(mydata)
summary(cutDATA)
plot(cutDATA,xlab="Persentase Perkebunan",ylab="frekuensi",col="green")
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = cutDATA,na.values="white"),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_manual(values=c("#FFFFFF","#CCFFCC","#33CC33","#006600","#003300"),name="P",guide="legend",na.value="#FFFFFF")
plotDAS+ggtitle("Persentase Luas Perkebunan")

#####################################################
############# PETA sebaran Sawah ###############
#####################################################

k2df<-as.data.frame(S)
k2df

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,k2df)
head(mydata)



#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 



summary(S)

#basic plot
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = S),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "white", mid = "Yellow", high = "brown", midpoint = 40 , na.value = "grey50", guide = "colourbar", guide_legend(title="S"))
plotDAS+ggtitle("Persentase Luas Sawah")

#quantile plot
cutDATA<-cut(S,breaks=c(0,20,40,60,80,100))
cutDATA<-as.data.frame(cutDATA)
mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,cutDATA)
head(mydata)
summary(cutDATA)
plot(cutDATA,xlab="Persentase Sawah",ylab="frekuensi",col="Orange")

merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = cutDATA,na.values="white"),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_brewer(name="S", palette = "YlOrRd",guide="legend",na.value="#FFFFCC")
plotDAS+ggtitle("Persentase Luas Sawah")

#####################################################
############# PETA sebaran Tegalan ###############
#####################################################

k2df<-as.data.frame(TG)
k2df

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,k2df)
head(mydata)



#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 



summary(TG)

#basic plot
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = TG),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "white", mid = "yellow", high = "brown", midpoint = 40 , na.value = "grey50", guide = "colourbar", guide_legend(title="TG"))
plotDAS+ggtitle("Persentase Luas Tegalan")

#quantile plot
cutDATA<-cut(TG,breaks=c(0,20,40,60,80,100))
cutDATA<-as.data.frame(cutDATA)
mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,cutDATA)
head(mydata)
summary(cutDATA)
plot(cutDATA,xlab="Persentase Tegalan",ylab="frekuensi",col="#CC6633")

merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = cutDATA,na.values="white"),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_brewer(name="TG", palette = "YlOrBr",guide="legend",na.value="#FFFFCC")
plotDAS+ggtitle("Persentase Luas Tegalan")

#####################################################
############# PETA sebaran kepadatan penduduk ###############
#####################################################

KPDTdf<-as.data.frame(KP)
KPDTdf

mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,KPDTdf)
head(mydata)


#merge with coefficients and reorder
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 


summary(KP)

#basic plot
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = KP),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_gradient2(low = "white", mid = "pink", high = "dark red", midpoint = 2000, na.value = "grey50", guide = "colourbar", guide_legend(title="KP"))
plotDAS

### berdasarkan interval
cutDATA<-cut(KP,breaks=c(0,750,1500,2250,3500),dig.lab = 4L)
cutDATA<-as.data.frame(cutDATA)
summary(cutDATA)
plot(cutDATA,col="dark red",xlab="kepadatan penduduk",ylab="frekuensi")
mydata<-data.frame(ORDO3=DAS.shp$ORDO3, id=DAS.shp$IDp,cutDATA)
head(mydata)
merge.shp.coef<-merge(DAS.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 
plotDAS<-ggplot()+geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = cutDATA),color = "grey", size = 0.2) + coord_map()
plotDAS<-plotDAS+scale_fill_manual(values=c("#FFFFFF","#FFCCCC","#CC6666","#660000"),name="KP",guide="legend")
plotDAS<-plotDAS+ggtitle("Kepadatan Penduduk")
plotDAS 







