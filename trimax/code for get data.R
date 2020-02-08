# Version info: R 3.2.3, Biobase 2.30.0, GEOquery 2.40.0, limma 3.26.8
# R scripts generated  Sat Jan 25 13:02:42 EST 2020


#Server: www.ncbi.nlm.nih.gov
#Query: acc=GSE11324&platform=GPL570&type=txt&groups=&colors=&selection=XXXXXXXXXXXX&padj=fdr&logtransform=auto&columns=ID&columns=adj.P.Val&columns=P.Value&columns=F&columns=Gene+symbol&columns=Gene+title&num=250&annot=ncbi

# Unable to generate script analyzing differential expression.
#      Invalid input: at least two groups of samples should be selected.

################################################################

# install Packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("Biobase","GEOquery"))


#   Boxplot for selected GEO samples
library(Biobase)
library(GEOquery)

# load series and platform data from GEO

gset <- getGEO("GSE11324", GSEMatrix =TRUE, getGPL=FALSE)
if (length(gset) > 1) idx <- grep("GPL570", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# set parameters and draw the plot

dev.new(width=4+dim(gset)[[2]]/5, height=6)
par(mar=c(2+round(max(nchar(sampleNames(gset)))/2),4,2,1))
title <- paste ("GSE11324", '/', annotation(gset), " selected samples", sep ='')
boxplot(exprs(gset), boxwex=0.7, notch=T, main=title, outline=FALSE, las=2)

#menjadikan data sebagai matrix
g <- exprs(gset)
str(g)

#memisahkan data perwaktu
t11 <- g[,"GSM286031"]
t12 <- g[,"GSM286032"]
t13 <- g[,"GSM286033"]
t21 <- g[,"GSM286034"]
t22 <- g[,"GSM286035"]
t23 <- g[,"GSM286036"]
t31 <- g[,"GSM286037"]
t32 <- g[,"GSM286038"]
t33 <- g[,"GSM286039"]
t41 <- g[,"GSM286040"]
t42 <- g[,"GSM286041"]
t43 <- g[,"GSM286042"]

t1 <- cbind(t11,t12,t13)
t2 <- cbind(t21,t22,t23)
t3 <- cbind(t31,t32,t33)
t4 <- cbind(t41,t42,t43)

write.table(t1,"t1.txt",col.names=FALSE,row.names=FALSE)
write.table(t2,"t2.txt",col.names=FALSE,row.names=FALSE)
write.table(t3,"t3.txt",col.names=FALSE,row.names=FALSE)
write.table(t4,"t4.txt",col.names=FALSE,row.names=FALSE)
