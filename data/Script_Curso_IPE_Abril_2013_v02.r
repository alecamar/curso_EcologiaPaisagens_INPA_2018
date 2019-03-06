###########################################################################################
###
###								SCRIPT CURSO IPE - Abril 2013
###
###########################################################################################
###
###
###		Objetivos
###
###			- Entrar, manipular e plotar vetores
###			- Entrar, manipular e plotar raster
###			- Calcular métricas espaciais básicas 
###
###
###
############################################################################################
###
###			Importa pacotes
###
############################################################################################

require(shapefiles)
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(plotKML)
library(raster)
require(SDMTools)

############################################################################################
###
###		Pasta de trabalho
###
############################################################################################

setwd("C:/Users/Alexandre/Documents/Ale/Consultoria/Curso IPE/Aula_R")

############################################################################################
###
###		Entra mapa shp
###
############################################################################################

#mapa.ipe<-readShapeSpatial("sel_2_07")
#proj4string(mapa.ipe) <- CRS("+init=epsg:29194")

dsn<-getwd()
mapa.ipe<-readOGR(dsn=dsn, layer="sel_2_07")


############################################################################################
###
###		Explora e manipula mapa
###
############################################################################################

###		Explora mapa
mapa.ipe

###		Plota mapa
plot(mapa.ipe)
plot(mapa.ipe, col="red")
plot(mapa.ipe, border="red")
plot(mapa.ipe, col="black", border="red")
plot(mapa.ipe, col=(mapa.ipe$CLAS_NIVEL))

###		Cria KML
plotKML(mapa.ipe, color="red")

###		Classes do mapa
lev.ipe<-levels(mapa.ipe$CLAS_NIVEL)

lev.ipe

###		Separando as classes do mapa

mapa.ipe.agu <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[1],]
mapa.ipe.ini <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[2],]
mapa.ipe.med <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[3],]
mapa.ipe.ava <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[4],]
mapa.ipe.rup <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[5],]
mapa.ipe.umi <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[6],]
mapa.ipe.lim <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[7],]
mapa.ipe.suj <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[8],]
mapa.ipe.cab <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[9],]
mapa.ipe.deg <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[10],]
mapa.ipe.rur <- mapa.ipe[mapa.ipe$CLAS_NIVEL %in% lev.ipe[11],]

plot(mapa.ipe.agu, col="blue")

###		Plota mapa com classes
x11(1400, 800)
plot(mapa.ipe.agu, col="blue")
plot(mapa.ipe.ini, col="lightgreen", add=TRUE)
plot(mapa.ipe.med, col="green", add=TRUE)
plot(mapa.ipe.ava, col="darkgreen", add=TRUE)
plot(mapa.ipe.rup, col="grey", add=TRUE)
plot(mapa.ipe.umi, col="purple", add=TRUE)
plot(mapa.ipe.lim, col="yellow", add=TRUE)
plot(mapa.ipe.suj, col="orange", add=TRUE)
plot(mapa.ipe.cab, col="brown", add=TRUE)
plot(mapa.ipe.deg, col="black", add=TRUE)
plot(mapa.ipe.rur, col="black", add=TRUE)

cores96<-c("blue", "lightgreen", "green", "darkgreen", "grey", "purple", "yellow", "orange", "brown", "black", "black")
legend("right", legend = c("Água", "Inicial", "Média", "Avancada", "Rupestre", "Campo Úmido", "Pasto Limpo", "Pasto Sujo", "Cabruca", "Degradado", "Casas"), col=cores96, text.col = cores96)

###		Cria figuras 

png("Flo_med.png", bg="white")
plot(mapa.ipe.med,col='green', border="green")
dev.off()

pdf("Flo_pasto2.pdf", bg="transparent")
plot(mapa.ipe.lim, col="orange", border="black")
title("Pasto Limpo")
axis(1)
axis(2)
dev.off()

###		Manipula mapa

class(mapa.ipe.med)

str(mapa.ipe.med) # ppt

slotNames(mapa.ipe.med)



###		"Tabela de Atributos"
mapa.ipe.med@data

############################################################################################
###
###		Calcula Métricas
###
############################################################################################

###		Area

mapa.ipe.med@polygons

mapa.ipe.med@polygons[[1]]@area

i<-1
area.tot<-NA
while (i < length(mapa.ipe.med)+1){
	area<-data.frame(mapa.ipe.med@polygons[[i]]@area)
	area.tot<-rbind(area.tot, area)
	i<-i+1
	}
	area.tot<-area.tot[-1,]
	
	hist(area.tot)
	mean(area.tot)
	sum(area.tot)
	
area.med.gAr <-gArea(mapa.ipe.med)
area.med.sum <-sum(area.tot)

area.med.gAr
area.med.sum

### Ou...

area.sapp <- data.frame(area=sapply(slot(mapa.ipe.med,"polygons"), slot, "area"))

head(area.sapp)
sum(area.sapp)
sub.area.sapp<-subset(area.sapp, area.sapp>259480.5)

sub.area.sapp/10000

round(sub.area.sapp/10000)

round(sub.area.sapp/10000, digits=2)

###		Métricas patch

rast.mapa.ipe.med <- vect2rast(mapa.ipe.med, cell.size=5)

image(rast.mapa.ipe.med)

patch.metrics = PatchStat(rast.mapa.ipe.med)

dt<-cbind(mapa.ipe.med@data, patch.metrics)

mapa.ipe.med@data<-dt
mapa.ipe.med

dt.limpa<-dt[,-(1:3)]

cor(dt.limpa)

dsn<-getwd()
writeOGR(mapa.ipe.med2, dsn=dsn, layer="mapa.ipe.res", driver="ESRI Shapefile")




