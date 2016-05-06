
# Please install corectly the packages by using install.packages or devtools::install_github

options(java.parameters = "-Xmx4g" )

library(XLConnect)
library(lubridate)
library(leaflet)
library(mapview)
library(cartography)
library(RColorBrewer)
library(rworldmap)
library(maptools)

##################################################################à
# load data and check 

summer=loadWorkbook("DATI_ANALISI_ESTATE.xlsx")

sheets=getSheets(summer)

data_summer = readWorksheet(summer, sheet = 1,startRow = 1,endRow = 29,startCol = 0)
##################################################################à
# Convert capitals of country from GSOD format 

data_summer$LAT=data_summer$LAT/1000
data_summer$LONG=data_summer$LONG/1000

##################################################################à
# Calculate index for mapping 


data_summer$Heat.related.children.risk_C=cut(data_summer$Heat.related.children.risk, breaks=c(0,0.2,0.4,0.6,0.8,1),labels = 1:5)
data_summer$Heat.related.elderly.risk_C=cut(data_summer$Heat.related.elderly.risk, breaks=c(0,0.2,0.4,0.6,0.8,1),labels = 1:5)

levels(data_summer$Heat.related.elderly.risk_C)=c("Very low risk","Low risk", "Moderate risk","High risk", "Very high risk")
levels(data_summer$Heat.related.children.risk_C)=c("Very low risk","Low risk", "Moderate risk","High risk", "Very high risk")

##################################################################à
# Create geo objects 


sp_data_summer=data_summer


coordinates(sp_data_summer) =~ LONG+LAT
proj4string(sp_data_summer)=CRS("+init=epsg:4326")

saveRDS(sp_data_summer,"sp_data_summer.rds")

##################################################################à
# Connect point data to polygon data of R cartography packages

sp_data_summer=readRDS("sp_data_summer.rds")

ind_k=which(sapply( nuts0.spdf$id, function(x) x %in% sp_data_summer$PAESE)==T)
nuts_heat=nuts0.spdf[ind_k,]

newdata <- sp_data_summer@data[order(sp_data_summer@data$PAESE),] 
nuts_heat@data=cbind(nuts_heat@data,newdata)

sp_data_summer_nuts=spTransform(sp_data_summer, CRS(proj4string(nuts0.spdf))) # from wgs84 to Europe projection epgs 3035

saveRDS(sp_data_summer_nuts,"sp_data_summer_nuts.rds")

##################################################################à
# Connect point data to polygon data of R cartography packages


sp_data_summer_nuts=readRDS("sp_data_summer_nuts.rds")

##################################################################################################à
# use different world basis for maps

newmap <- getMap(resolution = "low")  # different resolutions available "coarse","low","less islands","li","high".
orig.world.map_nuts=spTransform(newmap, CRS(proj4string(nuts0.spdf)))

##################################################################################################à
# Set graphical parameters

opar <- par(mar = c(0.1,0.1,1.4,0.1))

##################################################################################################à
# Plot a layer with the extent of the EU28 countries with only a background color


plot(nuts0.spdf, border = NA, col = NA,xlim=c(2641758, 5913157), bg = "#A6CAE0")

# Plot non european space

plot(orig.world.map_nuts, border="grey10", col  = "#E3DEBF",  lwd=0.15, add=TRUE)

# create palette

carto.pal = colorRampPalette(c("green","yellow","orange","red","purple"))(5)

# choroplets for factors

typoLayer( spdf = nuts_heat, # SpatialPolygonsDataFrame 
           df = nuts_heat@data, # data frame 
           var = "Heat.related.elderly.risk_C", # compound annual growth rate field in df
           #breaks = c(0,0.2,0.4,0.6,0.8,1), # list of breaks
           col = carto.pal, # colors 
           border = "grey40", # color of the polygons borders
           lwd = 0.5, # width of the borders
           legend.pos = "topleft", # position of the legend
           legend.title.txt = "", # title of the legend
           #legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame=F,
           add = TRUE) # add the layer to the current plot

# Put capitals

plot(sp_data_summer_nuts, col  = "black",  pch=20,cex=0.6,add=TRUE)


# Layout plot

layoutLayer(title = "Heat-related Elderly Risk", author = "Author: a.crisci@ibimet.cnr.it", 
            sources = "IBIMET CNR, 2016", frame = TRUE,
            scale = NULL ,coltitle = "black",
            south = TRUE,
            col = "#688994") # add a south arrow

dev.copy(png,'heat_children_risk.png')
dev.off()
##########################################################################################################################à

##################################################################################################à
# Set graphical parameters

opar <- par(mar = c(0.1,0.1,1.4,0.1))

##################################################################################################à
# Plot a layer with the extent of the EU28 countries with only a background color


plot(nuts0.spdf, border = NA, col = NA,xlim=c(2641758, 5913157), bg = "#A6CAE0")

# Plot non european space

plot(orig.world.map_nuts, border="grey10", col  = "#E3DEBF",  lwd=0.15, add=TRUE)

# create palette

carto.pal = colorRampPalette(c("green","yellow","orange","red","purple"))(5)

# choroplets for factors

typoLayer( spdf = nuts_heat, # SpatialPolygonsDataFrame 
           df = nuts_heat@data, # data frame 
           var = "Heat.related.children.risk_C", # compound annual growth rate field in df
           #breaks = c(0,0.2,0.4,0.6,0.8,1), # list of breaks
           col = carto.pal, # colors 
           border = "grey40", # color of the polygons borders
           lwd = 0.5, # width of the borders
           legend.pos = "topleft", # position of the legend
           legend.title.txt = "", # title of the legend
           #legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame=F,
           add = TRUE) # add the layer to the current plot

# Put capitals

plot(sp_data_summer_nuts, col  = "black",  pch=20,cex=0.6,add=TRUE)


# Layout plot

layoutLayer(title = "Heat-related Children Risk", author = "Author: m.morabito@ibimet.cnr.it", 
            sources = "IBIMET CNR, 2016", frame = TRUE,
            scale = NULL ,coltitle = "black",
            south = TRUE,
            col = "#688994") # add a south arrow

dev.copy(png,'heat_children_risk.png')
dev.off()

################################################################################################################
# References 

# https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html
# http://www.endmemo.com/program/R/pchsymbols.php




