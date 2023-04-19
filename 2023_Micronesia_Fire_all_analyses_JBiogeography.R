rm(list=ls())
options(scipen=999)


library(raster)
library(rgdal)
library(sp) 
library(exactextractr)
library(sf)
library(ggplot2)
library(reshape2)
library(corrplot)
library(tools)
library(reshape2)
library(MuMIn)
library(mgcv)
library(ggpubr)

#write package citations to file
pkgs<-c("raster", "rgdal", "sp", "exactextractr", "sf", "ggplot2", "reshape2", "corrplot", "tools", "factoextra","reshape2", "gstat", "MuMIn", "mgcv", "ggpubr" )
pkgcitations<-file("/Users/clayt/Desktop/micronesia_analysis_packages_bibtex.txt")
pkglist<-vector("list", length(pkgs))
for(i in 1:length(pkgs)){
    cit<-citation(pkgs[i])
    pkglist[[i]]<-toBibtex(cit)
}
writeLines(unlist(pkglist),pkgcitations )

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Land covers - LANDFIRE  US Insular Areas "Existing Vegetation Type"
#https://landfire.gov/insular_areas.php
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yaplc<-raster("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF2016_Micronesia_200_IA/LF2016_FSM_YAP_200_IA/LF2016_EVT_200_YAP/Tif/LY16_EVT_200.tif")
Palaulc<-raster("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF2016_PW_200_IA/LF2016_EVT_200_PW/Tif/LP16_EVT_200.tif")
Chklc<-raster("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF2016_Micronesia_200_IA/LF2016_FSM_CHK_200_IA/LF2016_EVT_200_CHK/Tif/LU16_EVT_200.tif")
Koslc<-raster("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF2016_Micronesia_200_IA/LF2016_FSM_KSA_200_IA/LF2016_EVT_200_KSA/Tif/LK16_EVT_200.tif")
Pohnlc<-raster("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF2016_Micronesia_200_IA/LF2016_FSM_PNI_200_IA/LF2016_EVT_200_PNI/Tif/LN16_EVT_200.tif")

#crop marianas islands 
Marianas<-raster("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF2016_GU_CNMI_200_IA/LF2016_EVT_200_GUCNMI/Tif/LG16_EVT_200.tif")

guamextent<-c(242070, 280000, 1464180 , 1511000)
saipanextent<-c(359000, 375000, 1668000, 1692000)
tinianextent<-c(345000, 360000, 1649000, 1671500)
rotaextent<-c(296000, 317000, 1560000, 1572000)


Guamlc<-crop(Marianas, guamextent)
plot(Guamlc)
Saipanlc<-crop(Marianas, saipanextent)
plot(Saipanlc)
Tinianlc<-crop(Marianas, tinianextent)
plot(Tinianlc)
Rotalc<-crop(Marianas, rotaextent)
plot(Rotalc)


#crop other regions to high islands

plot(Koslc) #GOOD

plot(Pohnlc) #crop ahnt atoll
pohnextent<-c(400000,431040,741900, 776280 )
plot(crop(Pohnlc,pohnextent))
Pohnlc<-crop(Pohnlc,pohnextent)

plot(Palaulc) #crop out Angaur, Kayangel, and (most) rock islands
palauextent<-c(440000, 465000, 800000, 866000)
plot(crop(Palaulc,palauextent))
Palaulc<-crop(Palaulc,palauextent)

plot(Chklc) #crop to high islands in lagoon
chkextent<-c(340000, 380000, 805080, 826000)
plot(crop(Chklc,chkextent))
Chklc<-crop(Chklc,chkextent)

plot(Yaplc) #GOOD

#LANDFIRE EVT codes linked to descriptive classes (https://landfire.gov/library_list.php?cat=1)

classesmaster<-read.csv("/Users/clayt/Temp_Data/Micronesia/2016_landfire/LF16_EVT_200_all_landfire_EVT_classes.csv")
micronesiaclasses<-unique(c(unique(Yaplc),unique(Palaulc), unique(Chklc), unique(Koslc), unique(Pohnlc), unique(Marianas)))
length(micronesiaclasses)

#Simplify 26 land cover classes to 9 classes for plotting/analyses:
#[1] "Other"                       "Shrubland/scrub"             "Other native forest"        
#[4] "Limestone forest"            "Mangrove"                    "Savanna"                    
#[7] "Agriculture"                 "Secondary dry woodland"      "Secondary forest/plantation"

classes<-classesmaster[classesmaster$VALUE %in% micronesiaclasses,c(1,2,9)]   
names(classes)<-c("VALUE", "EVT_Name","old_EVT")
classes$simple<-NA
classes[grep("Forest", classes$old_EVT),"simple"]<-"Other native forest"
classes[classes$EVT_Name=="Micronesian Swamp Forest","simple"]<-"Other native forest"
classes[grep("Shrubland", classes$EVT_Name),"simple"]<-"Shrubland/scrub"
classes[grep("Scrub", classes$EVT_Name),"simple"]<-"Shrubland/scrub"
classes[classes$EVT_Name=="Micronesian Dry Coastal Strand Herbaceous","simple"]<-"Shrubland/scrub"
classes[classes$EVT_Name=="Polynesian Ruderal Dry Woodland","simple"]<-"Shrubland/scrub"
classes[grep("Grassland", classes$old_EVT),"simple"]<-"Savanna"
classes[classes$EVT_Name=="Agriculture-Pasture and Hay","simple"]<-"Savanna"
classes[classes$EVT_Name=="Agriculture-Cultivated Crops and Irrigated Agriculture","simple"]<-"Agriculture"
#classes[grep("Agriculture", classes$EVT_Name),"simple"]<-"Agriculture"
classes[classes$EVT_Name=="Micronesian Lowland Limestone Forest","simple"]<-"Limestone forest"
classes[classes$EVT_Name=="Western Pacific Mangrove","simple"]<-"Mangrove"
classes[classes$EVT_Name=="Polynesian Ruderal Dry Woodland","simple"]<-"Secondary dry woodland"
classes[classes$EVT_Name=="Polynesian Ruderal Lowland Rainforest","simple"]<-"Secondary forest/plantation"
classes[classes$EVT_Name=="Tropical Pacific Forest Plantation","simple"]<-"Secondary forest/plantation"
#classes[classes$EVT_Name=="Tropical Pacific Forest Plantation","simple"]<-"Agroforest"
classes[,"simple"][is.na(classes[,"simple"])]<-"Other"
classes$simple_numeric<-as.numeric(factor(classes$simple))

unique(classes$simple)

aggregate(classes, simple_numeric~simple, FUN=mean)


#Reclassify land cover rasters to 9 classes 

islcode<-c("Yap", "Palau", "Guam", "Rota", "Tinian", "Saipan", "Chk", "Pohn", "Kos")
islandsimplelclist<-vector("list", length=length(islcode))

for(k in 1:length(islcode)){
  simplelc<-get(paste0(islcode[k],"lc"))
  simplelc[simplelc==-9999]<-NA
  simplelc[simplelc==7292]<-NA
  simplelc<-reclassify(simplelc, rcl=classes[,c(1,5)])
  islandsimplelclist[[k]]<-simplelc
}
names(islandsimplelclist)<-islcode

#land covers from islandsimplelclist above (for plotting below)
yapsimple<-islandsimplelclist[[1]]
palausimple<-islandsimplelclist[[2]]
guamsimple<-islandsimplelclist[[3]]
rotasimple<-islandsimplelclist[[4]]
tiniansimple<-islandsimplelclist[[5]]
saipansimple<-islandsimplelclist[[6]]
chuuksimple<-islandsimplelclist[[7]]
pohnpeisimple<-islandsimplelclist[[8]]
kosraesimple<-islandsimplelclist[[9]]


#Legend and color scheme to look at land covers

#color-blind freindly palettes (https://personal.sron.nl/~pault/)
Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

simplecolorscheme<-data.frame(classname=unique(classes$simple), classcode=unique(classes$simple_numeric))
simplecolorscheme<-simplecolorscheme[order(simplecolorscheme$classcode),]
simplecolorscheme$hexcol<-c(Tol_muted[8],Tol_muted[2],Tol_muted[4], "#8D9F9B", Tol_muted[3], Tol_muted[5],Tol_muted[7], Tol_muted[6],Tol_light[6])
pie(rep(1,9), col =simplecolorscheme$hexcol , labels=simplecolorscheme$classname)


#plot to check - note color code based on original simple lc maps in islandsimplelclist
par(mfrow=c(3,3))
for(n in 1:9){
  plot(islandsimplelclist[[n]], main=islcode[n], col=simplecolorscheme$hexcol[unique(islandsimplelclist[[n]])])
}


#calculate % covers : forest_perc savanna_perc mangrove_perc scrub_perc

forsavcov<-data.frame(isl=as.numeric(), forest_perc=as.numeric(), savanna_perc=as.numeric(), mangrove_perc=as.numeric(), scrub_perc=as.numeric(), secondary_perc=as.numeric())
for(p in 1:length(islcode)){
  covercount<-as.data.frame(freq(islandsimplelclist[[p]]))
  covercount$isl<-islcode[p]
  names(covercount)[1]<-"classcode"
  covercount<-merge(covercount,simplecolorscheme[,c(1,2)])
  covercount$forestsav<-NA
  forest_perc<-sum(covercount$count[grep("forest",covercount$classname)])/sum(covercount$count)*100
  savanna_perc<-covercount$count[covercount$classname=="Savanna"]/sum(covercount$count)*100
  mangrove_perc<-covercount$count[covercount$classname=="Mangrove"]/sum(covercount$count)*100
  mangrove_perc<-ifelse(length(mangrove_perc)==0,0,mangrove_perc)
  scrub_perc<-covercount$count[covercount$classname=="Shrubland/scrub"]/sum(covercount$count)*100
  scrub_perc<-ifelse(length(scrub_perc)==0,0,scrub_perc)
  secondary_perc<-sum(covercount$count[grep("Secondary",covercount$classname)])/sum(covercount$count)*100
  forsavcov[p,1]<-islcode[p]
  forsavcov[p,c(2:6)]<-c(round(forest_perc, 3),round(savanna_perc,3),round(mangrove_perc,3),round(scrub_perc,3), round(secondary_perc, 3))
}


forsavcov$allfor_perc<-forsavcov$forest_perc + forsavcov$mangrove_perc
forsavcov$isl[7:9]<-c("chuuk", "pohnpei", "kosrae")



#Determine "Forest Exposure" to savanna 
# real forest-savanna edge length vs edge length if savanna were a continguous square
# = actual forest edge length-minimum edge length)/minimum edge length the adjacency

#Reclassify land cover to forest, savanna or NA (9999)
#then use  adjancent() function to determine savanna pixels around each forest pixel to figure out edge length

simpleclasses<-aggregate(classes, simple_numeric~simple, FUN=mean)
simpleclasses$supersimple<-c(9999,1,1,9999, 1, 2,1,1,9999)

exposurevals<-data.frame(island=islcode, forestedgekm=NA, exposure=NA)

for(q in 1:9){
  lc<-islandsimplelclist[[q]]
  forsavlc<-reclassify(lc, rcl=simpleclasses[,c(2,3)])
  forsavlc[forsavlc==9999]<-NA
  adjacency<-adjacent(forsavlc, cells=1:ncell(forsavlc), directions=4, sorted=TRUE)
  adjacencydf<-data.frame(from=adjacency[,1], to=adjacency[,2])
  adjacencydf$lcfrom<-forsavlc[adjacencydf$from]
  adjacencydf$lcto<-forsavlc[adjacencydf$to]
  #count forest pixel edges adjacent to savanna
  forestedgekm<-nrow(adjacencydf[which(adjacencydf$lcfrom==1 & adjacencydf$lcto==2),])*30/1000
  #REFERENCE: perimeter of a circle made of all savanna pixels
  edgeminkm<-((sqrt((ncell(forsavlc[forsavlc==2])/pi)))*2*pi)*30/1000
  #Perimeter complexity/exposure = (observed perimeter-minimum perimeter)/minimum perimeter
  exposure<-(forestedgekm - edgeminkm)/edgeminkm
  exposurevals[q,2]<-forestedgekm
  exposurevals[q,3]<-exposure
}

exposurevals

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Island-scale statistics for plotting land cover, geography, climate, fire correlations 
#Data based on Table 1 in associated publication
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Island scale statistics [include both landfire 2010 (lf10) and landfire 2016 (lf2016) percent land covers]
allstats<-read.csv("/Users/clayt/Temp_Data/Micronesia/2020_micronesia_stats_per_island_fire_lc_rf.csv")

#create perc annual area burned variables for plotting (set NAs to 0s)
allstats$perc_burn_plot<-allstats$perc_burn.median
allstats$perc_burn_plot[c(1,3,5)]<-0
allstats$perc_burn_plotmax<-allstats$perc_burn.max
allstats$perc_burn_plotmax[c(1,3,5)]<-0
allstats$ha_burned_med<-allstats$acres_burned.median*.4047
allstats$ha_burned_med[c(1,3,5)]<-0

allstats$islandcaps<-c("Chuuk", "Guam", "Kosrae", "Palau", "Pohnpei", "Rota", "Saipan", "Tinian", "Yap")

##Plot islands in 'climate space' determined by mean annual rainfall and rainfall seasonality, size by median annual percent area burned

ggplot(allstats, aes(x= meanannualrf*10, y= Seasonality.Index, label=islandcaps, size=perc_burn_plot))+
  geom_point() +geom_text(size=2.9, hjust=-.35, vjust=0)+
  scale_y_continuous("Rainfall seasonality index")+
  scale_x_continuous("Mean annual rainfall (mm)", limits=c(500,2400))+
  labs(size="Med. annual\npercent area\nburned")+
  scale_size_continuous(breaks=c(0,.5,1,2), labels = c("No Data", 0.5, 1.0, 2.0))+
  theme_bw()
ggsave("/Users/clayt/Documents/Manuscripts/2021_Micronesia_fire/island_plots_by_climate_space.pdf", height=4, width=5)


##CORRELATION PLOTS 

#land cover geography -  island age, island size, human arrival

#select columns from allstats
names(allstats)
geostats<-as.data.frame(cor(allstats[, c(5,7,10, 46,45)]))
geocors<-as.matrix(geostats[c(5,4,3,1,2), c(5,4,3,1,2)])  #create matrix for corrplot after re-arranging df column/row order

pdf("/Users/clayt/Desktop/2023_USAPI_land_cover_correlates_geography_v2_humans.pdf", width=8, height=8)
#plot lower half of corrplot
colnames(geocors)<-c("", "", "", "", "")
rownames(geocors)<-c("Forest cover", "Savanna cover", "Island size", "Island age", "Human arrival")
corrplot(geocors, method="number", type="lower", tl.col="black", tl.srt=45, col=colorRampPalette(c("dark green", "dark grey", "purple"))(200))
#add upper half of corrplot
rownames(geocors)<-c("", "", "", "", "")
colnames(geocors)<-c("Forest cover", "Savanna cover", "Island size", "Island age", "Human arrival")
corrplot(geocors, method="ellipse", type="upper", tl.col="black", tl.srt=45, col=colorRampPalette(c("dark green", "dark grey", "purple"))(200),add=TRUE )
dev.off()

#land cover climate - rainfalls
rfstats<-as.data.frame(cor(allstats[, c(39, 41, 42, 46, 45)]))
rfcors<-as.matrix(rfstats[c(5,4,1,2,3), c(5,4,1,2,3)]) 

pdf("/Users/clayt/Desktop/2023_USAPI_land_cover_correlates_rainfall_v1.pdf", width=8, height=8)
#plot lower half of corrplot
colnames(rfcors)<-c("", "", "", "", "")
rownames(rfcors)<-c("Forest cover", "Savanna cover", "Mean annual RF", "Min monthly RF", "Monthly RF range")
corrplot(rfcors, method="number", type="lower", tl.col="black", tl.srt=45, col=colorRampPalette(c("dark green", "dark grey", "purple"))(200))
#plot upper half of corrplot
colnames(rfcors)<-c("Forest cover", "Savanna cover", "Mean annual RF", "Min monthly RF", "Monthly RF range")
rownames(rfcors)<-c("", "", "", "", "")
corrplot(rfcors, method="ellipse", type="upper", tl.col="black", tl.srt=45, col=colorRampPalette(c("dark green", "dark grey", "purple"))(200),add=TRUE )
dev.off()

## fire correlations (for 6 islands w data)
firestats<-as.data.frame(cor(allstats[c(2,4,6:9), c(22, 43, 46, 44, 9)]))
firecors<-as.matrix(firestats)

pdf("/Users/clayt/Desktop/2023_USAPI_fire_correlates_v3.pdf", width=8, height=8)
#plot lower half of corrplot
colnames(firecors)<-c("", "", "", "","")
rownames(firecors)<-c("Median % area burnt", "Median fire count", "Savanna cover", "RF Seasonality", "Population density")
corrplot(firecors, method="number", type="lower", tl.col="black", tl.srt=45, col=colorRampPalette(c("dark green", "dark grey", "purple"))(200))
#plot upper half of corrplot
colnames(firecors)<-c("Median % area burnt", "Median fire count", "Savanna cover", "RF Seasonality", "Population density")
rownames(firecors)<-c("", "", "", "","")
corrplot(firecors, method="ellipse", type="upper", tl.col="black", tl.srt=45, col=colorRampPalette(c("dark green", "dark grey", "purple"))(200),add=TRUE )
dev.off()


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##LIGHTNING ANALYSiS 
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Lightning Data https://ghrc.nsstc.nasa.gov/pub/lis/climatology/LIS/
#Albrecht, R., Goodman, S., Buechler, D., Blakeslee, R. & Christian, H. (2016) 
#LIS 0.1 Degree very high resolution gridded lightning climatology data collection. 
#Data sets available online [https://ghrc. nsstc. nasa. gov/pub/lis/climatology/LIS/] 
#from the NASA Global Hydrology Resource Center DAAC, Huntsville, Alabama, USA doi: http://dx. doi. org/10.5067/LIS/LIS/DATA306

lightningdens<-raster("/Users/clayt/Temp_Data/Global data/NASA global lightning 1998-2013/lis_vhrfc_1998_2013_v01.nc", varname="VHRFC_LIS_FRD")
plot(lightningdens)

#Extract lightning density to regions surrounding all pacific islanda and micronesia
#Pacific Island boundaries: https://earthworks.stanford.edu/catalog/stanford-dk240nj4773
stanpacisl_10<-read_sf("/Users/clayt/Downloads/stanford pacific island groupings 10mil","ne_10m_admin_0_pacific_groupings")
stanpacisl_10<-as(stanpacisl_10, "Spatial")

newext<-extent(134.57, 165.98,  0, 21.9997660861079)
microbounds<-crop(stanpacisl_10, newext)
plot(microbounds)

microlightning<-na.omit(getValues(mask(lightningdens,microbounds )))
pacisllightning<-na.omit(getValues(mask(lightningdens,stanpacisl_10 )))
globallightning<-na.omit(getValues(lightningdens))

#combine into data.frame
alllightningdata<-data.frame(lightning=c(microlightning, pacisllightning, globallightning), 
                             region=c(rep("Micronesia", length(microlightning)),
                                      rep("All Pacific Islands", length(pacisllightning)),
                                      rep("Global", length(globallightning))))

alllightningdata$region<-factor(alllightningdata$region, levels=c("Global", "All Pacific Islands", "Micronesia"))

#arcsin transform function for plotting
asnh <- trans_new("asnh",
                  function(x) asinh(x),
                  function(y) sinh(y),
                  domain=c(-Inf, Inf))

#subsample to reduce data set for plotting
subsamp<-data.frame(lightning=c(sample(globallightning, 500),sample(pacisllightning, 500), sample(microlightning, 500)), 
                    region=c(rep("Global", 500), rep("All Pacific Islands", 500), rep("Micronesia", 500)) )

subsamp$region<-factor(subsamp$region, levels=c("Global", "All Pacific Islands", "Micronesia"))


ggplot(subsamp, aes(x=region, y=lightning))+
  geom_boxplot()+
  scale_x_discrete("Region")+
  scale_y_continuous('Mean lightning density (flashes/sq.km/yr)', trans=asnh, breaks=c(0,1,2,4,10,40,200))+
  theme_bw()

ggsave("/Users/clayt/Documents/Manuscripts/2021_Micronesia_fire/lightning density pacific global.pdf", height=4, width=4)


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##SOILS ANALYSES - using National Resources Conservation Service Soils Maps
#https://gdg.sc.egov.usda.gov/
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##***Note - models below may produce slightly different results due to random sampling

#Soils (see bottom for soils data explore)
#Imports NRCS shapefile, soils data from chor "horizon" table; joins using "comp.txt" table

# from "Chorizon table grab these variables: c("hzname", "ph", "om", "sumbases", "ExtrAL", "pbray", "compkey", "horkey")
#("V1", V136", "V67" , "V133", "V151", "V157","V170", "V171")

dirs<-list.files("/Users/clayt/Temp_Data/NRCS_Pacific_islands_soil_survey_data/", full.names=TRUE)[c(2:6,16,18)]
soilslist<-vector("list", 7) #create a list with 7 empty slots
soilmaps<-vector("list", 7)
lclistnames<-c("kosrae", "pohnpei", "chuuk", "yap", "guam", "cnmi", "palau")

for(i in 1:7){
  dir<-dirs[i]
  spatialdir<-paste0(dir, "/spatial")
  tabledir<-paste0(dir, "/tabular")
  shapename<-file_path_sans_ext(list.files(spatialdir)[grep("mu_a", list.files(spatialdir))][1])
  mu_a<-read_sf(spatialdir, shapename)
  mu_a<-as(mu_a, "Spatial")
  #  unique(mu_a@data$MUKEY)
  chor<-read.table(paste0(tabledir,"/chorizon.txt"), sep="|")
  chortomerge<-chor[,c("V1","V136", "V67" , "V133", "V151", "V157", "V170", "V171")]  #adjust soil variables here
  names(chortomerge)<- c("hzname","ph", "om", "sumbases", "ExtrAL", "pbray", "compkey", "horkey")
  comp<-read.table(paste0(tabledir,"/comp.txt"), sep="|")
  comptaxonomy<-comp[,c("V84", "V85", "V87", "V108", "V109")]
  names(comptaxonomy)<-c("order", "suborder", "subgroup", "MUKEY", "compkey") 
  chor_key<-merge(chortomerge, comptaxonomy, by="compkey")
  chor_key$island<-lclistnames[i]
  soilslist[[i]]<-chor_key
  # chor_key<-chor_key[!duplicated(chor_key$V108),] - don't remove duplicates...
  map<-mu_a
  map@data$MUKEY<-as.numeric(map@data$MUKEY)
  #  soils@data<-merge(soils@data, chor_key, by.x="MUKEY", by.y="V108", all.x=TRUE)
  map@data$island<-unlist(strsplit(basename(dir), split="_"))[2]
  soilmaps[[i]]<-map
}

#break up cnmi into rota, saipan, tinian
rotasoils<-crop(soilmaps[[6]], projectRaster(islandsimplelclist[[4]], crs=crs(soilmaps[[6]])))
rotasoils$island<-"rota"
tiniansoils<-crop(soilmaps[[6]], projectRaster(islandsimplelclist[[5]], crs=crs(soilmaps[[6]])))
tiniansoils$island<-"tinian"
saipansoils<-crop(soilmaps[[6]], projectRaster(islandsimplelclist[[6]], crs=crs(soilmaps[[6]])))
saipansoils$island<-"saipan"

soilmaps<-soilmaps[-6]
soilmaps[[7]]<-rotasoils
soilmaps[[8]]<-tiniansoils
soilmaps[[9]]<-saipansoils

names(soilmaps)<-c("Kosrae", "Pohnpei", "Chuuk", "Yap", "Guam", "Palau","Rota", "Tinian", "Saipan")
names(soilslist)<-c("Kosrae", "Pohnpei", "Chuuk", "Yap", "Guam", "Cnmi", "Palau")


#use land covers from islandsimplelclist above to sample savanna and forest (NOT MANGROVE, NOT LIMESTONE)
# 9 land cover classes:
aggregate(classes, simple_numeric~simple, FUN=mean)
#                   simple        simple_numeric
# 1                 Agriculture              1
# 2            Limestone forest              2
# 3                    Mangrove              3
# 4                       Other              4
# 5         Other native forest              5
# 6                     Savanna              6
# 7      Secondary dry woodland              7
# 8 Secondary forest/plantation              8
# 9             Shrubland/scrub              9

islcodesoils<-c("Yap", "Palau", "Guam", "Rota", "Tinian", "Saipan", "Chuuk", "Pohnpei", "Kosrae")

##random samples savanna/forest

samplelist<-vector("list", 9)

for(j in 1:9){
  lc<-islandsimplelclist[[j]]
  randoms<-sampleRandom(lc, 10000, xy=T, package="raster")
  randoms<-as.data.frame(randoms)
  names(randoms)[3]<-"EVT_NAME"
  randoms$island<-islcodesoils[j]
  savsamp<-randoms[randoms$EVT_NAME==6,]      #savanna land cover code
  forsavsamp<-randoms[randoms$EVT_NAME==5 | randoms$EVT_NAME==2,]   #other forest and limestone forest land cover code
  endsamp<-rbind(savsamp,forsavsamp[sample(nrow(forsavsamp), 1000), ] )
  coordinates(endsamp)<-~x+y
  crs(endsamp)<-crs(lc)
  samplelist[[j]]<-endsamp
}

samplelist

#rasterize soil polygons based on land covers and extract soil map layer ID - MUKEY 

names(soilslist[[1]])
soilsamps<-matrix(ncol=ncol(soilslist[[1]])+3, nrow=0)
soilsamps<-data.frame(soilsamps)
names(soilsamps)<- c("MUKEY", "x", "y","VALUE_1", "island", names(soilslist[[1]])[1:11])

for(k in 1:9){
  soilrast<-rasterize(spTransform(soilmaps[[islcodesoils[k]]], CRS=crs(islandsimplelclist[[k]])), islandsimplelclist[[k]], "MUKEY")
  samplelist[[k]]$MUKEY<-extract(soilrast,samplelist[[k]])
  if(islcodesoils[k]=="Rota"|islcodesoils[k]=="Tinian"|islcodesoils[k]=="Saipan"){
    sample<-merge(as.data.frame(samplelist[[k]]),soilslist[["Cnmi"]],by="MUKEY")
  }else{
    sample<-merge(as.data.frame(samplelist[[k]]),soilslist[[islcodesoils[k]]],by="MUKEY")
  }
  sample<-sample[,1:16]
  names(sample)[5]<-"island"
  soilsamps<-rbind(soilsamps,sample)
}
names(soilsamps)
nrow(soilsamps[soilsamps$island=="Kosrae" & soilsamps$EVT_NAME==6,])

#Create cover label var
soilsamps$cover<-ifelse(soilsamps$EVT_NAME==6,"Savanna", "Forest")

#Surface soils - "H1" all Os and A Horizon types

surfacerows<-c(grep("O",soilsamps$hzname), grep("A",soilsamps$hzname))
surface<-soilsamps[surfacerows,]
surface<-rbind(surface, soilsamps[soilsamps$hzname=="H1",])
unique(surface$hzname)


#create single var data.frames for soil ph, organic matter, sum of extrable bases and extractable Al
#remove NAs AND remove duplicate locale samples

#Soil pH
surfph<-surface[,c(2,3,5,8,17,14)]
names(surfph)
surfph<-surfph[complete.cases(surfph),]
surfph$unique<-paste(surfph$x, surfph$y, sep="_")
surfph<-distinct(surfph, unique, .keep_all=TRUE)

#subsample for model fitting - based on min savanna sample size (kosrae)
nrow(surfph[surfph$island=="Kosrae" & surfph$cover=="Savanna",])
phsamplesize<-nrow(surfph[surfph$island=="Kosrae" & surfph$cover=="Savanna",])

table(surfph[,c(3,5)])
surfph <- surfph %>% group_by(island,cover) %>% slice_sample(n=phsamplesize)

##MODEL FITING
#Using gam() function in mgcv to fit linear models with spatial autocorrelation

hist(surfph$ph)
mph<-gam(ph~cover*island,correlation=corAR1(form=~x+y|island) , data=surfph)
summary(mph)
hist(resid(mph)) #normal residuals
mph1<-gam(ph~cover+island,correlation=corAR1(form=~x+y|island) , data=surfph)
mph2<-gam(ph~cover,correlation=corAR1(form=~x+y|island) , data=surfph)
mph3<-gam(ph~island,correlation=corAR1(form=~x+y|island) , data=surfph)
mphnull<-gam(ph~1,correlation=corAR1(form=~x+y|island) , data=surfph)

AIC(mph, mph1, mph2, mph3, mphnull)
Weights(AIC(mph, mph1, mph2, mph3, mphnull))
1-deviance(mph)/deviance(mphnull)

#Model predictions

##STANDARD ERRORS ARE THE SAME FOR ALL COEFFICIENTS - because equal sample sizes

predictdfph<-data.frame(island=rep(unique(surfph$island), 2), cover=c(rep("Forest", length(unique(surfph$island))),rep("Savanna", length(unique(surfph$island)))))
predictdfph$prediction<-predict(mph,predictdfph, se.fit=TRUE )$fit
predictdfph$se<-predict(mph,predictdfph, se.fit=TRUE )$se.fit
#no transformation
predictdfph$upperci<-predictdfph$prediction+(predictdfph$se*1.96)
predictdfph$lowerci<-predictdfph$prediction-(predictdfph$se*1.96)

#plot model predictions
pd <- position_dodge(0.4)
phfig<-ggplot(predictdfph, aes(x=island, y=prediction, colour=cover)) + 
  geom_errorbar(aes(ymin=lowerci, ymax=upperci), width=0, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  #  labs(colour="Land\nCover")+
  scale_x_discrete("")+
  scale_y_continuous("Soil pH")+
  annotate("label", x = "Kosrae", y = 7.1, label = "Expl Dev. = 58.7%\nAkaike Wt > 0.99", size=3)+
  theme_bw()+
  scale_colour_grey(labels=c("Forest", "Savanna"))+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))


#Soil Organic Matter
surfom<-surface[,c(2,3,5,9,17,14)]
surfom<-surfom[complete.cases(surfom),]
names(surfom)
table(surfom[,c(3,5)])
surfom$unique<-paste(surfom$x, surfom$y, sep="_")
surfom<-distinct(surfom, unique, .keep_all=TRUE)

#subsample for model fitting - based on min savanna sample size (kosrae)
nrow(surfom[surfom$island=="Kosrae" & surfom$cover=="Savanna",])
omsamplesize<-nrow(surfom[surfom$island=="Kosrae" & surfom$cover=="Savanna",])

table(surfom[,c(3,5)])
surfom <- surfom %>% group_by(island,cover) %>% slice_sample(n=omsamplesize)

##MODEL FITING
#Using gam() function in mgcv to fit linear models with spatial autocorrelation
hist(surfom$om)
hist(surfom$om^.25)
mom<-gam(om^.25~cover*island,correlation=corAR1(form=~x+y|island) , data=surfom)
summary(mom)
hist(resid(mom)) #normal residuals
mom1<-gam(om^.25~cover+island,correlation=corAR1(form=~x+y|island) , data=surfom)
mom2<-gam(om^.25~cover,correlation=corAR1(form=~x+y|island) , data=surfom)
mom3<-gam(om^.25~island,correlation=corAR1(form=~x+y|island) , data=surfom)
momnull<-gam(om^.25~1,correlation=corAR1(form=~x+y|island) , data=surfom)

AIC(mom, mom1, mom2, mom3, momnull)
Weights(AIC(mom, mom1, mom2, mom3, momnull))
1-deviance(mom)/deviance(momnull)

#model predictions
predictdfom<-data.frame(island=rep(unique(surfom$island), 2), cover=c(rep("Forest", length(unique(surfom$island))),rep("Savanna", length(unique(surfom$island)))))
predictdfom$prediction<-predict(mom,predictdfom, se.fit=TRUE )$fit
predictdfom$se<-predict(mom,predictdfph, se.fit=TRUE )$se.fit
#back-transform
predictdfom$predictiontrue<-predictdfom$prediction^(1/.25)
predictdfom$upperci<-(predictdfom$prediction+(predictdfom$se*1.96))^(1/.25)
predictdfom$lowerci<-(predictdfom$prediction-(predictdfom$se*1.96))^(1/.25)

#Plot model results

pd <- position_dodge(0.4)
omfig<-ggplot(predictdfom, aes(x=island, y=predictiontrue, colour=cover)) + 
  geom_errorbar(aes(ymin=lowerci, ymax=upperci), width=0, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  labs(colour="Land\nCover")+
  scale_x_discrete("")+
  scale_y_continuous("Soil organic matter (%)")+
  annotate("label", x = "Saipan", y = 50, label = "Expl Dev. = 51.4%\nAkaike Wt > 0.99", size=3)+
  theme_bw()+
  scale_colour_grey(labels=c("Forest", "Savanna"))+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

#Sum of extrable bases

surfbases<-surface[,c(2,3,5,10,17,14)]
surfbases<-surfbases[complete.cases(surfbases),]
names(surfbases)

#subsample for model fitting - based on min savanna sample size (kosrae)
table(surfbases[,c(3,5)])
#ditch Kosrae, Pohnpei

surfbases$unique<-paste(surfbases$x, surfbases$y, sep="_")
surfbases<-distinct(surfbases, unique, .keep_all=TRUE)
table(surfbases[,c(3,5)])

#ditch Kosrae and pohnpei and rota (too small sample sizes)
surfbases<-surfbases[!surfbases$island=="Kosrae", ]
surfbases<-surfbases[!surfbases$island=="Pohnpei", ]
surfbases<-surfbases[!surfbases$island=="Rota", ]

#check others
boxplot(surfbases$sumbases[surfbases$island=="Saipan"]~surfbases$cover[surfbases$island=="Saipan"])
boxplot(surfbases$sumbases[surfbases$island=="Tinian"]~surfbases$cover[surfbases$island=="Tinian"])
boxplot(surfbases$sumbases[surfbases$island=="Chuuk"]~surfbases$cover[surfbases$island=="Chuuk"])


#Single, same values for Saipan, Rota, Tinian; 3, same values for Chuuk
surfbases<-surfbases[!surfbases$island=="Saipan", ]
surfbases<-surfbases[!surfbases$island=="Tinian", ]
surfbases<-surfbases[!surfbases$island=="Chuuk", ]

#subsample  298f - min sample size for Guam forest Guam/Palau
unique(surfbases$island)
table(surfbases[,c(3,5)])

surfbases <- surfbases %>% group_by(island,cover) %>% slice_sample(n=200)

nrow(surfbases[surfbases$sumbases==max(surfbases$sumbases),])


##MODEL FITING
#Using gam() function in mgcv to fit linear models with spatial autocorrelation

hist(surfbases$sumbases)
hist((surfbases$sumbases)^.65)

msumbases<-gam(sumbases^.65~cover*island,correlation=corAR1(form=~x+y|island) , data=surfbases)
hist(resid(msumbases)) #normal residuals
summary(msumbases)
msumbases1<-gam(sumbases^.65~cover+island,correlation=corAR1(form=~x+y|island) , data=surfbases)
msumbases2<-gam(sumbases^.65~cover,correlation=corAR1(form=~x+y|island) , data=surfbases)
msumbases3<-gam(sumbases^.65~island,correlation=corAR1(form=~x+y|island) , data=surfbases)
msumbasesnull<-gam(sumbases^.65~1,correlation=corAR1(form=~x+y|island) , data=surfbases)

AIC(msumbases, msumbases1, msumbases2, msumbases3, msumbasesnull)
Weights(AIC(msumbases, msumbases1, msumbases2, msumbases3, msumbasesnull))
1-deviance(msumbases3)/deviance(msumbasesnull)


#model predictions
predictdfbases<-data.frame(island=rep(unique(surfbases$island), 2), cover=c(rep("Forest", length(unique(surfbases$island))),rep("Savanna", length(unique(surfbases$island)))))
predictdfbases$prediction<-predict(msumbases,predictdfbases, se.fit=TRUE )$fit
predictdfbases$se<-predict(msumbases,predictdfbases, se.fit=TRUE )$se.fit
#back-transform
predictdfbases$predictiontrue<-predictdfbases$prediction^(1/.65)
predictdfbases$upperci<-(predictdfbases$prediction+(predictdfbases$se*1.96))^(1/.65)
predictdfbases$lowerci<-(predictdfbases$prediction-(predictdfbases$se*1.96))^(1/.65)


#plot model results
pd <- position_dodge(0.2)
basesfig<-ggplot(predictdfbases, aes(x=island, y=predictiontrue, colour=cover)) + 
  geom_errorbar(aes(ymin=lowerci, ymax=upperci), width=0, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  labs(colour="Land\nCover")+
  scale_x_discrete("")+
  scale_y_continuous("Sum of extractable bases (cmol/kg)")+
  annotate("label", x = "Guam", y = 80, label = "Expl Dev. = 37.0%\nAkaike Wt = 0.87", size=3)+
  theme_bw()+
  scale_colour_grey(labels=c("Forest", "Savanna"))+
  theme(legend.position="none")



#Extractable Aluminum

surfAL<-surface[,c(2,3,5,11,17,14)]
surfAL<-surfAL[complete.cases(surfAL),]
names(surfAL)
surfAL$unique<-paste(surfAL$x, surfAL$y, sep="_")
surfAL<-distinct(surfAL, unique, .keep_all=TRUE)

#Check sample sizes/values and subsample
table(surfAL[,c(3,5)])

#ditch Kosrae, Pohnpei (too  few samples; no values for Chuuk)
surfAL<-surfAL[!surfAL$island=="pohnpei",]

boxplot(surfAL$ExtrAL[surfAL$island=="Saipan"]~surfAL$cover[surfAL$island=="Saipan"])
boxplot(surfAL$ExtrAL[surfAL$island=="Tinian"]~surfAL$cover[surfAL$island=="Tinian"])
boxplot(surfAL$ExtrAL[surfAL$island=="Rota"]~surfAL$cover[surfAL$island=="Rota"])

#ditch CNMI - same single value per cover for rota, tinian, saipan
surfAL<-surfAL[surfAL$island=="Guam" | surfAL$island=="Palau",]
unique(surfAL$island)

surfAL <- surfAL %>% group_by(island,cover) %>% slice_sample(n=200)

#MODEL FITING
#Using gam() function in mgcv to fit linear models with spatial autocorrelation

hist(surfAL$ExtrAL)
hist(surfAL$ExtrAL^.3)

mExtrAL<-gam(ExtrAL^.3~cover*island,correlation=corAR1(form=~x+y|island) , data=surfAL)
hist(resid(mExtrAL)) #normal residuals
summary(mExtrAL)
mExtrAL1<-gam(ExtrAL^.3~cover+island,correlation=corAR1(form=~x+y|island) , data=surfAL)
mExtrAL2<-gam(ExtrAL^.3~cover,correlation=corAR1(form=~x+y|island) , data=surfAL)
mExtrAL3<-gam(ExtrAL^.3~island,correlation=corAR1(form=~x+y|island) , data=surfAL)
mExtrALnull<-gam(ExtrAL^.3~1,correlation=corAR1(form=~x+y|island) , data=surfAL)

AIC(mExtrAL, mExtrAL1, mExtrAL2, mExtrAL3, mExtrALnull)
Weights(AIC(mExtrAL, mExtrAL1, mExtrAL2, mExtrAL3, mExtrALnull))
1-deviance(mExtrAL1)/deviance(mExtrALnull)



#MOdel predictions
predictdfAL<-data.frame(island=rep(unique(surfAL$island), 2), cover=c(rep("Forest", length(unique(surfAL$island))),rep("Savanna", length(unique(surfAL$island)))))
predictdfAL$prediction<-predict(mExtrAL1,predictdfAL, se.fit=TRUE )$fit
predictdfAL$se<-predict(mExtrAL1,predictdfAL, se.fit=TRUE )$se.fit
#back-transform
predictdfAL$predictiontrue<-predictdfAL$prediction^(1/.3)
predictdfAL$upperci<-(predictdfAL$prediction+(predictdfAL$se*1.96))^(1/.3)
predictdfAL$lowerci<-(predictdfAL$prediction-(predictdfAL$se*1.96))^(1/.3)

#plot model results
pd <- position_dodge(0.2)
ALfig<-ggplot(predictdfAL, aes(x=island, y=predictiontrue, colour=cover)) + 
  geom_errorbar(aes(ymin=lowerci, ymax=upperci), width=0, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  labs(colour="Land Cover")+
  scale_y_continuous("Extractable aluminum (cmol/kg)", limits=c(0,1))+
  scale_x_discrete("Island", labels=c( "Guam", "Palau"))+
  annotate("label", x = "Guam", y = .2, label = "Expl Dev. = 15.5%\nAkaike Wt = 0.72", size=3)+
  theme_bw()+
  scale_colour_grey(labels=c("Forest", "Savanna"))+  
  theme(legend.position=c(.75, .7),legend.background = element_blank(),
        legend.box.background = element_rect(fill="white", colour = "black"))


ggarrange(phfig, omfig, basesfig, ALfig, ncol = 2, nrow = 2)
#ggarrange(phfig, omfig, basesfig, ALfig, ncol = 4, nrow = 1)

ggsave("/Users/clayt/Documents/Analyses/Micronesia/2023_Micronesia_soils_forest_savanna_ph_om_bases_all.pdf", height=6, width=6)

#SOIL TAXONOMY FIGURES/ANALYSIS

names(surface)
soiltax<-surface[,c(2,3,5,14,17)]
soiltax<-soiltax[complete.cases(soiltax),]
soiltax$unique<-paste(soiltax$x, soiltax$y, sep="_")
soiltax<-distinct(soiltax, unique, .keep_all=TRUE)

table(soiltax[,c(3,5)]) # look at sample sizes by land cover
table(soiltax[,c(3,4)])  # look at sample sizes by soil order

#one order cat is blank
#only 19 sites with ultisols on Palau

soiltax<-soiltax[!soiltax$order=="",]
soiltax<-soiltax[!soiltax$order=="Ultisols",]

soiltax$island<-factor(soiltax$island)

library(dplyr)
soiltax <- soiltax %>% group_by(island,cover) %>% slice_sample(n=92) #min sample size - Kosrae savannas

ggplot(soiltax, aes(x=order,group=cover, fill=cover))+
  geom_bar(position = 'dodge')+
  facet_wrap(.~island, ncol=3)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  scale_x_discrete("")+
  labs(fill="Land Cover")+
  scale_y_continuous("Count of samples")+
  scale_fill_grey(labels=c("Forest", "Savanna"))+
  theme(legend.position=c(.21, .905),legend.background = element_blank(),
        legend.box.background = element_rect(fill="white", colour = "black"))

ggsave("/Users/clayt/Documents/Analyses/Micronesia/2023_Micronesia_soil_orders_forest_savanna_byisland.pdf", width=5, height=6)


#simple chi-sq test
soilordertab<-table(soiltax$cover, soiltax$order)
chi.sqtest<-chisq.test(soilordertab)
print(chi.sqtest)

#Pearson's Chi-squared test
#data:  soilordertab
#X-squared = 80.86, df = 5, p-value = 0.0000000000000005546

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
##  FIRES AND LAND COVER INTERSECTIONS - #Attribute pixels burned per land cover type per island
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#Spatial fire perimeters - USFS Data 
#https://www.fs.usda.gov/rds/archive/catalog/RDS-2023-0012 - Guam, CNMI, Yap
#https://www.fs.usda.gov/rds/archive/catalog/RDS-2022-0039 - Babeldaob Palau
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``


allfires<-st_read("/Users/clayt/Temp_Data/Micronesia/", "2021_2011_USFS_Micronesia_fire_perimeters_all_simple.shp")
allfires<-as(allfires, "Spatial")

burnedlc<-data.frame(zone=as.numeric(), burned_pix_cnt=as.numeric(), year=as.numeric(), pixels_per_class=as.numeric(), tot_pix_burned=as.numeric(), class=as.numeric(), island=as.numeric())

#sets loop for island name and earliest year of fire history
loopref<-data.frame(islands=c("Yap", "Palau", "Saipan", "Tinian", "Rota", "Guam"),
                    yearmin=c(2016, 2012, 2016, 2016, 2016, 2015))

for(i in 1:nrow(loopref)){
  lclayer<-get(paste0(loopref[i,1],"lc"))
  pixel_cnts<-as.data.frame(freq(lclayer))
  names(pixel_cnts)<-c("VALUE", "pixels_per_class")
  pixel_cnts<-merge(pixel_cnts, classes[,c(1:2)], all.y=FALSE) 
  isldat<-data.frame(VALUE=as.numeric(),burned_pix_cnt=as.numeric(), year=as.numeric(), pixels_per_class=as.numeric(), tot_pix_burned=as.numeric())
  for(j in loopref[i,2]:2021){                        ## SET MAX YEAR
    fires<-allfires[allfires$island==loopref[i,1],]
    fireyr<-fires[fires@data$year==j,]
    fireyr<-spTransform(fireyr, crs(lclayer))
    lcburned<-exact_extract(lclayer, st_as_sf(fireyr))	#extract cells intersecting burned polygons
    burntcells<-do.call(rbind, lcburned)					#unlist features into single df
    burntperclass<-aggregate(coverage_fraction~value, burntcells, FUN=function(x) sum(x))		
    names(burntperclass)<-c("VALUE", "burned_pix_cnt")
    burntperclass$year<-j
    burntperclass<-merge(burntperclass, pixel_cnts, all.y=FALSE)
    burntperclass$tot_pix_burned<-sum(burntperclass$burned_pix_cnt)
    isldat<-rbind(isldat, burntperclass)
  }
  burnedlc<-rbind(burnedlc, cbind(isldat, rep(loopref[i,1], nrow(isldat))))
}

burnedlc$perc_burnt<-round(burnedlc$burned_pix_cnt/burnedlc$tot_pix_burned*100, 1)

head(burnedlc)
names(burnedlc)[7]<-"island"
tail(burnedlc)

write.csv(burnedlc, "/Users/clayt/Temp_Data/Micronesia/2023_landfire2016_2021_2016_burned_pixels_per_LC_per_yr_per_island.csv")

# # scripts to look at island-level fire stats
# burnedlctinian<-aggregate(perc_burnt~EVT_Name, burnedlc[burnedlc$island=="Tinian",], FUN=function(x)mean(x))
# burnedlcpalau<-aggregate(perc_burnt~EVT_Name, burnedlc[burnedlc$island=="Palau",], FUN=function(x)mean(x))
# burnedlcguam<-aggregate(perc_burnt~EVT_Name, burnedlc[burnedlc$island=="Guam",], FUN=function(x)mean(x))
# burnedlcrota<-aggregate(perc_burnt~EVT_Name, burnedlc[burnedlc$island=="Rota",], FUN=function(x)mean(x))
# burnedlcsaipan<-aggregate(perc_burnt~EVT_Name, burnedlc[burnedlc$island=="Saipan",], FUN=function(x)mean(x))
# burnedlcyap<-aggregate(perc_burnt~EVT_Name, burnedlc[burnedlc$island=="Yap",], FUN=function(x)mean(x))
# 
# lctinian<-aggregate(pixels_per_class~EVT_Name, burnedlc[burnedlc$island=="Tinian",], FUN=function(x)mean(x))
# lcpalau<-aggregate(pixels_per_class~EVT_Name, burnedlc[burnedlc$island=="Palau",], FUN=function(x)mean(x))
# lcguam<-aggregate(pixels_per_class~EVT_Name, burnedlc[burnedlc$island=="Guam",], FUN=function(x)mean(x))
# lcrota<-aggregate(pixels_per_class~EVT_Name, burnedlc[burnedlc$island=="Rota",], FUN=function(x)mean(x))
# lcsaipan<-aggregate(pixels_per_class~EVT_Name, burnedlc[burnedlc$island=="Saipan",], FUN=function(x)mean(x))
# lcyap<-aggregate(pixels_per_class~EVT_Name, burnedlc[burnedlc$island=="Yap",], FUN=function(x)mean(x))


#USe 'simplified' land cover (9 classes) to summarize fire-land cover intersections per year ) 
unique(classes$simple)

burnedlc<-merge(burnedlc, classes[,c(1,4)], by="VALUE", sort=FALSE)
head(burnedlc)
#summarize percent burned by new land cover classes 
simpletotals<-aggregate(perc_burnt~simple + island + year, burnedlc, FUN=function(x)sum(x))
head(simpletotals)
levels(factor(simpletotals$simple))
#Re-order factor levels for plotting
simpletotals$simple<-factor(simpletotals$simple, c("Other","Agriculture", "Mangrove", "Limestone forest",
                                                   "Other native forest","Secondary dry woodland",
                                                   "Secondary forest/plantation","Shrubland/scrub", "Savanna"))


#annual percent area burned distribution across land cover types

simpletotals$simple<-factor(simpletotals$simple, rev(c("Other","Agriculture", "Mangrove", "Limestone forest",
                                                       "Other native forest","Secondary dry woodland",
                                                       "Secondary forest/plantation","Shrubland/scrub", "Savanna")))

ggplot(simpletotals)+
  geom_jitter(data=simpletotals,size=.8, aes(y=perc_burnt, x=simple), position = position_jitter(width = 0.15))+
  facet_wrap(~island) +
  xlab("Land cover class")+  
  ylab("Percent of annual area burned")+
  # scale_y_continuous("Percent of annual area burned", limits=c(0,100))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("/Users/clayt/Documents/Analyses/Micronesia/2023_percburned_x_landcover_9LCCats_scatter_all_islands_BW_VERTICAL.pdf",   width=6,height=4)


#subset fires based on island-

islandnames<-unique(allfires$island)
Yapfires<-allfires[allfires$island=="Yap",]
Tinianfires<-allfires[allfires$island=="Tinian",]
Rotafires<-allfires[allfires$island=="Rota",]
Guamfires<-allfires[allfires$island=="Guam",]
Palaufires<-allfires[allfires$island=="Palau",] #& allfires$year>=2016),]
Saipanfires<-allfires[allfires$island=="Saipan",]


#create rasters of count of fires per pixel 
#   to map fire frequency and determine reburn percentages

firecountrasters<-vector("list", length=length(islandnames))

for(o in 1:length(islandnames)){
  firestack<-stack()
  islandfires<-get(paste0(islandnames[o], "fires"))
  islandlc<-get(paste0(islandnames[o], "lc"))
  for(p in 1:length(unique(islandfires$year))){
    fires<-islandfires[islandfires$year==unique(islandfires$year)[p],]
    fires<-spTransform(fires, CRS=crs(islandlc))
    firerast<-rasterize(fires,islandlc, 1)
    firerast[is.na(firerast)==TRUE]<-0
    firestack<-stack(firestack, firerast)
  }
  firecount<-calc(firestack, fun=sum)
  firecount[firecount==0]<-NA
  firecountrasters[[o]]<-firecount
}

islandnames

yapfirecount<-firecountrasters[[6]]
palaufirecount<-firecountrasters[[5]]
guamfirecount<-firecountrasters[[4]]
saipanfirecount<-firecountrasters[[1]]
tinianfirecount<-firecountrasters[[2]]
rotafirecount<-firecountrasters[[3]]

##Determine reburn percentage, or frequncy of area burned recurring in same pixels
reburn<-function(x){
  tab<-table(getValues(x))
  burn<-(sum(tab[2],tab[length(tab)]))/sum(tab)*100
  return(burn)
}

reburn(yapfirecount)
#[1] 9.407424
reburn(palaufirecount)
#[1] 17.6552
reburn(guamfirecount)
#[1] 25.5888
reburn(saipanfirecount)
#[1] 47.81327
reburn(rotafirecount)
#[1] 40.11367
reburn(tinianfirecount)
#[1] 26.24042


#Maps of land cover, with reburn frequency plotted for islands with fire records

#color scheme
simplecolorscheme
pie(rep(1,9), col =simplecolorscheme$hexcol , labels=simplecolorscheme$classname)

#land covers from islandsimplelclist above
yapsimple<-islandsimplelclist[[1]]
palausimple<-islandsimplelclist[[2]]
guamsimple<-islandsimplelclist[[3]]
rotasimple<-islandsimplelclist[[4]]
tiniansimple<-islandsimplelclist[[5]]
saipansimple<-islandsimplelclist[[6]]
chuuksimple<-islandsimplelclist[[7]]
pohnpeisimple<-islandsimplelclist[[8]]
kosraesimple<-islandsimplelclist[[9]]

pdf("/Users/clayt/Documents/Analyses/Micronesia/2023_USAPI_land_cover_9LCCLasses__pohn_kos_chuuk.pdf", width=8, height=8)

par(mfrow=c(4,4), mar=c(.3,.3,.3,.3))

plot(guamsimple, col=simplecolorscheme$hexcol[unique(guamsimple)], axes=FALSE, box=FALSE, legend=FALSE)
scalebar(10000, divs=1, type="bar", below="km" , xy=c(245000, 1500000), label=c("",10,""))
text(min(extent(guamsimple)[1:2])+10000, max(extent(guamsimple)[3:4])-3000, "Guam", cex=1.3)
#legend(260000, 1485000, simplecolorscheme$classname, pch=15, pt.cex=1.2, col=simplecolorscheme$hexcol,bty="n", cex=.8)
plot(guamsimple, col= "gray90", axes=FALSE, legend=FALSE, box=FALSE)
plot(guamfirecount, col=c("#004D40","#FFC107","#1E88E5","#D81B60"), add=TRUE, legend=FALSE)
text(min(extent(guamsimple)[1:2])+10000, max(extent(guamsimple)[3:4])-6000, "Fires 2015-2021", cex=1)
#legend(257000, 1485000, c("1", "2", "3", "4"), pch=15, pt.cex=1.2, ncol=2, col=c("#004D40","#FFC107","#1E88E5","#D81B60"),bty="n", title="Times burned", cex=.8)
#legend(241000+4000, 1515000-11000, c("1", "2", "3", "4+"), pch=15, pt.cex=1.2, ncol=2, col=c("#004D40","#FFC107","#1E88E5","#D81B60"),bty="n", title="Times burned", cex=.8)

plot(palausimple, col=simplecolorscheme$hexcol[unique(palausimple)], axes=FALSE, box=FALSE,legend=FALSE, xlim=c(440000, 461280), ylim=c(810000,857700))
scalebar(10000, divs=1, type="bar", below="km" , xy=c(456000, 816000), label=c("",10,""))
text(440000+6000, 857700-3000, "Babeldaob,", cex=1.3)
text(440000+6000, 857700-3000-5000, "Palau", cex=1.3)
#plot(palaufirecount, col=rainbow(5), add=TRUE, xlim=c(440000, 461280), ylim=c(80000,857700))
plot(palausimple, col= "gray90", axes=FALSE, legend=FALSE, box=FALSE, xlim=c(440000, 461280), ylim=c(810000,857700))
plot(palaufirecount, col=c("#004D40","#FFC107","#1E88E5","#D81B60"), add=TRUE, xlim=c(440000, 461280), ylim=c(810000,857700), legend=FALSE)
text(440000+6000, 857700-6000, "Fires 2012-2021", cex=1)

plot(yapsimple, col=simplecolorscheme$hexcol[unique(yapsimple)], axes=FALSE, legend=FALSE, box=FALSE)
text(min(extent(yapsimple)[1:2])+2000, max(extent(yapsimple)[3:4])-2000, "Yap", cex=1.3)
scalebar(5000, divs=1, type="bar", below="km" , xy=c(184000, 1049000), label=c("",5,""))
plot(yapsimple, col= "gray90", axes=FALSE, legend=FALSE, box=FALSE)
plot(yapfirecount, col=c("#004D40","#FFC107","#1E88E5","#D81B60"), add=TRUE, legend=FALSE)
text(min(extent(yapsimple)[1:2])+3500, max(extent(yapsimple)[3:4])-3000, "Fires 2016-2021", cex=1)

plot(saipansimple, col=simplecolorscheme$hexcol[unique(saipansimple)], axes=FALSE, box=FALSE, legend=FALSE)
text(min(extent(saipansimple)[1:2])+2000, max(extent(saipansimple)[3:4])-2000, "Saipan", cex=1.3)
scalebar(5000, divs=1, type="bar", below="km" , xy=c(368000, 1670000), label=c("",5,""))
plot(saipansimple, col= "gray90", axes=FALSE, legend=FALSE, box=FALSE)
plot(saipanfirecount, col=c("#004D40","#FFC107","#1E88E5","#D81B60"), add=TRUE, legend=FALSE)
text(min(extent(saipansimple)[1:2])+3000, max(extent(saipansimple)[3:4])-4000, "Fires 2016-2021", cex=1)

plot(rotasimple, col=simplecolorscheme$hexcol[unique(rotasimple)], axes=FALSE, box=FALSE, legend=FALSE)
text(min(extent(rotasimple)[1:2])+3000, max(extent(rotasimple)[3:4])-1000, "Rota", cex=1.3)
scalebar(5000, divs=1, type="bar", below="km" , xy=c(310000, 1560000), label=c("",5,""))
plot(rotasimple, col= "gray90", axes=FALSE, legend=FALSE, box=FALSE)
plot(rotafirecount, col=c("#004D40","#FFC107","#1E88E5","#D81B60"), add=TRUE, legend=FALSE)
text(min(extent(rotasimple)[1:2])+5000, max(extent(rotasimple)[3:4])-2000, "Fires 2016-2021", cex=1)

plot(tiniansimple, col=simplecolorscheme$hexcol[unique(tiniansimple)], axes=FALSE, box=FALSE, legend=FALSE)
text(min(extent(tiniansimple)[1:2])+2000, max(extent(tiniansimple)[3:4])-2000, "Tinian", cex=1.3)
scalebar(5000, divs=1, type="bar", below="km" , xy=c(346000, 1654000), label=c("",5,""))
plot(tiniansimple, col= "gray90", axes=FALSE, legend=FALSE, box=FALSE)
plot(tinianfirecount, col=c("#004D40","#FFC107","#1E88E5","#D81B60"), add=TRUE, legend=FALSE)
text(min(extent(tiniansimple)[1:2])+3000, max(extent(tiniansimple)[3:4])-2000, "Fires 2016-2021", cex=1)

plot(chuuksimple, col=simplecolorscheme$hexcol[unique(chuuksimple)], axes=FALSE, box=FALSE, legend=FALSE)
scalebar(10000, divs=1, type="bar", below="km" , xy=c(350000, 805000), label=c("",10,""))
text(min(extent(chuuksimple)[1:2])+10000, max(extent(chuuksimple)[3:4])-3000, "Chuuk", cex=1.3)

plot(pohnpeisimple, col=simplecolorscheme$hexcol[unique(pohnpeisimple)], axes=FALSE, box=FALSE, legend=FALSE)
scalebar(5000, divs=1, type="bar", below="km" , xy=c(410000, 747000), label=c("",5,""))
text(min(extent(pohnpeisimple)[1:2])+5000, max(extent(pohnpeisimple)[3:4])-2000, "Pohnpei", cex=1.3)

plot(kosraesimple, col=simplecolorscheme$hexcol[unique(kosraesimple)], axes=FALSE, box=FALSE, legend=FALSE)
scalebar(5000, divs=1, type="bar", below="km" , xy=c(270000, 580000), label=c("",5,""))
text(min(extent(kosraesimple)[1:2])+3000, max(extent(kosraesimple)[3:4])-2000, "Kosrae", cex=1.3)

#legend
plot(kosraesimple,col=NA, axes=FALSE, box=FALSE, legend=FALSE) 
legend(265500, 595050, simplecolorscheme$classname, pch=15, pt.cex=2, col=simplecolorscheme$hexcol,bty="n", cex=1, title="Land cover                       ")
legend(265740+10500, 595050, c("1", "2", "3", "4+"), pch=15, pt.cex=2, ncol=2, col=c("#004D40","#FFC107","#1E88E5","#D81B60"),bty="n", title="Times burned", cex=1)

dev.off()


