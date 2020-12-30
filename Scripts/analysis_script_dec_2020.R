library(sp) 
library(rgdal)
library(geosphere)
library(tidyverse)
library(raster)
library(sf)
library(RColorBrewer)
library(traitdataform)
library(dggridR)
library(ape)
library(phangorn)
library(phytools)
library(geomorph)
library(ggthemes)

library(here)

#construct a global grid with hexagons with centers 5000m appart
dggs   <- dgconstruct(area=5000, metric=TRUE, resround='nearest') 
##Resolution: 9, Area (km^2): 2591.40182758771, Spacing (km): 50.276890494384, CLS (km): 57.4411078487275


#load in a polygon outline of south america
shp2 <- raster::shapefile(here("Data", "SA_2", "S_Amer.shp"))
#and now crop the polygon to get rid of some of the small islands we dont care about
shp2 <- raster::crop(shp2, extent(-85,-34.78,-55.9,12.59))
# get the grid that overlaps with your polygon of south america
sa_grid   <- dgshptogrid(dggs, here("Data", "SA_2", "S_Amer.shp"))


#Plot South Africa's borders and the associated grid
p<- ggplot() + 
    geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_polygon(data=sa_grid,   aes(x=long, y=lat, group=group), fill="blue", alpha=0.4)   +
    geom_path   (data=sa_grid,   aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
    coord_equal()+
    xlab("Longitude")+
    ylab("Latitude")
p

#extract the coordinates that definte the center of each grid hexagon:
cellcenters   <- dgSEQNUM_to_GEO(dggs,as.numeric(unique(sa_grid$cell)))
cellcenterpts <- data.frame(long=cellcenters$lon_deg, lat=cellcenters$lat_deg, cell=(unique(sa_grid$cell)))
coordinates(cellcenterpts) <- ~ long + lat
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
# this is necessary because different types of maps have different ways of converting the 3D globe into 2D flat maps
proj4string(cellcenterpts) <- proj4string(shp2)

#filter your giant grid so that you get only the ones that are within the outline of south america and not in the ocean
result <- raster::intersect(cellcenterpts, shp2)
df<-as.data.frame(result)
my_grid<-sa_grid%>%filter(.,cell%in%df$cell)

#plot again
p<- ggplot() + 
    geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_polygon(data=my_grid,   aes(x=long, y=lat, group=group), fill="blue", alpha=0.4)   +
    geom_path   (data=my_grid,   aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
    coord_equal()
p

#download the worldlim climate data:
bioclim <- raster::getData("worldclim", var = "bio", res = 10)
points<-as_tibble(cellcenterpts)
#Extract the temperature for each sampling location.
temperature <- raster::extract(bioclim[[1]], points[,1:2])

 points<-points %>% mutate(.,temp=temperature)
 points2<-points%>%group_by(., cell)
 points2<-points2%>%dplyr::select(.,cell, temp)
 grid <- merge(my_grid,points2,by.x="cell",by.y="cell")
 
 #plot the temperature for each grid hex
 tempplot <- ggplot()+
     geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")+
     geom_polygon(data=grid,   aes(x=long, y=lat, group=group, fill=temp/10))+
     scale_fill_viridis_c(name="Temp in Degrees C")+
     coord_equal()+
     theme_bw()+
     xlab("Longitude")+
     ylab("Latitude")
 tempplot
 
 #Extract the precipitation for each sampling location.
 precip <- raster::extract(bioclim[[12]], points[,1:2])

 wet_precip <- raster::extract(bioclim[[13]], points[,1:2])

  
 min_temp <- raster::extract(bioclim[[6]], points[,1:2])
 points <- points %>% mutate(.,temp=temperature,precip=precip, wet_precip=wet_precip, min_temp=min_temp)
 points2 <- points %>% group_by(., cell)
 points2 <- points2 %>% dplyr::select(.,cell, temp,precip, wet_precip, min_temp)
 grid <- merge(my_grid,points2,by.x="cell",by.y="cell")
 
#### #load the vegetation index data

vegetation_data <- read_csv("~/Dropbox/_UCL/Students/Tegan Foister/Data_sharing/Primate_Ecometrics/Data/NDVI_grid.csv") 


points1<-points%>%mutate(.,temp=temperature,precip=precip, wet_precip=wet_precip, min_temp=min_temp)
points2<-points1%>%group_by(., cell)
points2<-points2%>%dplyr::select(.,cell, temp,precip,wet_precip,min_temp)
grid <- merge(my_grid,points2,by.x="cell",by.y="cell")%>%mutate(., Mean_veg=vegetation_data$Mean_veg)

### can maybe delete these next 3 lines?
#points2<-points2%>%mutate(cell=as.double(cell))
#vegdat1<--vegetation_data%>%dplyr::select(.,cell, Mean_veg)%>%unique()
#points2<-points2%>%left_join(., test1)

#plot it!
vegetation_map<-ggplot()  +
    geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_polygon(data=grid,   aes(x=long, y=lat, group=group, fill=Mean_veg))   +
    scale_fill_distiller(name="Vegetation Index",palette = "Greens",direction=1)+
    coord_equal()+
    theme_bw()+
    xlab("Longitude")+ ###
    ylab("Latitude")+
    theme(legend.position = c(0.7, 0.17),
        #legend.direction = "horizontal",
        legend.box.background = element_rect(color="black"))
vegetation_map

precip_map<-ggplot()  +
  geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
  geom_polygon(data=grid,   aes(x=long, y=lat, group=group, fill=precip))   +
  scale_fill_distiller(name="Mean Annual\nPrecipitation",palette = "Blues",direction=1)+
  coord_equal()+
  theme_bw()+
  xlab("Longitude")+ ###
  ylab("Latitude")+
  theme(legend.position = c(0.7, 0.17),
        #legend.direction = "horizontal",
        legend.box.background = element_rect(color="black"))
precip_map

temp_map<-ggplot()  +
  geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
  geom_polygon(data=grid,   aes(x=long, y=lat, group=group, fill=temp/10))   + #temp from Bioclim database is in degrees C * 10
  scale_fill_viridis_c(name="Mean Annual\nTemp",option = "magma")+
  coord_equal()+
  theme_bw()+
  xlab("Longitude")+ ###
  ylab("Latitude")+
  theme(legend.position = c(0.7, 0.17),
        #legend.direction = "horizontal",
        legend.box.background = element_rect(color="black"))
temp_map


#Compiling climate data  
climate1 <-  as_tibble(grid)

ggplot( data =climate1, aes((temp^(1/3)))) + geom_histogram()



######### Summarize phylogenies
posterior_trees <- read.nexus(here("Data", "500_trees_from_vertlife.nex"))
MCCtree <- maxCladeCred(posterior_trees)


#read in trait data 
traits <- read.csv(here("Data","Data_table_for_Species_Level.csv")) 
row.names(traits) <- traits$Alt_name
traits <- traits[MCCtree$tip.label,]
library(geiger)
name.check(MCCtree,traits)
#test IMI phylogenetic signal
phylosig.test <- phylosig(tree = MCCtree, x = traits$IMI,  method='lambda')
phylosig.test
#test IMI allometry
library(caper)
compdata<-comparative.data(MCCtree,as.data.frame(traits),names.col = "Alt_name")
lm1 <- lm(traits$IMI~log(traits$Body_Size_Mean))
pgls1 <- pgls(IMI~log(Body_Size_Mean), data = compdata, lambda = phylosig.test$lambda)


regression.plot <- ggplot(data=traits, aes(x=log(Body_Size_Mean),y=IMI,color=Family))+
  geom_point()+
  stat_smooth(method="lm") +
  theme_bw()+
  scale_color_colorblind()+
  theme(aspect.ratio = 1, legend.position="bottom")+
  labs(x="log(Body Size)")
#ggsave(filename = here("Figs","bodysizeregression.pdf"), regression.plot, device = cairo_pdf)
#ggsave(filename = here("Figs","bodysizeregression.tif"), regression.plot, device = "tiff")

#read in TERRESTIAL MAMMALS 
geography <- raster::shapefile(here("Data","TERRESTRIAL_MAMMALS","TERRESTRIAL_MAMMALS.shp"))

rownames(traits)<-traits$Scientific_Name 
traits$Scientific_Name
my.geography <- geography[geography$binomial %in% traits$Scientific_Name,]
my.geography <- geography[geography$binomial %in% gsub("_", " ",traits$Alt_name2),]

#mapping species locality data
sp <- SpatialPoints(climate1[,2:3], proj4string = CRS(proj4string(my.geography)))

o <- sp::over(x= sp, y = my.geography, returnList = TRUE)

#figure out which taxa are missing from occurance data:
speclist2 <- unlist(lapply(o, function(x) unique(x$binomial)))
sciname1 <- gsub("_", " ",traits$Alt_name2)
sciname2 <- traits$Scientific_Name
setdiff(sciname2, geography$binomial)
setdiff(sciname1, geography$binomial)
#write.csv(as.matrix(sort(unique(geography$binomial))), file = here("Data","shapefiletaxonomy.csv"))


richness <- unlist(lapply(o, function(x) length(- traits[x$binomial,"IMI"])))
ecometric_bodymass <- unlist(lapply(o, function(x) mean(traits[x$binomial,"Body_Size_Mean"], na.rm = T)))
ecometric_bodymass_sd <- unlist(lapply(o, function(x) sd(traits[x$binomial,"Body_Size_Mean"], na.rm = T)))
ecometric_imi <- unlist(lapply(o, function(x) mean(traits[x$binomial,"IMI"], na.rm = T)))
ecometric_imi_sd <- unlist(lapply(o, function(x) sd(traits[x$binomial,"IMI"], na.rm = T)))

grid2<-grid%>%mutate(., Stand_dev_limbs = ecometric_imi_sd)%>%mutate(., Mean_limbs = ecometric_imi) %>%mutate(., Ecomet_BM = ecometric_bodymass) %>% mutate(., Ecomet_BM_sd = ecometric_bodymass_sd)

Mean_imi_map<-ggplot()+
  geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")+
  geom_polygon(data=grid2,   aes(x=long, y=lat, group=group, fill=Mean_limbs))+
  scale_fill_viridis_c(name="Mean\nIMI")+
  coord_equal()+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position = c(0.7, 0.17),
        #legend.direction = "horizontal",
        legend.box.background = element_rect(color="black"))
Mean_imi_map

SD_imi_map<-ggplot()+
  geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")+
  geom_polygon(data=grid2,   aes(x=long, y=lat, group=group, fill=Stand_dev_limbs))+
  scale_fill_viridis_c(name="Standard\nDeviation\nIMI")+
  coord_equal()+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position = c(0.7, 0.17),
        #legend.direction = "horizontal",
        legend.box.background = element_rect(color="black"))
SD_imi_map

Mean_BM_map<-ggplot()+
  geom_polygon(data=shp2, aes(x=long, y=lat, group=group), fill=NA, color="black")+
  geom_polygon(data=grid2,   aes(x=long, y=lat, group=group, fill=Ecomet_BM))+
  scale_fill_viridis_c(name="Mean\nBody Mass")+
  coord_equal()+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position = c(0.7, 0.17),
        #legend.direction = "horizontal",
        legend.box.background = element_rect(color="black"))
Mean_BM_map

library(patchwork)
(temp_map | vegetation_map)/(Mean_imi_map | Mean_BM_map)
#ggsave(filename = here("Figs","maps.pdf"),  device = cairo_pdf, width=8.5*1.25, height=11*1.25, units = 'in')
#ggsave(filename = here("Figs","maps.tif"),  device = "tiff", width=8.5*1.25, height=11*1.25, units = 'in')

climate1<-as_tibble(grid2)

Model the relationship between the environmental variables and 


model_mass_veg <- lm(pull(climate1[richness > 4,"Mean_veg"]) ~ ecometric_bodymass[richness > 4])
summary(model_mass_veg)
model_imi_veg <- lm(pull(climate1[richness > 4,"Mean_veg"]) ~ ecometric_imi[richness > 4])
summary(model_imi_veg)

model_mass_temp <- lm(pull(climate1[richness > 4,"temp"]) ~ ecometric_bodymass[richness > 4])
summary(model_mass_temp)
model_imi_temp <- lm(pull(climate1[richness > 4,"temp"]) ~ ecometric_imi[richness > 4])
summary(model_imi_temp)

model_mass_min_temp <- lm(pull(climate1[richness > 4,"min_temp"]) ~ ecometric_bodymass[richness > 4])
summary(model_mass_min_temp)
model_imi_min_temp <- lm(pull(climate1[richness > 4,"min_temp"]) ~ ecometric_imi[richness > 4])
summary(model_imi_min_temp)

model_mass_precip <- lm(pull(climate1[richness > 4,"precip"]) ~ ecometric_bodymass[richness > 4])
summary(model_mass_precip)
model_imi_precip <- lm(pull(climate1[richness > 4,"precip"]) ~ ecometric_imi[richness > 4])
summary(model_imi_precip)

model_mass_wet_precip <- lm(pull(climate1[richness > 4,"wet_precip"]) ~ ecometric_bodymass[richness > 4])
summary(model_mass_wet_precip)
model_imi_wet_precip <- lm(pull(climate1[richness > 4,"wet_precip"]) ~ ecometric_imi[richness > 4])
summary(model_imi_wet_precip)

regression_results <- bind_rows(
  glance(model_mass_veg),
  glance(model_imi_veg),
  glance(model_mass_temp),
  glance(model_imi_temp),
  glance(model_mass_min_temp),
  glance(model_imi_min_temp),
  glance(model_mass_precip),
  glance(model_imi_precip),
  glance(model_mass_wet_precip),
  glance(model_imi_wet_precip)
)

model_list <- c(
  "model_mass_veg",
  "model_imi_veg",
  "model_mass_temp",
  "model_imi_temp",
  "model_mass_min_temp",
  "model_imi_min_temp",
  "model_mass_precip",
  "model_imi_precip",
  "model_mass_wet_precip",
  "model_imi_wet_precip"
)

regression_results <- regression_results %>% mutate(model = model_list, .before = r.squared)
  

write.csv(regression_results, file = here("Data","regression_results.csv"))



sd_ecometric_bodymass <- unlist(lapply(o, function(x) sd(traits[x$binomial,"Body_Size_Mean"], na.rm = T)))

#make bins
mveg <- range(climate1$Ecomet_BM, na.rm = T)
sdveg <- range(climate1$Ecomet_BM_sd, na.rm = T) 
mbrks <- seq(mveg[1], mveg[2], diff(mveg)/25)
sdbrks <- seq(sdveg[1], sdveg[2], diff(sdveg)/25) 
mbc <- .bincode(climate1$Ecomet_BM, breaks = mbrks)
sdbc <- .bincode(climate1$Ecomet_BM_sd, breaks = sdbrks)

obj <- array(NA,dim = c(25,25))
for(i in 1:25){ for(j in 1:25){
  dat <- round(climate1$Mean_veg[which(mbc==i & sdbc==j)])
obj[26 - j,i] <- mean(dat, na.rm = T) } }

r<- raster(extent(0,25,0,25), resolution = 1)
#set the values to the obj
r<- setValues(r,obj)

#make the NDVI square plot
plot(1:25, 1:25, type = "n", xlim = c(1,25), ylim = c (1,25), xaxs = "i", yaxs = "i", asp = 1, axes = F, xlab ="",
     ylab = "") 
rect(0, 1, 25, 25, lwd = 3)  
#need to change the colours 
plot(r,colours = rev(terrain.colors(10))(round(maxValue(r) - minValue(r))), add = T)
rect(11, 9, 12, 10, lwd = 4)
  
#grab data from box 
dat <- round (veg[which(mbc==12 & sdbc==10)]) 
 
##anomaly plots 
modmax <- array(data=NA, dim=length(points.veg[,1])) 
mod <- list() 
for (i in 1:length(points.veg[,1])) {
  dat <- round(climate$Mean_veg[which(mbc==mbc[i]& sdbc==sdbc[i])]/10000) 
  mod[[i]] <- density(dat,bw=1) 
  modmax[i]<- mod[[i]]$x[which.max(mod[[i]]$y)]
}  ##Error in density.default(dat, bw = 1) : 'x' contains missing values
modmax <- round(modmax*10000) 
 
cutoff <- 1 



