#---- bivariate map ------------------------------------------------------------

path <- getwd()


#### raster with proportion of small field #####################################
library(raster)

field_size_prop <- raster(paste0(path,"/smallholders_5km.tif"))

#### classify 0s to Nas! #######################################################

field_size_prop_no0 <- reclassify(field_size_prop, cbind(0, NA),right=FALSE)

rm(field_size_prop)

#### open restoration suitability map ##########################################

suit <- raster(paste0(path,"/countries_restoration_mosaic.tif"))

# I dont think this is a good idea anymore!
#suit_no0 <- reclassify(suit, cbind(0, NA),right=FALSE)

#### crop ######################################################################

overlap <- intersect(extent(suit), extent(field_size_prop_no0))

suit_c <- crop(suit, overlap)
field_size_prop_c <- crop(field_size_prop_no0, overlap)

rm(field_size_prop_no0,suit_no0)

#### resample ######################################################

field_size_prop_res <- resample(x = field_size_prop_c,suit_c)

rm(field_size_prop_c)
#### mask ############################################################

field_size_prop_m <- mask(field_size_prop_res,suit_c)

rm(field_size_prop_res)

#### convert to df ##############################################

field_df <- as.data.frame(field_size_prop_m,xy=T)
suit_df <- as.data.frame(suit_c,xy=T)


#### combining layers ##############################################

#ate aqui foi super eficiente

names(suit_df)[3] <- "suit"
names(field_size_prop_m)

comb <- cbind(field_df,suit_df[,3])

names(comb)[4] <- "suit"

rm(field_size_prop_m,suit_c)
gc()

#### saving df #################################################################

write.csv(comb,"bivariate_country.csv",row.names = F)

#---- plotting -----------------------------------------------------------------


comb <- read.csv(paste0(path,"/bivariate_country.csv"))

# how plot using biscale package -- I think I can do with mapping points!
# https://slu-opengis.github.io/biscale/


# https://stackoverflow.com/questions/48572744/plot-a-bivariate-map-in-r

devtools::install_github("wmurphyrd/colorplaner")

require(colorplaner)
require(ggplot2)
library(ggthemes)

# depois q salvei como csv esta dando problema!

#---- script Phill -------------------------------------------------------------

##### run color_function from other script #####################################

#### plot ######################################################################

library(ggplot2)
require(colorplaner)

biv_map1<-ggplot()

# biv_map2<-biv_map1+geom_raster(data=comb,aes(x=x,y=y,fill=smallholders_5km,fill2=suit))+
#   scale_fill_colourplane(name = "",na.color = "NA",color_projection = col_func,
#   limits_y = c(0,1),limits=c(0,1),axis_title = "blablabla",
#   axis_title_y = "blablabla",
#   breaks = c(0,0.5,1),breaks_y = c(0,0.5,1),
#   hue1 = 1, hue2 = 0.62,
#   dark_grey = 0.5,
#   light_grey = 0.9)+
#   theme_bw()+coord_equal(ylim = c(-6500000,8500000))+
#   guides(fill = guide_colorplane(title.theme = element_text(size = 13),
#   label.theme = theme_gray(),
#   label.y.theme = theme_gray(),axis.ticks = element_blank()))+
#   theme(axis.line=element_blank(),panel.background=element_blank(),
#   panel.border=element_blank(),
#   panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),
#   axis.ticks = element_blank(),
#   legend.position=c(.1,.3),legend.key.size=unit(0.8,"cm"))


# check the collors to get nice result!

biv_map2<-biv_map1+geom_raster(data=comb,aes(x=x,y=y,fill=smallholders_5km,fill2=suit))+
  scale_fill_colourplane("",color_projection = "YUV",
                         na.color ="transparent",axis_title = "proportion smallholders",
  axis_title_y = "restoration priority",breaks = c(0,0.5,1),breaks_y = c(0,0.5,1))+
  theme_bw()+coord_equal(ylim = c(-6500000,8500000))+
  guides(fill = guide_colorplane(title.theme = element_text(size = 8),
  label.theme = theme_gray(),
  label.y.theme = theme_gray(),axis.ticks = element_blank()))+
  theme(axis.line=element_blank(),panel.background=element_blank(),
        panel.border=element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.ticks = element_blank(),
        legend.position=c(.12,.3),legend.key.size=unit(0.6,"cm"),axis.text.x=element_blank(),
        axis.text.y=element_blank())+ 
    labs(x = "",y="")
gc()
#### adding countries ##########################################################

## Create a SpatialLines object
library(maps)
library(maptools)

# for lines, replace fill=F and no col

# countries <- map("world", plot=FALSE, fill=TRUE, col="gray") 
# IDs <- sapply(strsplit(countries$names, ":"), function(x) x[1])
# # for lines replace for map2lines, no IDs
# countries <- map2SpatialPolygons(countries, proj4string = CRS("+proj=longlat"),
#                                  IDs=IDs)
data("wrld_simpl")


library(rgeos)

countries_moll<- spTransform(wrld_simpl, crs(suit))
#countries_moll_simp<-gSimplify(countries_moll,tol = 0.1,topologyPreserve = TRUE)#simplify coastline to speed up plotting
#countries_moll_sf <- sf::st_as_sf(countries_moll_simp)
#countries_moll_df<-SpatialLinesDataFrame(countries_moll_simp,wrld_simpl@data) 
#countries_df<-SpatialpolDataFrame(countries_moll,countries_moll@data)
countries_moll_df = as(countries_moll, "SpatialLinesDataFrame")

# biv_map3 <- biv_map2+
#   geom_sf(data=countries_moll_sf,fill="transparent", color="black", size=0.25)
# 

rm(countries_moll_sf,countries_moll,countries_moll_simp)
rm(field_df,teste)

biv_map3 <- biv_map2+
  geom_path(data=countries_moll_df,aes(x = long, y = lat, group = group),colour="black",size=0.1)


memory.limit(12000)

# tem q arrumar essas proporcos pra legenda ficar direito!

ggsave(filename = "bivariate_plot_countries.png",plot = biv_map3,width = 19,height = 10,units = 'cm')

gc()
#### invertendo ################################################################

# teste <- ggplot() +
#   geom_sf(data = countries_moll_sf, fill = "lightgray",
#         colour = "white", size = 0.2)+
#   geom_raster(data=comb,aes(x=x,y=y,fill=smallholders_5km,fill2=suit))+
#   scale_fill_colourplane("",na.color ="transparent",axis_title = "proportion smallholders",
#   axis_title_y = "restoration priority",na.value = NA)+
#   theme_bw()+coord_sf(ylim = c(-6500000,8500000))+
#   guides(fill = guide_colorplane(title.theme = element_text(size = 13),
#   label.theme = theme_gray(),label.y.theme = theme_gray(),axis.ticks = element_blank()))+
#   theme(axis.line=element_blank(),panel.background=element_blank(),
#         panel.border=element_rect(colour = "black", fill=NA, size=1),
#         panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         plot.background=element_blank(),
#         axis.ticks = element_blank(),
#         legend.position=c(.12,.3),legend.key.size=unit(0.8,"cm"),axis.text.x=element_blank(),
#         axis.text.y=element_blank())+ 
#   labs(x = "",y="")
# 
# 
# ### nao consigo deixar o raster transparente em valor NA, q deveria!
# 
# biv_map2 <- biv_map1+
#   geom_polygon(data=countries_moll, aes(x=long, y=lat, group=group), 
#                fill="lightgray", color="white", size=0.25)
# 
# biv_map3 <-  biv_map2+
#   geom_raster(data=comb,aes(x=x,y=y,fill=smallholders_5km,fill2=suit))+
#   scale_fill_colourplane("",na.color ="transparent")+
#   theme_bw()+coord_equal(ylim = c(-6500000,8500000))+
#   guides(fill = guide_colorplane(title.theme = element_text(size = 13),
#   label.theme = theme_gray(),
#   label.y.theme = theme_gray(),axis.ticks = element_blank()))+
#   theme(axis.line=element_blank(),panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),
#         axis.ticks = element_blank(),
#         legend.position=c(.1,.3),legend.key.size=unit(0.8,"cm"))
# 

