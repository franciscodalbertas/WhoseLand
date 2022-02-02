#---- combine gfw data ---------------------------------------------------------

# OBS:


# I'm doing firs on my local machine, but will have  to replace it by files 
# saved on our google drive.

# I wasn't able to download mining concessions. Still need to include

# testing to make a change and commiting. doint it again! Before commit I have
# to push! do it again. try again.

#---- packages -----------------------------------------------------------------

library(sf)
library(dplyr)
#library(rnaturalearth)
library(ggplot2)
library(maps)
library(viridis)


#---- files --------------------------------------------------------------------

path <- "D:\\whose_land\\global_forest_watch\\"

#### global oil and gas concessions ############################################

o_g <- st_read(paste0(path,"Oil_and_Gas_Concessions.shp"))


#### global oil palm concession #######################################################


o_p <- st_read(paste0(path,"Oil_Palm_Concessions.shp"))


#### Cambodja concession ########################################################


agriCamb <- st_read(paste0(path,"licadho-land-concessions-2020.shp"))

#---- selecting common columns -------------------------------------------------

#### editing names #############################################################

#names(o_p)[3] <- names(o_g)[3]

o_p$Concession <- "agriculture"

o_p$Concession <- "oil palm"


o_p$Concession <- "oil palm"

o_g$Concession <- "oil and gas"

names(o_g)[8] <- names(o_p)[5]


names(agriCamb)[7] <- "company"

agriCamb$Concession <- "agriculture"

#### separate timber and exclude class "none"

agriCamb$Concession[agriCamb$CROP=="timber"] <- "timber"

agriCamb <- agriCamb[!agriCamb$CROP=="none",]

common <- intersect(names(o_g), names(o_p)) 

common <- intersect(common,names(agriCamb))


common <- intersect(names(o_g), names(o_p)) 


#### subset common names #######################################################

o_g <- o_g   %>% select(common)
 
o_p <- o_p   %>% select(common)

agriCamb <- agriCamb  %>% select(common)

#--- merging data --------------------------------------------------------------

# multipolygon z doesn't bind well with multipolygon
# drop z:
o_g <- st_zm(o_g,drop = TRUE,what = "ZM")

df <- rbind(o_g,o_p,agriCamb)


#---- world data -----------------------------------------------------------------

data("wrld_simpl")

#world<-ne_countries(scale="large",returnclass="sf")
world <- map("world", plot=FALSE, fill=TRUE, col="gray") 
world_sf <- sf::st_as_sf(world)

##### subsetting land deals on land ############################################

# multipolygon z doesn't bind well with multipolygon
# drop z

o_g <- st_zm(o_g,drop = TRUE,what = "ZM")

#---- subsetting land deals on land -------------------------------------------

df <- st_make_valid(df)

df <- st_intersection(x = df,world_sf)

#---- plotting -----------------------------------------------------------------


world_map<-ggplot(data=world_sf)+geom_sf(colour=NA,fill="grey80")+
  geom_sf(data=df,aes(fill=Concession),colour = NA)+
  coord_sf(xlim=c(-120,150),ylim = c(-53, 65), expand = FALSE)+
  scale_fill_viridis_d()+
  theme_bw()
  


