
# https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, opemxlsx)

Defores = stack("Raster/Hansen_GFC-2019-v1.7_lossyear_10S_070W.tif")

Puert  = read_sf("SHP/Zona.shp")

Puerto  <- st_transform(Puert,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Puer_Mal <- st_read ("SHP/Puerto.shp") # Caragmos un shp de puerto maldonado
Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CP <- st_read ("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))


Defores_Puer   <- crop(Defores, Puerto)                           #   
Defores_Puer   <- Defores_Puer<- mask(Defores_Puer, Puerto)
plot(Defores_Puer)


Defores_tbl  <-  rasterToPoints(Defores_Puer)
Defores_df   <-  data.frame(Defores_tbl)
colnames(Defores_df) = c("x", "y", "Año")

Defores_df_2012= Defores_df%>%
  subset(Año<= 19 & Año> 0)  %>%
  mutate(Años = 2000 +Año)


colores<- c("red")

library(ggspatial)
library(elevatr)
elev = get_elev_raster(Puerto, z=12)

plot(elev)
Poligo_alt    <- crop(elev, Puerto)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Puerto)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores = c( 
"#fefae0", "#faedcd", "#d4a373"
  )#amarillo pastel

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

col<- c("red")

library(ggnewscale) 

Defores_df_2000 = Defores_df%>% subset(Año<= 1 & Año> 0)  %>% mutate(Años = 2000 +Año)
Defores_df_2002 = Defores_df%>% subset(Año<= 2 & Año> 1)  %>% mutate(Años = 2000 +Año)
Defores_df_2003 = Defores_df%>% subset(Año<= 3 & Año> 2)  %>% mutate(Años = 2000 +Año)
Defores_df_2004 = Defores_df%>% subset(Año<= 4 & Año> 3)  %>% mutate(Años = 2000 +Año)
Defores_df_2005 = Defores_df%>% subset(Año<= 5 & Año> 4)  %>% mutate(Años = 2000 +Año)
Defores_df_2006 = Defores_df%>% subset(Año<= 6 & Año> 5)  %>% mutate(Años = 2000 +Año)
#------------------


A=ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores , 
                       name='Elevacion \n(msnm)')+
  new_scale_fill()+
  geom_tile(data= Defores_df_2000, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = col)+
  geom_sf(data=Puerto  ,fill=NA,color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c( -69.2818,-69.10407), ylim = c(-12.66831,-12.50898),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#cce3de"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.28, y = -12.51, hjust = 0, vjust = 1, 
           label = "A",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -69.28, y = -12.66, hjust = 0, vjust = 0,
           label = "2001", fontface = 2,
           size = 5, family = "serif", color = "#35978f")




B=ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores , 
                       name='Elevacion \n(msnm)')+
  new_scale_fill()+
  geom_tile(data= Defores_df_2002, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = col)+
  geom_sf(data=Puerto  ,fill=NA,color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c( -69.2818,-69.10407), ylim = c(-12.66831,-12.50898),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#cce3de"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.28, y = -12.51, hjust = 0, vjust = 1, 
           label = "B",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -69.28, y = -12.66, hjust = 0, vjust = 0,
           label = "2002", fontface = 2,
           size = 5, family = "serif", color = "#35978f")


C=ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores , 
                       name='Elevacion \n(msnm)')+
  new_scale_fill()+
  geom_tile(data= Defores_df_2003, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = col)+
  geom_sf(data=Puerto  ,fill=NA,color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c( -69.2818,-69.10407), ylim = c(-12.66831,-12.50898),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#cce3de"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.28, y = -12.51, hjust = 0, vjust = 1, 
           label = "C",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -69.28, y = -12.66, hjust = 0, vjust = 0,
           label = "2003", fontface = 2,
           size = 5, family = "serif", color = "#35978f")


D=ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores , 
                       name='Elevacion \n(msnm)')+
  new_scale_fill()+
  geom_tile(data= Defores_df_2004, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = col)+
  geom_sf(data=Puerto  ,fill=NA,color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c( -69.2818,-69.10407), ylim = c(-12.66831,-12.50898),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#cce3de"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.28, y = -12.51, hjust = 0, vjust = 1, 
           label = "D",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -69.28, y = -12.66, hjust = 0, vjust = 0,
           label = "2004", fontface = 2,
           size = 5, family = "serif", color = "#35978f")



E=ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores , 
                       name='Elevacion \n(msnm)')+
  new_scale_fill()+
  geom_tile(data= Defores_df_2005, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = col)+
  geom_sf(data=Puerto  ,fill=NA,color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c( -69.2818,-69.10407), ylim = c(-12.66831,-12.50898),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#cce3de"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.28, y = -12.51, hjust = 0, vjust = 1, 
           label = "E",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -69.28, y = -12.66, hjust = 0, vjust = 0,
           label = "2005", fontface = 2,
           size = 5, family = "serif", color = "#35978f")


FF=ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores , 
                       name='Elevacion \n(msnm)')+
  new_scale_fill()+
  geom_tile(data= Defores_df_2006, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = col)+
  geom_sf(data=Puerto  ,fill=NA,color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c( -69.2818,-69.10407), ylim = c(-12.66831,-12.50898),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#cce3de"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.28, y = -12.51, hjust = 0, vjust = 1, 
           label = "F",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -69.28, y = -12.66, hjust = 0, vjust = 0,
           label = "2006", fontface = 2,
           size = 5, family = "serif", color = "#35978f")

#------------------------
library(cowplot)
Mapa=ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 30), expand = FALSE) +
  draw_plot(A  , width = 10, height = 10,x = 0, y = 20)+
  
  draw_plot(B  , width = 10, height = 10,x = 10, y = 20)+
  
  draw_plot(C  , width = 10, height = 10,x = 0, y = 10.5)+
  draw_plot(D  , width = 10, height = 10,x = 10, y = 10.5)+
  
  draw_plot(E  , width = 10, height = 10,x = 0, y = 1)+
  
  draw_plot(FF  , width = 10, height = 10,x = 10, y = 1)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Mapa ,"Mapa/Mapa deforestacion.png",units = "cm",width = 20, #alto
       height = 30, #ancho
       dpi=1200)

tbl = rasterToPoints(Defores_Puer, spatial = F)
tbl = as_tibble(tbl)
tbl = setNames(tbl, c("x", "y", "year"))
tbl = filter(tbl, year > 0)


summ = tbl %>%
  group_by(year)%>%
  summarise(count =n())%>%
  ungroup ()%>%
  mutate( year= 2000 + year)

summ = mutate(summ, meters =count *900, has = meters /10000)
summ = dplyr::select(summ, year,has)


library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# current verison
packageVersion("hrbrthemes")
## [1] '0.8.6'
update_geom_font_defaults(font_rc_light)

Estadis = ggplot(data = summ, aes(x=year, y=has)) +
  geom_col(fill= "#ce4257") +
  scale_y_comma(limits=c(0,1300)) +
  coord_flip() +
  labs(x="Periodo 2000 - 2019 (años)",
       y="Hectareas (ha)",
       title="Deforestacion entre 2000 - 2016 en la ciudad de Puerto Maldonado",
       subtitle="Resultados del análisis de series temporales de imágenes Landsat para \ncaracterizar la extensión y el cambio de los bosques \nCambio forestal global 2000-2019 ",
       caption="Gorky Florez  'gflorezc@unamad.edu.pe'") + 
  theme_ipsum_rc(grid="X")+
    geom_text(aes(label=paste0(round(has,1), "ha"), hjust=0, nudge_y=20) ,family="serif")+
    theme(plot.background = element_rect(fill = "white"),
           plot.subtitle = element_text(face = "italic", family="serif"),
           plot.caption = element_text(size = 10, hjust = 0.95, family="serif", face = "italic"))+
    scale_x_continuous(breaks = c(2001:2019)) 

Estadis

ggsave(plot=Estadis,"Mapa/Estadis.png",units = "cm",width = 25, #ancho
         height = 20, #alto
         dpi=1200)


































