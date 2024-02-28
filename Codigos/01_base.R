# los ultimos paquetes en llamarse tienen prioridad
if (!require("pacman")) install.packages("pacman")
pacman::p_load("igraph","rgdal","raster","tidyverse","lubridate","mapview",
               "leaflet", "htmltools","ggpubr", "ggplot2","fmsb", "Cairo",
               "sf", "lwgeom", "webshot","leaflet.extras","leafpop",
               "dbscan","geosphere","magick")

Alc <- readOGR("SHP/poligonos_alcaldias_cdmx.shp",
               use_iconv = TRUE,encoding = "UTF-8")
CT <- readOGR("SHP/coordinaciones.shp",
              use_iconv = TRUE,encoding = "UTF-8")


dir.base <- "Base/carpetas_2019-2021.csv"

base<-read.csv(dir.base,stringsAsFactors = F,fileEncoding = "UTF-8") %>%
  filter(ao_hechos == 2019,
         !(is.na(latitud)),
         !(is.na(longitud)),
         latitud != 0,
         longitud != 0) %>%
  mutate(fecha_hechos = ymd(fecha_hechos)) %>% 
  select(ao_hechos,mes_hechos,fecha_hechos,hora_hechos,delito,categoria_delito,
         alcaldia_hechos,longitud,latitud)

#primero hay que homologar y después filtrar los delitos

dir.diccionario <- "Diccionario/Diccionario delitos.csv"
diccionario <- read.csv(dir.diccionario,stringsAsFactors = F,fileEncoding = "WINDOWS-1252")
base.homol <- merge(base,diccionario,by.x = "delito", by.y = "MODALIDAD")



cambio_coo <- function (base){
  MEGA <- base
  if (nrow(MEGA)>0) {
    coordinates(MEGA)<- ~ longitud + latitud
    proj4string(MEGA)<-CRS("+proj=longlat +datum=WGS84")
    MEGA$lng<-MEGA@coords[,1]
    MEGA$lat<-MEGA@coords[,2]
  }
  return (MEGA)
}

############## AQUI DETERMINAMOS A QUÉ CT PERTENECE CADA DELITO

robos_vehiculos <- base.homol %>% 
  filter(DELITO.HOMOL%in%c("ROBO A CONDUCTOR/PASAJERO DE VEHÍCULO CON VIOLENCIA",
                           "ROBO A REPARTIDOR",#"ROBO A TRANSEÚNTE EN VÍA PÚBLICA",
                           "ROBO DE VEHÍCULO CON VIOLENCIA")) %>%
  mutate(etiq = "ROBO EN VEHÍCULO") %>%
  cambio_coo() %>%
  raster::intersect(CT)

########################################################################################################
######################################## Las CT que concentran la mayoría de estos delitos #############
########################################################################################################
# las que suman más de la mitad de todos los delitos de la ciudad

top_ct_en_robo <- robos_vehiculos@data %>% 
  group_by(ct,etiq) %>%
  summarise("CANTIDAD TOTAL"=n()) %>%
  arrange(desc(`CANTIDAD TOTAL`))

suma.acumulada <- cumsum(top_ct_en_robo$`CANTIDAD TOTAL`)
mitad.delitos <- nrow(robos_vehiculos)/2 
indice <- which(suma.acumulada > mitad.delitos) %>% min()



# elegimos solo las CT consideradas y añadimos su respectiva cantidad en el orden correcto de la capa

top_nombres<-top_ct_en_robo$ct[1:indice]




################################################################################

# Sección de las CT's que buscamos

shp_top_ct <- CT[CT$ct%in%top_nombres,]
shp_top_ct$`CANTIDAD DE DELITOS` <- left_join(data.frame(ct=shp_top_ct$ct),
                                              top_ct_en_robo,
                                              by="ct")$`CANTIDAD TOTAL`

#Mapa coloreado de las CT involucradas

mapviewOptions(fgb = FALSE,vector.palette = colorRampPalette(c("orange","red2")))
mapa_top_ct <- mapView(Alc,alpha.regions=0,zcol="NOMGEO",layer.name="Alcaldías",
                       map.types="OpenStreetMap",lwd=4,popup=NULL,homebutton=F,
                       legend=F) +
  
  mapview(shp_top_ct,layer.name="CT's",zcol="CANTIDAD DE DELITOS",homebutton=F,
          popup=popupTable(shp_top_ct,zcol = c("delegacion","ct",
                                               "CANTIDAD DE DELITOS"),
                           row.numbers = F,feature.id = F)
          )
mapviewOptions(default = T)



########################################################################################################
##############ESTO FUE PARA ENCONTRAR EPSILON, PERO NO VA EN LA TESIS###################################
########################################################################################################

kNNdistplot(robos_vehiculos@coords,k = 4-1)
abline(h=0.001,lty = 2,col='red')
text(x=2500, y=0.0016, 'eps=0.001', col='red')

########################################################################################################
##################################CIRCULOS REPRESENTANDO LA ZONA########################################
########################################################################################################

# se fija una semilla para que se pueda reproducir el proyecto
set.seed(1)
densi <- dbscan(robos_vehiculos@coords,eps=0.001,minPts = 4)
robos_vehiculos$cluster <- densi$cluster

# vamos a elegir el 5% más denso
# contamos cuantos son el 5%
cantidad.clusters <- robos_vehiculos$cluster %>%
  max() %>% `/`(20) %>% floor()

# hallamos los nombres de los clusters más densos
top.clusters <- robos_vehiculos$cluster %>%
  table() %>%
  sort() %>%
  tail(cantidad.clusters) %>%
  names() %>%
  .[.!=0]

# filtramos la base de robos para elegir solo de los clusteres deseados
indices.topclus <- which(robos_vehiculos$cluster %in% top.clusters)
robos.topclusters <- robos_vehiculos[indices.topclus,]

# mapview(robos.topclusters,zcol="cluster",map.types="OpenStreetMap",homebutton=F,legend=F)

#aqui algo inusual que ocurrió es que el cluster 36 tiene 1200 puntos. Esto podría trabajarse
#de dos maneras: con un cluster gigante o corriendo dbscan nuevamente con esos puntos

################################################################################

# declaramos un dataframe para almacenar la información de los círculos

cubiertas.clus <- data.frame() 

for (clus in top.clusters) {
  # los puntos se convierten en objetos de tipo SF 
  # para generar el círculo delimitador mínimo
  
  puntos.aux <- robos.topclusters@data %>% 
    filter(cluster == clus) %>%
    select(lng,lat) %>% 
    as.matrix() %>%
    st_multipoint() %>%
    st_sfc %>% 
    st_set_crs(4326) # EPSG 4326 = WGS84
  
  #lo siguiente genera un poligono, pero no lo vamos a proyectar así
  
  mi_circulo <- puntos.aux %>%
    lwgeom::st_minimum_bounding_circle() 
  
  mi_caja <- mi_circulo %>% st_bbox()
  
  centro.aux <- st_centroid(mi_circulo)
  
  radio.aux <- distm(x = mi_caja[c("xmin","ymax")],
                     y = mi_caja[c("xmax","ymax")]) %>% #antes tenía ,fun = distHaversine
    `/`(2) %>% ceiling() 
  
  # ahora juntamos los datos obtenidos
  
  renglon.aux <- data.frame("lon" = centro.aux[[1]][1],
                            "lat" = centro.aux[[1]][2],
                            "tam.clus" = puntos.aux[[1]] %>% length(),
                            "radio.clus" = radio.aux,
                            "num.clus" = clus)
  
  # añadimos los datos al data frame
  
  cubiertas.clus <- rbind(cubiertas.clus,
                          renglon.aux)
}


################################################################################
# AÑADIMOS AL MAPA LOS CIRCULOS QUE CUBREN A LOS CLUSTERES

col.paleta <- c("yellow","orange","red2")

cubiertas.clus <- cubiertas.clus %>%
  mutate(colores = colorRampPalette(col.paleta)(max(tam.clus))[tam.clus])


# lo siguiente solo es para ver el estado actual de las cubiertas, pero no se usa para el mapa final

mapa_top_ct@map%>% 
  addMapPane(name = "ames_circles", zIndex = 420)%>%
  addCircles(data = cubiertas.clus,lng = ~lon,lat = ~lat,radius = ~radio.clus,fillOpacity = 0.8,fillColor = ~colores,
             weight = 2, color = "black",opacity = .9,group = "Circulos",options = pathOptions(pane = "ames_circles")) %>%
  addLayersControl(position = "topleft",
                   overlayGroups = c("Alcaldías", "CT's","Circulos"),
                   options = layersControlOptions(collapsed = FALSE)
  )

# write.csv(cubiertas.clus, file = "MyMoto.csv",row.names=FALSE, na="",fileEncoding = "WINDOWS-1252") #o "UTF-8"

################################################################################










