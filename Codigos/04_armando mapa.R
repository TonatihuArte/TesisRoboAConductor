####### CONTRUCCIÓN DEL MAPA COMPLETO
# datos de los centros en cubiertas.clus

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Robo a Conductor")
) 


direc<-"ImagenesGeneradas/terna_"


cantidad_de_centros <- nrow(cubiertas.clus)#3

imagenes.popup <- paste0(direc,1:cantidad_de_centros,".png")

# imagenes.popup <- paste0(direc,1:nrow(cubiertas.clus),".png")


centros.con.popup <- cubiertas.clus[1:cantidad_de_centros,]
centros.sin.popup <- cubiertas.clus[-c(1:cantidad_de_centros),]


mapa_circ_popup <- mapa_top_ct@map %>% 
  addMapPane(name = "ames_circles", zIndex = 420) %>%
  addCircles(data = centros.con.popup,lng = ~lon,lat = ~lat,
             radius = ~radio.clus,fillOpacity = 0.8,fillColor = ~colores,
             weight = 2, color = "black",opacity = .9,
             options = pathOptions(pane = "ames_circles"),
             group = "Circulos",
             highlightOptions = highlightOptions(color = "gold",fillOpacity =0,
                                                 weight = 5, bringToFront = T, 
                                                 opacity = 1)) %>%
  addPopupImages(imagenes.popup, group = "Circulos",embed=T,width =300) %>%
  addCircles(data = centros.sin.popup,lng = ~lon,lat = ~lat,
             radius = ~radio.clus,fillOpacity = 0.7,fillColor = ~colores,
             weight = 2, color = "black",opacity = .8,options = pathOptions(pane = "ames_circles"))%>%
  addControl(title, position = "topleft", className="map-title")



mapshot(mapa_circ_popup,url = "Mapa_final.html",selfcontained = T,title="Robo a Conductor 2019")






