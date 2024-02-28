# la base principal se llama   robos.topclusters
# cubiertas.clus tiene la información de los circulos

delitos_a_considerar<-c("ROBO A TRANSEÚNTE EN VÍA PÚBLICA","ROBO A REPARTIDOR",
                        "ROBO DE VEHÍCULO CON VIOLENCIA",
                        "ROBO A CONDUCTOR/PASAJERO DE VEHÍCULO CON VIOLENCIA")
delitos_segundo_plano<-c("ROBO A CASA HABITACIÓN CON VIOLENCIA",
                         "ROBO A CASA HABITACIÓN SIN VIOLENCIA",
                         "ROBO A CUENTAHABIENTE","ROBO A NEGOCIO CON VIOLENCIA",
                         "ROBO A TRANSPORTISTA",
                         "ROBO DE VEHÍCULO SIN VIOLENCIA")

base_extra <- base.homol %>% 
  filter(DELITO.HOMOL %in% delitos_a_considerar) %>% cambio_coo()
base_extra_segPlano <- base.homol %>% 
  filter(DELITO.HOMOL %in% delitos_segundo_plano) %>% cambio_coo()


# función que genera las tres imágenes, las pega y guarda la imagen para ser
# leida para usar como popup


popup_generador <- function(datos_centros){
  dir.guardado <- "ImagenesGeneradas/"
  dir.create(dir.guardado,recursive = T,showWarnings = F)
  
  # datos_centros <- cubiertas.clus[1:3,] #borrar
  
  for (i in 1:nrow(datos_centros)) {
    renglon <- datos_centros[i,]
    entorno <- funcion_entorno(numero_de_cluster = renglon$num.clus)
    
    # Foto del mapa con delitos
    
    mapa <- mapa_zona_particular(renglon_cubierta = renglon, 
                                 vecinos = entorno[[1]],
                                 nombre = paste0("Entorno_",i,".png"))
    
    # Foto del Radar
    
    tabla.radar <- datos_radar(entorno[[1]],entorno[[2]])
    radar(tabla.radar,paste0("Radar_",i,".png"))
    
    # Foto del horario
    
    tabla_horarios(datos = entorno[[1]],nombre = paste0("Tabla_",i,".png"))
    
    # Pegando las tres imágenes
    
    generador_terna(i)
  }
}

################################################################################
################### BUSCANDO PUNTOS DENTRO DE LA CUBIERTA#######################
################################################################################

# recibiendo el número de cluster, se genera su cubierta mínima.
# luego se intersecta con los dos conjuntos de puntos
# los regresa en una lista

funcion_entorno <- function(numero_de_cluster){
  
  # este poligono ha sido creado por segunda ocasión, lo que representa una
  # falla sobre la eficiencia computacional. Pero se separa por cuestiones
  # didacticas
  
  mi_circulo <- robos.topclusters@data %>% 
    filter(cluster == numero_de_cluster) %>%
    select(lng,lat) %>% 
    as.matrix() %>%
    st_multipoint() %>%
    st_sfc() %>% 
    st_set_crs(4326) %>% # EPSG 4326 = WGS84
    st_minimum_bounding_circle() %>% 
    as('Spatial')
  
  vecinos_grupo1 <- raster::intersect(base_extra,mi_circulo)
  vecinos_grupo2 <- raster::intersect(base_extra_segPlano,mi_circulo)
  
  return(c(c(vecinos_grupo1),c(vecinos_grupo2)))
}  

################################################################################
################CONSTRUYENDO EL MAPA INTERACTIVO CON SU RED#####################
################################################################################
# En esta función seguramente se deba
# buscar el coeficiente de agrupamiento

construir.red <- function(puntos_espaciales,distancia){
  # nombramos a cada punto
  puntos_espaciales$nombre <- 1:nrow(puntos_espaciales)
  
  conjunto.lineas <- c()
  lista.aristas <- matrix(ncol = 2, nrow = 0) %>% 
    data.frame()
  
  for (i in 1:(nrow(puntos_espaciales)-1)) {
    
    punto.aux <- puntos_espaciales[i,]
    
    # se hace un circulo de 100m de radio para intersectar puntos
    
    circulo_100m <- punto.aux@data %>% 
      select(lng,lat) %>% 
      as.matrix() %>%
      st_multipoint() %>%
      st_sfc() %>% 
      st_set_crs(4326) %>%
      st_buffer(dist = distancia)%>% 
      as('Spatial')
    
    puntos.para.revisar <- puntos_espaciales[(i+1):nrow(puntos_espaciales),]
    
    condicion <- raster::intersect(raster::union(punto.aux,puntos.para.revisar),
                                   circulo_100m) %>% nrow()
    
    if (condicion > 1) {
      puntos.cercanos <- raster::intersect(puntos.para.revisar,circulo_100m)
      
      for (j in 1:nrow(puntos.cercanos)) {
        dos.puntos <- rbind.SpatialPoints(punto.aux,puntos.cercanos[j,])
        linea <- as(dos.puntos,
                    "SpatialLines") 
        if (is.null(conjunto.lineas)) {
          conjunto.lineas <- linea
        }else{
          conjunto.lineas <- rbind.SpatialLines(conjunto.lineas,linea)
        }
        # añadimos esta información a la lista de aristas
        arista.aux <- c(punto.aux$nombre,puntos.cercanos[j,]$nombre)
        lista.aristas <- rbind(lista.aristas,arista.aux)
      }
    }
  }
  
  lista.aristas <- lista.aristas %>%
    setNames( c("from", "to"))
  
  # g <- graph_from_data_frame(lista.aristas, directed=F)
  # print(g, e=TRUE, v=TRUE)
  # plot(g)
  
  par <- list(conjunto.lineas,lista.aristas)
  
  return(par)
}

# La siguiente función calcula el coeficiente de agrupamiento
# y se pueden calcular la siguientes centralidadeS: 
# Closeness, betweenness, pagerank, eigenvector y degree.
# transitivity es el cueficiente de clustering

puntos.relevantes <- function(lista_de_aristas, vecinos_espaciales){
  graf.completa <- igraph::graph_from_data_frame(lista_de_aristas, directed=F) 
  mayor.comp.conexa <- igraph::largest_component(graf.completa)
  grados <- igraph::degree(mayor.comp.conexa)
  
  # Aquí calculamos el coeficiente de agrupamiento
  
  coeficientes.clus <- igraph::transitivity(mayor.comp.conexa,type = "local")
  
  # tomamos los vertices con grado mayor a 2 (me pareció cool pero esto puede cambiar)
  
  indices <- which(grados>2 &
                     coeficientes.clus == max(coeficientes.clus,na.rm = T)) %>% 
    names() %>% as.numeric()
  
  return(vecinos_espaciales[indices,])
}


mapa_zona_particular <- function(renglon_cubierta,vecinos,nombre){
  
  # renglon_cubierta <- renglon #borrar
  # vecinos <- entorno[[1]] #borrar
  
  red <- construir.red(vecinos,100)
  
  icons <- awesomeIcons(
    icon = "exclamation",
    library = "fa",
    iconColor = "#FFFFFF",
    # markerColor = "orange"
  )
  
  puntos_elegidos <- puntos.relevantes(red[[2]],vecinos)
  
  mapviewOptions(fgb = FALSE)
  
  # se concatenan los vertices y los puntos
  
  ZONA_PARTICULAR <- mapview(red[[1]],alpha=0.5)+
    mapview(vecinos,layer.name="ENTORNO",zcol="DELITO.HOMOL",homebutton=F)
  
  # se añade el círculo delimitador mínimo
  
  ZONA_PARTICULAR <- ZONA_PARTICULAR@map%>% 
    addCircles(data = renglon_cubierta,lng = ~lon,lat = ~lat,
               radius = ~radio.clus,fillOpacity = 0,
               weight = 2, color = "black",opacity = .9,
               group = "Circulos")%>%
    addAwesomeMarkers(data = puntos_elegidos,lng = ~lng, lat = ~lat, icon=icons, 
                      label="Punto Relevante", labelOptions=labelOptions(noHide=F))
  
  # se guarda la imágen
  
  mapshot(ZONA_PARTICULAR,file = paste0("ImagenesGeneradas/",nombre),
          remove_controls = c("zoomControl","layersControl"),delay=5)
}




######################### FALTA VER EN DONDE construir la matriz de enlaces (o su forma eficiente)
#### se necesita añadir una columna de ID unicos en la base de delitos para que funcione, creo

################################################################################
########################## RADAR ###############################################
################################################################################

datos_radar <- function(vecinosPrimarios,vecinosSecundarios){
  if (nrow(vecinosSecundarios)>0) {
    datos_zona<-t(unclass(table(vecinosPrimarios$DELITO.HOMOL))) %>%
      as.data.frame()
    datos_zona_segPlano<-t(unclass(table(vecinosSecundarios$DELITO.HOMOL))) %>%
      as.data.frame()
    
    vacio_1 <- rbind(rep(NA,ncol(datos_zona))) %>%
      data.frame() %>% setNames(nm = colnames(datos_zona))
    vacio_2 <- rbind(rep(NA,ncol(datos_zona_segPlano))) %>%
      data.frame() %>% setNames(nm = colnames(datos_zona_segPlano))
    
    grupo1<-cbind(datos_zona,vacio_2)
    rownames(grupo1)<-"GRUPO 1"
    grupo2<-cbind(vacio_1,datos_zona_segPlano)
    rownames(grupo2)<-"GRUPO 2"
    max_rad<-max(max(grupo1,na.rm = T),max(grupo2,na.rm = T))
    base_radar<-rbind(rep(max_rad,ncol(grupo1)),
                      rep(0,ncol(grupo1)),grupo1,grupo2)
    colnames(base_radar)<-str_wrap(colnames(base_radar),width = 17)
    return(base_radar)
  }else{
    datos_zona<-as.data.frame(t(unclass(table(vecinosPrimarios$DELITO.HOMOL))))
    max_rad<-max(datos_zona,na.rm = T)
    base_radar<-rbind(rep(max_rad,ncol(datos_zona)),
                      rep(0,ncol(datos_zona)),datos_zona)
    colnames(base_radar)<-str_wrap(colnames(base_radar),width = 17)
    return(base_radar)
  }
}

# se ingresa la base generada anteriormente y guarda la imagen del radar

radar<-function(datos,nombre,segmentos=7){
  max_rad <- datos[1,1]
  secuencia <- seq(max_rad/segmentos,max_rad,max_rad/segmentos) %>%
    round(digits = 1)
  
  for (i in 1:length(secuencia)) {
    if (i%%2 != segmentos%%2) {
      secuencia[i]<-""
    }
  }
  
  Cairo::Cairo(file=paste0("ImagenesGeneradas/",nombre),
               type="png",
               units="in",
               width=36,
               height=33,
               pointsize=22,
               dpi=72,bg = "white")
  
  #aqui revisa si necesitas usar radarchart2
  
  fmsb::radarchart(datos,
              # tamaño del texto
              vlcex = 1.65,calcex =  2,
              # segmentos
              seg=segmentos, caxislabels=c(0,secuencia), 
              #etiquetas en la red
              axistype=1, axislabcol="black", 
              # red
              cglty=3, cglwd=1,
              #poligonos
              pcol=1:8,plty=1:2,plwd = c(3,2),
              #relleno de poligonos con vectores
              pangle =60,pfcol =c(adjustcolor("lightblue3",alpha.f = 0.6),
                                  adjustcolor("white",alpha.f = 0.0)),
              #centro de la red
              na.itp=F)
  
  dev.off()
}

################################################################################
################## HORARIO DELICTIVO ###########################################
################################################################################

tabla_horarios <- function(datos,nombre,titulo="Horario delictivo",
                           subtitulo="01/01/2019-31/12/2019"){
  
  datos$diasemana <- datos$fecha_hechos %>% 
    weekdays(abbreviate = T) %>%
    str_to_title() %>% 
    factor(levels = c("Dom.","Lun.","Mar.","Mié.","Jue.","Vie.","Sáb."),
           ordered = T)
  
  datos$hora <- datos$hora_hechos %>%
    strsplit(split=":") %>%
    lapply(function(x){x[1]}) %>%
    unlist() %>%
    as.numeric()
  
  tabla <- table(datos$diasemana,datos$hora) %>%
    as.data.frame()
  
  Cairo::Cairo(file=paste0("ImagenesGeneradas/",nombre),
               type="png",
               units="in",
               width=9,
               height=13,
               pointsize=22,
               dpi=72,bg = "white")
  
  aux <- ggpubr::ggballoonplot(tabla,fill = "value",
                               rotate.x.text = F,size.range = c(0,12))+
    scale_fill_gradient(low = "green",high = "red")+
    geom_text(color="black",label=tabla$Freq,size=6,nudge_x = 0.28)+
    geom_tile(color="black",alpha=0.05)+
    theme(axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 15),
          plot.title = element_text(size = 21), 
          plot.subtitle = element_text(size = 13))+
    scale_x_discrete(position = "top")+
    labs(title = titulo,caption = "fuentes: PGJCDMX",subtitle = subtitulo)
  
  print(aux)
  dev.off()
}

################################################################################
######################### GENERADOR DE TERNA####################################
################################################################################

generador_terna <- function(nombre_numero) {
  dir.entorno <- paste0("ImagenesGeneradas/Entorno_",nombre_numero,".png")
  dir.radar <- paste0("ImagenesGeneradas/Radar_",nombre_numero,".png")
  dir.tabla <- paste0("ImagenesGeneradas/Tabla_",nombre_numero,".png")
  dir.terna <- paste0("ImagenesGeneradas/terna_",nombre_numero,".png")
  
  imag01 <-image_read(dir.entorno)
  imag02 <-image_read(dir.radar)
  imag03 <-image_read(dir.tabla)
  
  parte_inf <- image_scale(c(imag03,imag02),"x750") %>% image_append()
  terna <- image_scale(c(imag01,parte_inf),"850") %>% image_append(stack = T)
  
  image_write(terna, path = dir.terna, quality = 100)
}
