# Paquetes
require(dplyr)
library(sf)
library(terra)
#library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rsconnect)
library(spData)
#library(spDataLarge)



# Datos
vias <-
  st_read(
    "https://raw.githubusercontent.com/Andre9424/T3Shiny/main/Datos/redvialWGS84.geojson",
    quiet = TRUE
  )

distritos <-
  st_read(
    "https://raw.githubusercontent.com/Andre9424/T3Shiny/main/Datos/distritoWGS84.geojson",
    quiet = TRUE
  )

#dem <-
#rast(
"https://raw.githubusercontent.com/Andre9424/Proyecto_parte_1/main/DemAcostaWGS84.tif"
#)
#hill <-
#rast(
"https://raw.githubusercontent.com/Andre9424/Proyecto_parte_1/main/SombraAcostaWGS84.tif"
#)

# Lista ordenada de tipo de ruta + "Todas"
lista_tipovia <- unique(vias$TIPO_VIA)
lista_tipovia <- sort(lista_tipovia)
lista_tipovia <- c("Todas", lista_tipovia)

# Lista ordenada de estado de vias + "Todas"
lista_estadovia <- unique(vias$Estado_d_4)
lista_estadovia <- sort(lista_estadovia)
lista_estadovia <- c("Todas", lista_estadovia)


#Agregar logo muni
#title <- tags$a(href='https://www.acosta.go.cr/',
#tags$img(src='Z:\ProyectoParte2R\img\LOGO.png',height='60',width='200'))
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Red Vial del Canton de Acosta", titleWidth = 450),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "tipo_de_via",
        label = "Tipo de via",
        choices = lista_tipovia,
        selected = "Todas"
      ),
      selectInput(
        inputId = "estado_de_la_via",
        label = "Estado de la via",
        choices = lista_estadovia,
        selected = "Todas"
      ),
      
      startExpanded = TRUE
    ),
    menuItem(
      text = "Trazo del derecho de via",
      textInput(
        inputId = "codigo", 
        label = "Ingrese el codigo de la via mostrada en el mapa",
        
      ),
      sliderInput(
        inputId = "ancho",
        label = "Indique la medida del ancho del derecho de via", 
        min = 3, max = 22, value = 3, step= 0.1,
      ),
      
      startExpanded = TRUE
    ))
  ),
  dashboardBody(fluidRow(
    box(
      title = "Mapa de la Red Vial Cantonal",
      leafletOutput(outputId = "mapa"),
      width = 6
    ),
    box(
      title = "Importancia de los caminos segun IVTS y TPD",
      plotlyOutput(outputId = "grafico1"),
      width = 6
    )
  ),
  fluidRow(
    box(
      title = "Informacion del Inventario Vial",
      DTOutput(outputId = "tabla"),
      width = 6
    ),
    box(
      title = "Cantidad de kilometros lineales segun el estado de la via",
      plotlyOutput(outputId = "grafico2"),
      width = 6
    )      
  )),
  dashboardBody()
) 


# Definición de la función server
server <- function(input, output, session) {
  filtrarRegistros <- reactive({
    
    # Remoción de geometrías y selección de columnas
    vias_filtrado <-
      vias %>%
      dplyr::select(CODIGO, LONGITUD_VIA, De, A, TPD, IVTS, TIPO_VIA, Estado_d_4)
    
    # Filtrado por tipo de via
    if (input$tipo_de_via != "Todas") {
      vias_filtrado <-
        vias_filtrado %>%
        filter(TIPO_VIA == input$tipo_de_via)
    }
    # Filtrado por estado
    if (input$estado_de_la_via != "Todas") {
      vias_filtrado <-
        vias_filtrado %>%
        filter(Estado_d_4 == input$estado_de_la_via)
    }
    
    
    return(vias_filtrado)
    
    
  })
  
  output$mapa <- renderLeaflet({
    registros <- filtrarRegistros()
    
    #dem_acosta <- raster::raster(dem)
    #sombras_acosta <- raster::raster(hill)
    
    ancho_medida <- input$ancho
    
    vias_crtm05 <-
      vias %>%
      st_transform(5367)
    
    camino <- 
      vias_crtm05 %>%
      filter(CODIGO == input$codigo)
    
    
    ancho_derechovia <- st_buffer(camino, ancho_medida, nQuadSegs = 100, endCapStyle = "ROUND")
    ancho_derechovia <-
      ancho_derechovia %>%
      st_transform(4326)
    
    camino_wgs84 <-
      camino %>%
      st_transform(4326)
    
    leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = -84.12127, lat1 = 9.85589, 
        lng2 = -84.32671, lat2 = 9.62106)%>%
      
      addPolylines(
        data = camino_wgs84,
        color = "red",
        stroke = TRUE,
        weight = 2.0,
        label = paste0(
          'Codigo de la via: ',camino$CODIGO),
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
      ) %>%
      
      addPolygons(
        data = ancho_derechovia,
        color = "magenta",
        stroke = TRUE,
        weight = 2.0,
        popup  = paste0(
          'Ancho del derecho de via (m): ',ancho_medida),
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        group = "Derecho de via"
      ) %>%
      
      addPolylines(
        data = registros,
        color = "red",
        stroke = TRUE,
        weight = 2.0,
        popup  = paste0(
          'Tipo de via: ',registros$TIPO_VIA),
        label = paste0(
          'Codigo de la via: ',registros$CODIGO),
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        group = "Red Vial Cantonal"
      ) %>%
      addPolylines(
        data = distritos,
        color = "yellow",
        stroke = TRUE,
        weight = 2.5,
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        group = "Distritos"
      ) %>%
      
      #addRasterImage(group = "Modelo Digital de Elevacion",
      #dem_acosta, 
      # opacity = 0.6
      #) %>%    
      # addRasterImage(group = "Modelo de Sombras",
      #  sombras_acosta, 
      # opacity = 0.6
      #) %>% 
      
      addMiniMap( 
        width = 90,
        height = 90,
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE) %>%
      
      addScaleBar()%>%    
      addMeasure(position = "topright",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters",
                 activeColor = "#3D535D",
                 completedColor = "#7D4479") %>%
      
      addMouseCoordinates()%>%
      addSearchOSM()%>%
      addResetMapButton()%>%
      
      
      addProviderTiles(
        providers$CartoDB.Positron, group = "Sitios") %>%
      addProviderTiles(
        providers$Esri.WorldImagery, group = "Imagen satelital") %>%
      addLayersControl(
        baseGroups = c("Imagen satelital","Sitios"),
        overlayGroups = c("Red Vial Cantonal","Distritos", "Derecho de via"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  output$tabla <- renderDT({
    registros <- filtrarRegistros()
    
    Tabla_final_vias <-
      registros %>%
      rename(CODIGO = CODIGO,
             LONGITUDkm = LONGITUD_VIA,
             INICIO = De,
             FINAL = A,
             TPD = TPD,
             IVTS = IVTS,
             ESTADO = Estado_d_4,
             CATEGORIA = TIPO_VIA)
    Tabla_final_vias
    
    Tabla_final_vias %>%
      st_drop_geometry() %>%
      dplyr::select(CODIGO, LONGITUDkm, INICIO, FINAL, TPD, IVTS, ESTADO, CATEGORIA) %>%
      datatable(Tabla_final_vias, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 5))
    
  })
  
  output$grafico1 <- renderPlotly({
    registros <- filtrarRegistros()
    
    
    cols <- c("#D43F3A", "#EEA236", "#5CB85C")
    ggplot(data = registros) +
      geom_line(lwd = 0.5, aes(x = IVTS , y = TPD, color = TIPO_VIA)) + 
      facet_grid(~TIPO_VIA)+
      scale_color_manual(values = cols) +
      xlab("Indice de Viabilidad Tecnico - Social ITVS") +
      ylab("Transito Promedio Diario TPD") +
      guides(color = guide_legend(title = "Tipo de via")) +
      scale_fill_discrete(labels = c("Primaria", "Secundaria", "Terceria"))
    
    
  })
  
  output$grafico2 <- renderPlotly({
    registros <- filtrarRegistros()
    
    kilometro_acumulado <-
      registros %>%
      rename(Kilometros = LONGITUD_VIA,
             Estado = Estado_d_4) %>%
      group_by(Estado) %>%
      summarise(Kilometros = sum(Kilometros))
    kilometro_acumulado
    
    ggplot(
      kilometro_acumulado, 
      aes(x = Estado, y = Kilometros, fill = Estado)) +
      geom_col() +
      xlab("Estado") +
      ylab("Kilometros acumulados") +
      guides(fill = guide_legend(title = "Estado")) +
      scale_fill_discrete(labels = c("Bueno", "Regular", "Malo"))
    
    
  })
  
}

# Llamado a la función shinyApp()
shinyApp(ui, server)