library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
# Define UI for application that draws a histogram
ui <-  navbarPage(title = "Progetto Datascience",
  tabPanel("Iscrizioni univeristà",
  tabsetPanel(
    tabPanel("Iscrizioni per provincia",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectProvincia","Selezioni provincia:",
                             province$ResidenzaP,selected = "AGRIGENTO"),
                 textOutput("mediaProv"),
                 textOutput("minProv"),
                 textOutput("maxProv"),
                 
               ),
               
               mainPanel(
                 p(strong("Fai click sul grafico per visualizzare il numero di iscritti")),
                 verbatimTextOutput("clickVal"),
                 plotOutput("iscirizioniPlot", click = "prov_plotClick")
               )
             )
    ),
    tabPanel("Iscrizioni per anno",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectAnnoA","Seleziona anno accademico:",
                             anniA$AnnoA, selected = "2021/2022"),
                 dataTableOutput("tableIscritti")
                 
               ),
               mainPanel(
                 leafletOutput("mapUni"),
               )
             )
    )
  ))
)


server <- function(input, output) {
  if(!exists("foo", mode="function")) source("unioneDatasets.r")
  output$iscirizioniPlot <- renderPlot({
    print(input$selectProvincia)
    ggplot( iscrittiByProvincia(input$selectProvincia),aes(y=AnnoA,x=IscrittiTot)) +
      geom_col()+
      geom_text(aes(label = IscrittiTot))
  })
  re <- reactive({paste("Media iscritti: ",mean(iscrittiByProvincia(input$selectProvincia)$IscrittiTot))})
  output$mediaProv <- renderText(re())
  output$minProv <- renderText(paste("Iscrizioni più basse:",min(iscrittiByProvincia(input$selectProvincia)$IscrittiTot)))
  output$maxProv <- renderText(paste("Iscrizioni più alte:",max(iscrittiByProvincia(input$selectProvincia)$IscrittiTot)))
  output$clickVal <- renderText(input$prov_plotClick$x)
  colorpal <- function(pal, n){colorRampPalette(pal)(n)}
  filtredDatiMap <- reactive({
    dbCompletoPerAnno(input$selectAnnoA)
  })
  filtredDatiTable <- reactive({
    dbCompletoPerAnno(input$selectAnnoA)%>%
      select(ResidenzaP,PercentIscritti)
  })
  output$mapUni <-renderLeaflet({
    leaflet(filtredDatiMap())%>%
      addTiles()%>%
      fitBounds(~min(long),~min(lat),~max(long),~max(lat))
  })
  output$tableIscritti <- renderDataTable(filtredDatiTable())
  observe({
    color_palette <- grDevices::colors()  
    palette <- color_palette[grep(
      "red", grDevices::colors())]
    green_palette <- sample(palette, 25)
    pal <- colorQuantile("Paired", domain = filtredDatiMap()$PercentIscritti , n= 10)
    print(pal)
    leafletProxy("mapUni", data = filtredDatiMap())%>%
      clearShapes()%>%
      addCircleMarkers(
        radius = ~filtredDatiMap()$PercentIscritti*2,
        fillOpacity = 0.7,
        fillColor = ~pal(filtredDatiMap()$PercentIscritti),
        color = ~pal(filtredDatiMap()$PercentIscritti),
        lng = as.double(filtredDatiMap()$long),
        lat = as.double(filtredDatiMap()$lat),
        popup = ~htmlEscape(paste(filtredDatiMap()$PercentIscritti,filtredDatiMap()$ResidenzaP)))%>%
      addLegend(position = "bottomleft", pal = pal, 
                values = ~filtredDatiMap()$PercentIscritti,
                title = "Iscritti univeristà")
    
  })

} 
# Run the application 
shinyApp(ui = ui, server = server)