library(shiny)
library(stringr)
library(leaflet)
library(leafletCN)
library(RColorBrewer)

ChinaGDP <- read.csv("https://raw.githubusercontent.com/Judioljuse/shiny_map/master/year_count_map_data.csv",fileEncoding = "UTF-8",stringsAsFactors = F)
#str(ChinaGDP)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Number of student", min(ChinaGDP$count), max(ChinaGDP$count),
                            value = range(ChinaGDP$count), step = 10
                ),
                sliderInput("year", "chose year", min(ChinaGDP$year), max(ChinaGDP$year),
                            value = min(ChinaGDP$year), step = 1,
                            animate = animationOptions(interval = 4000,loop = TRUE, playButton = NULL,
                                                       pauseButton = NULL)
                ),
                selectInput("college", "college",selected="全部",
                            c('全部',unique(ChinaGDP$学院))
                ),
                selectInput("major", "major",selected="全部",
                            c('全部',unique(ChinaGDP$专业))
                ),

                #selectInput("colors", "Color Scheme",
                #            rownames(subset(brewer.pal.info, category %in% c("div")))
                #),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {

  filteredData <- reactive({
    ChinaGDP <- ChinaGDP[ChinaGDP$year==input$year[1],]

    if (input$college !="全部") {
      ChinaGDP <- ChinaGDP[ChinaGDP$学院==input$college,]
    }
    if (input$major!="全部") {
      ChinaGDP <- ChinaGDP[ChinaGDP$专业==input$major,]
    }
    ChinaGDP <- ChinaGDP[ChinaGDP$count >= input$range[1] & ChinaGDP$count <= input$range[2],]

    dat <- aggregate(ChinaGDP$count, by=list(Category=ChinaGDP$city), FUN=sum)
    leafletGeo("city", dat)

  })

  colorpal <- reactive({
    #colorNumeric(input$colors, ChinaGDP$count,reverse = TRUE)
    bins <- c(0,1,10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = ChinaGDP$count, bins = bins)
  })


  output$map <- renderLeaflet({
    pal <- colorpal()
    map = filteredData()
    map@data[is.na(map$value),]$value <- 0

    labels <- sprintf(
      "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
      map$name, map$value
    ) %>% lapply(htmltools::HTML)

    leaflet(map) %>% amap() %>%
      #加入框边界及颜色
      addPolygons(stroke = TRUE,
                  smoothFactor = 1,
                  fillOpacity = 0.7,
                  weight = 0.4,
                  fillColor = ~pal(value),
                  color = "white",
                  popup = ~htmltools::htmlEscape(popup),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)
      )
      #加入右下角边框

  })

  observe({
    pal <- colorpal()
    map = filteredData()
    map@data[is.na(map$value),]$value <- 0
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people <sup>2</sup>",
      map$name, map$value
    ) %>% lapply(htmltools::HTML)

    leafletProxy("map", data = map) %>%
      addPolygons(stroke = TRUE,
                  smoothFactor = 1,
                  fillOpacity = 0.7,
                  weight = 0.5,
                  fillColor = ~pal(value),
                  color = "white",
                  popup = ~htmltools::htmlEscape(popup),
                  highlight = highlightOptions(
                    weight = 0.5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })

  observe({
    proxy <- leafletProxy("map", data = filteredData())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend("bottomright", pal = pal, values = ~value,
                                title = "legendTitle",
                                labFormat = leaflet::labelFormat(prefix = ""),
                                opacity = 1)
    }
  })
  observe({
    x <- input$year
    ChinaGDP <- ChinaGDP[ChinaGDP$year==input$year[1],]
    updateSelectInput(session, "college",
                      label = paste("college"),
                      choices = c('全部',unique(ChinaGDP$学院)),
                      selected = '全部'
    )
  })
  observe({
    x <- input$college
    # Can use character(0) to remove 全部 choices
    if (is.null(x))
      x <- '全部'

    updateSelectInput(session, "major",
                      label = paste("major", length(x)),
                      choices = c('全部',unique(ChinaGDP[ChinaGDP$学院==x,]$专业)),
                      selected = '全部'
    )
  })

}

shinyApp(ui, server)
