library(shiny)
library(readr)
library(magrittr)
library(sf)
library(tibble)
library(dplyr)

ui <- fluidPage(
  titlePanel(HTML("根据经纬度判断所处的省市区县<br>使用 2019 年各级行政区划数据<br><small>微信公众号：RStata</small>")),
  
  sidebarLayout(
    sidebarPanel(
      # 文件输入
      fileInput("file1", "选择一个 csv 文件（里面应该包含 lon 和 lat 字段）",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # 坐标参考系
      textInput("crs", label = "请输入你要转换坐标的 CRS", value = "+proj=longlat +datum=WGS84 +no_defs", placeholder = "+proj=longlat +datum=WGS84 +no_defs"),
      # 选择省市区县
      selectInput("select", label = "请选择你要判断该坐标所处的省、市还是区县", 
                  choices = c("省", "市", "县")),
      downloadButton("downloadData", "Download")
    ),

    mainPanel(
      tableOutput("contents")
    )
    
  )
)

server <- function(input, output) {

  datasetInput <- reactive({
    read_rds(paste0(input$select, ".rds")) -> map
    read_csv(input$file1$datapath) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = input$crs) %>% 
      st_make_valid() %>% 
      st_intersection(st_make_valid(map)) -> df1

    bind_cols(
      as_tibble(st_coordinates(df1)),
      st_drop_geometry(df1)
    ) %>% 
      rename(lon = X, lat = Y)
  })
  
  output$contents <- renderTable({
    
    req(input$file1)
    tryCatch(
      {
        df <- datasetInput()
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(head(df))
  })
  output$downloadData <- downloadHandler(
    filename = function() {"结果数据.csv"},
    content = function(file) {
      write_csv(datasetInput(), file)
    }
  )
}

shinyApp(ui, server)
