# module load  StdEnv/2023  gcc/12.3  udunits/2.2.28  hdf/4.2.16  gdal/3.7.2  r/4.3.1

.libPaths(c("/home/cseiler/daisy/renv", "/home/cseiler/R/x86_64-pc-linux-gnu-library/4.3"))

library(shiny)
library(foreach)
library(slickR)
rm(list = ls())
setwd("/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/shinyApp")

#-------------------------------------------------------------------------------
# Individual simulations
#-------------------------------------------------------------------------------
amberOutputDir <- "/home/cseiler/projects/def-cseiler-ab/cseiler/data-assimilation/AMBER"
fnames <- list.files(path = amberOutputDir, pattern = ".png", all.files = TRUE, recursive = TRUE)
modelName.fileName <- strsplit(fnames, split = "/")

fun.createTable <- function(x) {
  model <- x[1]
  file <- x[2]
  
  myString <- strsplit(file, split = "-")
  variable <- myString[[1]][1]
  lastElement <- length(myString[[1]])
  metric <- myString[[1]][lastElement]
  metric <- gsub(".png", "", metric)
  metric[is.na(metric)] <- "mean"
  result <- data.frame(model, variable, metric, file)
  colnames(result) <- c("model", "variable", "metric", "file")
  return(result)
}
data <- lapply(modelName.fileName, fun.createTable)
data <- do.call(rbind, data)

# clean up
data <- subset(data, model != "summaries")
data <- subset(data, variable != "zonalMeanStats")

model <- unique(data$model)
variable <- unique(data$variable)
metric <- unique(data$metric)


data01 <- data


#-------------------------------------------------------------------------------
# Ensemble
#-------------------------------------------------------------------------------
fnames <- list.files(path = paste(amberOutputDir, "/summary", sep = ""), pattern = ".png", all.files = TRUE, recursive = TRUE)

result <- foreach(i=variable) %do%
  {
    file <- grep(i, fnames, value = TRUE)
    nfile <- length(file)
    data <- data.frame(rep(i, nfile), file)
    colnames(data) <- c("variable", "file")
    return(data)
  }
data <- do.call(rbind, result)
data02 <- data

model <- rep("summaries", nrow(data02))
metric <- rep("summaries", nrow(data02))
variable <- data02$variable
file <- data02$file

data02 <- data.frame(model, variable, metric, file)

# add individual files that don't fit the scheme
a <- c("summaries", "scores", "summaries", "scoresCompareEachModel.png")
b <- c("summaries", "scores", "summaries", "scatterplotScores.png")
c <- data.frame(rbind(a, b))
colnames(c) <- c("model", "variable", "metric", "file")

data02 <- rbind(c, data02)
rownames(data02) <- c(1:nrow(data02))

#-------------------------------------------------------------------------------
# Combine ensemble and individual models
#-------------------------------------------------------------------------------

data <- rbind(data02, data01)
rownames(data) <- c(1:nrow(data))
model <- unique(data$model)
variable <- sort(unique(data$variable))

## Move scores up front
x <- setdiff(variable, "scores")
variable <- c("scores", variable)

metric <- unique(data$metric)
df <- data

#-------------------------------------------------------------------------------
# ui
#-------------------------------------------------------------------------------

ui <- pageWithSidebar(
  headerPanel("AMBER"),
  sidebarPanel(
    selectInput("model", "Simulation", choices = model, selected = 1),
    selectInput("variable", "Variable", choices = variable, selected = 1),
    selectInput("metric", "Metric", choices = metric, selected = 1), width = 3, 
    h4("Notes"),
    "Select a model, variable, and statistical metric.", br(),
    "The category summaries gives results for the ensemble.", br(),
    "Note that not all metrics are available for all variables.", br(),
    h4("Abbreviations"),
    "mly = monthly Hovmoeller plots", br(),
    "yly = yearly time series", br(),
    "sd = standard deviation", br(),
    "rmse = root mean square error", br(),
    "crmse = centralized rmse", br(),
    "iav = interannual variability", br(),
    h4("Methods and Code"),
    "Automated Model Benchmarking R Package (AMBER)", br(),
    "https://gitlab.com/cseiler/AMBER", br(),
  ),
  
  mainPanel(
    
    
    tabsetPanel(
      tabPanel("Figures",
               slickROutput("slickr", width = "100%")),
      tabPanel("File Names", tableOutput("table1"))
      
      
    )
  )
)

#-------------------------------------------------------------------------------
# server
#-------------------------------------------------------------------------------

server <- (function(input, output) {
  
  df_subset <- reactive({
    mySubset <- subset(df, model == input$model &
                         variable == input$variable &
                         metric == input$metric)
    ID <- 1:nrow(mySubset)
    mySubset <- data.frame(ID, mySubset)
    return(mySubset)
  })
  
  output$table1 <- renderTable(df_subset())
  
  output$slickr <- renderSlickR({
    myFiles <- file.path(amberOutputDir, input$model, df_subset()$file)
    slickR(obj = myFiles, slideId = NA, height = 500, width = "100%")
  })
})


shinyApp(ui, server)
#-------------------------------------------------------------------------------

