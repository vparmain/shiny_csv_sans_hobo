options(java.parameters = "-Xmx8000m")
options(shiny.maxRequestSize=30*1024^2)#30MB


######################################################
#packages
######################################################
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(chron)
library(openxlsx2)
library(timeDate)
library(stringr)
library(dplyr)
library(lubridate)
#library(rgdal)
library(sf)
library(shinybusy)
library(ggplot2)
library(ggpubr)


source("./source.R", local=TRUE)
######################################################

######################################################
# Define User Interface
######################################################
fluidPage(
  tags$head(tags$style('
   body {
      font-family: Arial; 
      font-size: 10px; 
      font-style: italic;
      #font-weight:bold;
   }'
  )),# ajout 012024
  titlePanel(title=div(h1("Preparation du tableur vigie-chiro pour analyse"),h6("Office National des Forets / V. Parmain & O. Vinet / Mars 2020/mise à jour 11/2023_01/2024")),
             windowTitle = "Prepa .csv"),

  add_busy_bar(color = "red", height = "40px", timeout = 1000),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("filecsv", h6("Choisir le fichier .csv a transformer:"), multiple = FALSE, accept = ".csv",
                buttonLabel = "Parcourir...", placeholder = "Pas de fichier"),
      fluidRow(
        column(4,actionButton("ValidTab", label = "Valider")),
        column(4,sliderInput("proba", "proba", 0.5, min = 0, max = 1)),
        column(4,sliderInput("size", "size", 4, min = 0, max = 10))
      ),
      
      #radioButtons("radio", label = h6("Systeme de coordonnees"), choices = list("RGF93" = 1, "WGS84" = 2),selected = 1),
      selectInput("coordsys", label = h6("Choisir le systeme de coordonnees"),
                  choices = list("RGF93", "WGS84"),selected = "WGS84"),
      
      fluidRow(
        column(4,numericInput("X", label = h6("X"), value = 1,width = '100%')),
        column(4,numericInput("Y", label = h6("Y"), value = 1,width = '100%'))),
      
      actionButton("convert", label = "Convertir"),
      
      textInput("op", h6("Observateur"), value = "Enter text..."),
     
      fluidRow(
        column(4,textInput("an", h6("Année"), value = "2024",width = '100%')),#ajout01_2024
        column(4,textInput("site", h6("Site"), value = "Enter text...",width = '100%')),#ajout01_2024
        column(4,textInput("point", h6("Point"),width = '100%',"Pass1-000"))),#ajout01_2024 #,sliderInput("n", "n", min = 0, max = 200, value = 0)
      
      #shinyDirButton('directory', 'Folder select', 'Please select a folder'),
      textInput("Chemin", h6("Chemin"), value = "Enter text..."),
      
      tableOutput("contents6"),
      
      actionButton("action", label = "Valider"),
      
      actionButton("transform", label = "Transformation"),
      
      downloadButton('download',"telecharger"),
      

      
      #downloadLink("downloadData", "Download")
      
    ),
    
    
    mainPanel(
      tableOutput("contents1"),
      tableOutput("contents11"),
      verbatimTextOutput("contents5"),
      #tableOutput("contents6"),
      tableOutput("contents7"),
      tableOutput("Contacts8"),
      plotOutput("plot", click = "plot_click",  width = "100%")
      #shiny::verbatimTextOutput("directorypath"),
      #shinyDirButton("dir", "Input directory", "Upload"),
      #verbatimTextOutput("dir", placeholder = TRUE)  # added a placeholder
    )
  )
)

