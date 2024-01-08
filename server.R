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

source("./source.R", local=TRUE)
#######################################################
# Define server logic -
#######################################################



 function(input, output,session) {
  
  ####################
  ##fichier Tadarida##
  ####################
  data1 <- reactiveValues()
  #previsualisation fichier tadarida
  output$contents1 <- renderTable({
    req(input$filecsv)#bloque la suite du code si vide
    df1 <- read.csv(input$filecsv$datapath, header = TRUE, sep = ";")
    return(head(df1, n = 1))

  })
  
  output$contents11 <- renderTable({
    req(input$filecsv)#bloque la suite du code si vide
    df1 <- read.csv(input$filecsv$datapath, header = TRUE, sep = ";")
     return(tail(df1, n = 1))
  })
  

  
  #lecture fichier Tadarida
  observeEvent(input$ValidTab, {
    
    if(!is.null(input$filecsv$datapath)){
      data1$table<- read.csv(input$filecsv$datapath, header = TRUE, sep = ";")
      sendSweetAlert(session = session, title = "Done !", text = "Le fichier a bien ete lu !",type = "success")
      output$plot <- renderPlot({
        plotG(data1$table,input$proba, input$size)
      }, height = 700, width = 1200)#, res = 120
    }
    #updateTabItems(session, "tabs", selected = "visualization")
    #}
  })
  
  
  
  ######################################
  #chemin, coordonnees et observateur ##
  ######################################
  
  WGS84<-reactiveValues()
  RGF93<-reactiveValues()
  
  observeEvent(input$convert, {
    if(input$coordsys=="RGF93"){
      RGF93$x<-input$X
      RGF93$y<-input$Y
      loc<-rgf2wgs(RGF93$x,RGF93$y)
      WGS84$x<-loc[1]
      WGS84$y<-loc[2]
    }else{
      WGS84$x<-input$X
      WGS84$y<-input$Y
      loc<-wgs2rgf(WGS84$x,WGS84$y)
      RGF93$x<-loc[1]
      RGF93$y<-loc[2]
    }
    sendSweetAlert(session = session, title = "Done !", text = "conversion OK !",type = "success")
  })
  
  data3<-reactiveValues()
  
  data3$DT <- data.frame(Chemin=NA,
                         X_RGF93 = NA,
                         Y_RGF93 = NA,
                         X_WGS84=NA,
                         Y_WGS84=NA,
                         Observateur=NA,
                         stringsAsFactors = FALSE)
  
  
  newEntry <- observeEvent(input$action, {
    newLine <- c(input$Chemin,RGF93$x, RGF93$y, WGS84$x, WGS84$y,input$op)
    data3$DT <- rbind(data3$DT, newLine)
    data3$DT<-data3$DT[-1,]
  })
  
  output$contents6 <- renderTable({
    data3$DT
  })
  
  
  
  
  data<-reactiveValues()
  
  observeEvent(input$transform, { # "transform" is an action button
    showModal(modalDialog("Transformation en cours...patience", footer=NULL))

    data$table<-transform(Contacts=data1$table,
                          X=data3$DT[1,2],
                          Y=data3$DT[1,3],
                          x=data3$DT[1,4],
                          y=data3$DT[1,5],
                          obs=data3$DT[1,6],
                          Chemin=data3$DT[1,1])
                    
    sendSweetAlert(session = session, title = "Done !", text = "C'est fait, fermez la fenetre!",type = "success")
    str(data$table)

    removeModal()
  }
  )
  
  output$download <- downloadHandler(
    
    #filename = function() {"site-2023-pt00_TadANALYSE.xlsx"},#modif 012024
    filename = function() {paste0(input$site,"-",input$an,"-",input$point,"_TadANALYSE.xlsx")},#modif 012024
    content = function(filename) {
      showModal(modalDialog("préparation du fichier...patience", footer=NULL))
      show_modal_progress_line() # show the modal window
      print("writing data to tempfile")
      #write.xlsx(data$table, filename, row.names = T)
      #wbook<-createWorkbook(type="xlsx")
      #sheet<-createSheet(wbook, sheetName="tadarida")
      #addDataFrame(data$table,sheet,startColumn=1) 
      #saveWorkbook(wb=wbook,file=filename)
      dimA <- paste0("A2:A", nrow(data$table))
      dims <- paste0("J2:J", nrow(data$table))
      dims1 <- paste0("G2:G", nrow(data$table))
      dims2 <- paste0("B2:B", nrow(data$table))
      
      update_modal_progress(0.2) # update progress bar value
      
      wb <- wb_workbook()
      wb$add_dxfs_style(name = "Style0", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "red"))
      wb$add_dxfs_style(name = "Style1", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "orange"))
      wb$add_dxfs_style(name = "Style2", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "yellow"))
      wb$add_dxfs_style(name = "Style3", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "yellowgreen"))
      wb$add_dxfs_style(name = "Style4", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "green"))
      wb$add_dxfs_style(name = "Style5", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "mediumseagreen"))
      
       update_modal_progress(0.4) # update progress bar value
      
      wb$add_worksheet("Tadarida")
      wb$add_data("Tadarida", data$table, withFilter = TRUE)
      wb$freeze_pane("Tadarida", first_row = TRUE) ## shortcut to first_active_row = 2
      
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims,
        rule = ">0",
        style = "Style0"
      )
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims,
        rule =">=0.25",
        style = "Style1"
      )
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims,
        rule = ">=0.5",
        style = "Style2"
      )
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims,
        rule = ">=0.75",
        style = "Style3"
      )
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims,
        rule = ">=0.9",
        style = "Style4"
      )
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims,
        rule = ">=0.99",
        style = "Style5"
      )
      wb$add_conditional_formatting(
        "Tadarida",
        dims = dims1,
        type = "dataBar",
        rule = c(0,5),
        style = c("green","green"),
        params = list(
          showValue = TRUE,
          gradient = TRUE)
      )
      
      wb$add_cell_style(dims = dimA, horizontal="right")
      wb$set_col_widths(cols=c(1,4:6,18,19:23),widths=c(19,rep(5,3),15,rep(10,5)))
      
      update_modal_progress(0.8) # update progress bar value
      
      wb$save(filename)
      
     update_modal_progress(0.99) # update progress bar value
          Sys.sleep(2)
      print("finished writing to tempfile, now copying it")

      sendSweetAlert(session = session, title = "Done !", text = "C'est fait, enregistez et fermez la fenetre!",type = "success")
      removeModal()
      remove_modal_progress() # remove it when done
    }
  )
}


# Run the app ----
#shinyApp(ui = ui, server = server)

