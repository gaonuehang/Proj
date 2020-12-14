---
output: html_document
runtime: shiny
---


library(shiny)
library(readxl)
library(ggplot2)
library(lubridate)


dat <- read_excel("Local_Area_Unemployment_Statistics__LAUS_.xlsx")

ui <- shinyUI(fluidPage(
  skin = "black",
  title = "Visualizing Unemployment Flow",
      
    column(6, wellPanel(
        
        selectInput("area",
                    "Select County:",
                    c("California",
                          unique(as.character((dat$Area_Name))))
        ),
        
        dateInput('date1', 
                label = 'Start Date: (Date will typically round to the previous 1st of the month)',
                value = ""
        ),

        dateInput('date2',
                label = 'End Date: (Date will typically round to the previous 1st of the month)',
                value = ""
        )
    )
    ),

    
    
    column(5, 
       wellPanel(
        textOutput("msg1"), 
        textOutput("msg2"),
        textOutput("msg3"),
      )
    ),


  mainPanel(
  
    plotOutput("plot1"),

    textInput(
      inputId = "filename",
      placeholder = "Name download file",
      label = ""
    ),
  
    downloadButton(
      outputId = "dl",
      label = "Download PNG",
      icon = icon("download"),
      style = "color: black; margin-left: 15px; margin-bottom: 5px;"
  
    ),
  )
))







server <- function(input, output, session) {
  
#MODAL INTRO
    
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("Intro.html"),
      easyClose = FALSE,
      footer = modalButton("Dismiss")
    ))
  })
  
  
# UPDATE UI SESSION
  
  observe({  
  selectedarea = reactive({
    dat <- read_excel("Local_Area_Unemployment_Statistics__LAUS_.xlsx")
    dat$Date <- ymd(dat$Date)
    area <- dat[c(dat$Area_Name == input$area), ]
    area_and_Date = area$Date
    area_and_Date
  })
  
    updateDateInput(
      session,
      "date1",
      value = "",
      min = min(selectedarea()),
      max = max(selectedarea())
    )
  
  
    updateDateInput(
      session,
      "date2",
      value = "",
      min = min(selectedarea()),
      max = max(selectedarea())
    )
  })
  
#RENDERING
    output$msg1 = renderText({
        sprintf("Rendered data based for the following:")
    })
    
    output$msg2 = renderText({
        sprintf("Area Name: %s", input$area)
    })
    
    output$msg3 = renderText({
        sprintf("Date Range: %s to %s:", input$date1, input$date2)
    })
    
    
#PLOT (Look at #REACTIVE above)
    
    plot_unemploy <- reactive({
      dat <- read_excel("Local_Area_Unemployment_Statistics__LAUS_.xlsx")
      dat$Date <- ymd(dat$Date)
      #save just in case: p = dat[order(as.Date(dat$Date, format="%Y-%m-%d", origin = "%Y/%m/%d")), ]
      p1 = dat[c(dat$Area_Name == input$area), ]
      p2 = p1[p1$Date >= input$date1 & p1$Date <= input$date2, ]
      p3 = p2[, c("Date", "Labor_Force", "Unemployment", "Employment", "Unemployment_Rate")]
      title = paste(input$area, " Unemployment Flow from ", input$date1, " to ", input$date2)
      sub = paste("x-lab `date` subject to change to limited given observations")
      g = ggplot(p3, aes(Date, Unemployment)) + geom_line(color = "blue") + theme_minimal() + ggtitle(title, sub)
      print(g) 
    })    
    

    output$plot1 = renderPlot({
      plot_unemploy()
    })
    

    
#DOWNLOAD
    
    download_box <- function(exportname, plot) {
      downloadHandler(
        filename = function() {
          paste(exportname, "_", input$area, "_unemployment_",input$date1, "_to_", input$date2, ".png", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plot, device = "png", width = 8)
        }
      )
    }
    
    output$dl <- download_box(input$filename, plot_unemploy())

}
    


shinyApp(ui = ui, server = server)



