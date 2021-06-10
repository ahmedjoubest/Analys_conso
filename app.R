library(shiny)
library(shinydashboard)
library(highcharter)
library(data.table)
library(lubridate)
library(dplyr, warn.conflicts = FALSE)
library(utc)
library(xts)
library(DT)



source("helpers.R")

header <- dashboardHeader(title = "Modélisation IPMVP")
sidebar <- dashboardSidebar(
  # h4(tags$u("Charger les données de consommation :"),align = "center"),
  
  fileInput("file_conso", "1- Charger les données de consommation",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  
  dateRangeInput("date_reference",
                 "2- Choisir la période de référence"),
  
  dateRangeInput("date_suivi",
                 "3- Choisir la période de suivi"),
  
  fileInput("file_X", "4- Charger les données des DJU",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  dateInput("APE_date_1", label = "5- Choisir la date de la première APE"),
  dateInput("APE_date_2", label = "6- Choisir la date de la 2ième APE (facultatif)"),
  dateInput("APE_date_3", label = "7- Choisir la date de la 3ième APE (facultatif)"),
  
  
  
  
  br(),
  
  div(
    align = 'center',
    actionButton(inputId = "submit",
                 label =  "8- Modéliser")
  )
)

body <- dashboardBody(
  
  
  fluidRow(
    # Heatmap
    box(
      title = "Intensité de consommation",
      # Intervalle date
      uiOutput("daterange_heatmap"),
      # Type d'aggregation
      div(
        align = 'center',
        radioButtons("Aggregation", "Aggregation :",
                     c("heure","10 minutes"))
      )
      ,
      
      # Heat map output
      highchartOutput("heatmap"), width=12,status="primary", solidHeader = T),
    
    box(
      title = "IPMVP", width=12,status="primary", solidHeader = T,
      
      br(),
      div(
        align = 'center',
        textOutput("R2")
      ),
      br(),
      
      highchartOutput("ipmvp")
      
    ),
    div( align ='center',
         p("Un jeu de données exemple pour les données de Conso est téléchargeable ", strong(tags$a("ICI",href = "link1"))),
         p("Un jeu de données exemple pour les données de DJU est téléchargeable ", strong(tags$a("ICI",href = "link1"))),
    )
  )
  
)

ui <- 
  dashboardPage(header, sidebar, body )



# Server : 

server <- function(input, output) {
  
  DT <- eventReactive(input$submit,{
    read.csv2(input$file_conso$datapath, encoding = "UTF8") %>% 
      format_conso()
  })
  
  DT_ipmvp <- eventReactive(input$submit,{
    read.csv2(input$file_conso$datapath, encoding = "UTF8") %>% 
      format_conso_ipmvp()
  })
  
  DT_DJU <- eventReactive(input$submit,{
    read.csv2(input$file_X$datapath, encoding = "UTF8")
  })
  
  # Intervalle de date heatmap
  output$daterange_heatmap <- renderUI({
    dateRangeInput("daterange_heatmap",
                   "Intervalle de temps",
                   start = max(DT()$Time_day)-12,
                   end = max(DT()$Time_day))
  })
  
  ### Heatmap
  output$heatmap <-
    renderHighchart({
      Build_HC_Temporal_heatmap(DT(),Date_debut = input$daterange_heatmap[1],
                                Date_fin = input$daterange_heatmap[2],
                                Aggregation = input$Aggregation)
    })
  
  ### ipmvp plot
  output$ipmvp <-
    renderHighchart({
      IPMVP_plot(DT_DJU(),DT_ipmvp(), input$date_reference[1], input$date_reference[2],
                 input$date_suivi[1], input$date_suivi[2], input$APE_date_1, input$APE_date_2,
                 input$APE_date_3)$highchart
    })
  
  output$R2 <-
    renderText({
      IPMVP_plot(DT_DJU(),DT_ipmvp(), input$date_reference[1], input$date_reference[2],
                 input$date_suivi[1], input$date_suivi[2], input$APE_date_1, input$APE_date_2,
                 input$APE_date_3)$R2
    })
  
  
}


shinyApp(ui, server)

