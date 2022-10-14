library(shiny)
library(shinydashboard)
library(highcharter)
library(data.table)
library(lubridate)
library(dplyr, warn.conflicts = FALSE)
library(utc)
library(xts)
library(DT)
library(shinycssloaders)


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
                 "2- Choisir la période de référence",start = "2020-10-01",
                   end = "2021-02-28"),
  
  dateRangeInput("date_suivi",
                 "3- Choisir la période de suivi", start = "2020-12-01",
                 end = "2021-06-11"),
  
  fileInput("file_X", "4- Charger les données des DJU",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  dateInput("APE_date_1", label = "5- Choisir la date de la première APE",value = "2021-01-01"),
  dateInput("APE_date_2", label = "6- Choisir la date de la 2ième APE (facultatif)", value = "2021-01-15"),
  dateInput("APE_date_3", label = "7- Choisir la date de la 3ième APE (facultatif)", value = "2021-02-02"),
  
  
  
  
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
      highchartOutput("heatmap")%>% 
        withSpinner(color="#3C8DBC",type=4, proxy.height = "460px",size = 0.5), width=12,status="primary", solidHeader = T),
    
    box(
      title = "IPMVP", width=12,status="primary", solidHeader = T,
      
      br(),
      div(
        align = 'center',
        textOutput("R2")
      ),
      br(),
      
      highchartOutput("ipmvp")%>% 
        withSpinner(color="#3C8DBC",type=4, proxy.height = "400px",size = 0.5)
      
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
  
  DT <- eventReactive(input$submit,ignoreNULL = F,{
    read.csv2("Enedis_SGE_HDM_A05VF6RP.csv", encoding = "UTF8") %>% 
      format_conso()
  })
  
  DT_ipmvp <- eventReactive(input$submit,ignoreNULL = F,{
    read.csv2("Enedis_SGE_HDM_A05VF6RP.csv", encoding = "UTF8") %>% 
      format_conso_ipmvp()
  })
  
  DT_DJU <- eventReactive(input$submit,ignoreNULL = F,{
    read.csv2("infoclimat-2020-07-01-2022-07-31-paris-montsouris.csv", encoding = "UTF8")
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

