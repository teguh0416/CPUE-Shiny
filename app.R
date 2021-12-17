library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(lubridate)
library(padr)
library(readxl)
library(ggplot2)
library(dplyr)
library(config)

setwd("D:/Data Sains/Data CPUE")
mydata <- read_excel("CPUE_palabuhanratu.xlsx")
mydata$tanggal <- as.Date(mydata$tanggal)


cpue <- mydata %>% group_by(tahun = format(as.Date(mydata$tanggal), format = "%Y"),ikan,alat_tangkap) %>% 
  summarise(total_produksi = sum(produksi),total_trip = n(), cpue1= total_produksi/total_trip
            , .groups= "drop") %>% filter(ikan=="Layur" & alat_tangkap=="Pancing Ulur") 

ui <- shinyUI(
  dashboardPage(
    skin = "black-light",
    title = "Sumberdaya Ikan",
    dashboardHeader(
      title = "Sumberdaya Ikan Palabuhanratu"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "CPUE",
          tabName = "page1",
          badgeLabel = "new",
          badgeColor = "green",
          icon = icon("gear")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          
          fluidRow(
            box(
              title = "Select Fishing Gears",
              closable = TRUE,
              enable_label = TRUE,
              status = "primary",
              solidHeader = FALSE,
              width = 12,
              
              selectInput(
                inputId = "catgear",
                label = "Select Gears",
                choices = c(cpue$alat_tangkap),
                selected = "Pancing Ulur"
              ),
              
              dataTableOutput(
                outputId = "cpue"
              )
              
            )
          ),
          fluidRow(
            box(
                title = "CPUE Graph",
                closable = TRUE, 
                enable_label = TRUE,
                label_status = "danger",
                status = "primary", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                width = 12,
                height = "600px",
                
                plotOutput(
                  outputId = "graph"
                )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
 
  output$cpue <- renderDataTable({
    datatable(cpue)
  })
  
  output$graph <-renderPlot({
    
    ggplot(cpue, aes(x=tahun, y=cpue1, group=1)) + geom_line(size=0.75) + geom_point(colour='blue',size=3) +
      labs(title = "CPUE Ikan Layur, Alat Tangkap Pancing Ulur",
           subtitle = "Based On Logbook (Bagian Statistik PPN Palabuhanratu)",
           caption = "IG: @teguhsantausa") + xlab("Tahun") + ylab("CPUE (Kg/Trip)")+
      geom_text(aes(label=sprintf("%0.2f", round(cpue1, digits = 2)), vjust=-0.8))+
      ylim(c(0,90))
      
  })
   
}

shinyApp(ui, server)
