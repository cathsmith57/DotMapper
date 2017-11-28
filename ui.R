#----------------------------------------
# Check and load packages
#----------------------------------------

#list of packages required
list.of.packages <- c("dplyr","tidyr","lubridate",
                      "leaflet","RColorBrewer","zoo",
                      "epitools","ggplot2","shinyjs", 
                      "shinydashboard")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(zoo)
library(epitools)
library(ggplot2)
library(shinyjs)
library(shinydashboard)

#----------------------------------------
# Dashboard elements
#----------------------------------------

header <- dashboardHeader(title="DotMapper")
sidebar <- dashboardSidebar(
  sidebarMenu(id="nav",
              menuItem(text="Input", tabName = "navinput", icon = icon("database"), selected=TRUE),
              menuItem(text="Map", tabName = "navmap", icon = icon("database")),
              menuItem(text="Table", tabName = "navtable", icon = icon("globe")),
              menuItem(text="Bar chart", tabName = "navbar", icon=icon("bar-chart"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    
    #----------------------------------------
    # Input
    #----------------------------------------
    
    tabItem(tabName="navinput", 
            useShinyjs(),
            fluidRow(
              column(width=6,
                     box(title="1. Load data", width=NULL, status="primary", solidHeader = T, 
                         column(width=6,
                                fileInput('fileCase', label="Load cases", accept=c("csv"))
                                ),
                         column(width=6,
                                fileInput("fileVenue", label="Load venues (optional)", accept=c("csv"))
                                )
                     )
              ),
              column(width=6,
                     tags$head(
                       tags$style(HTML('#gen{background-color:orange}'))),
                     textOutput("warn"),
                     actionButton("gen", label="Go")
              )
            ),
            fluidRow(
              column(width=6,
                     box(title="2. Preview data", width=NULL, status="primary", solidHeader=T, 
                         tabBox(width=NULL, side="left", 
                                tabPanel(title="Cases", value="casPrev", 
                                         conditionalPanel(condition="output.caseFileUploaded",
                                                          div(style='overflow-y:scroll; overflow-x: scroll; height:500px',
                                                              
                                                              DT::dataTableOutput('previewCase')
                                                          )
                                         )
                                ),
                                tabPanel(title="Venues", value="venPrev", 
                                         conditionalPanel(condition="output.venueFileUploaded",
                                                          div(style='overflow-y:scroll; overflow-x: scroll; height:500px',
                                                              DT::dataTableOutput('previewVenue')
                                                          )
                                         )
                                )
                         )
                     )
              ),
              column(width=6,
                     box(title="3. Identify variables", width=NULL, status="primary", solidHeader=T,
                         tabBox(width=NULL, side="left",
                                tabPanel(title="Cases", value="casTab",
                                         conditionalPanel(condition="output.caseFileUploaded",
                                           div(style = 'overflow-y: scroll; height:500px',
                                               uiOutput("caseidUi"),
                                               uiOutput("casedateUi"),
                                               uiOutput("catvarUi"),
                                               uiOutput("caselatUi"),
                                               uiOutput("caselonUi"),
                                               textOutput("caselatlon")
                                           )
                                         )
                                ),
                                tabPanel(title="Venues", value="venTab", 
                                         conditionalPanel(condition="output.venueFileUploaded",
                                           div(style = 'overflow-y: scroll; height:500px',
                                               uiOutput("venueidUi"),
                                               uiOutput("venuetypeUi"),
                                               uiOutput("venuenameUi"),
                                               uiOutput("venuelatUi"), 
                                               uiOutput("venuelonUi"),
                                               textOutput("venuelatlon")
                                           )
                                         )
                                )
                         )
                     )
              )
            )
    ),
    
    #----------------------------------------
    # Interactive map
    #----------------------------------------
    
    tabItem(tabName="navmap",
            tags$style(type = "text/css", "#datamap {height: calc(100vh - 80px) !important;}"),
            leafletOutput("datamap")
    ),
    
    #----------------------------------------
    # Table
    #----------------------------------------
    
    tabItem(tabName="navtable",
            column(width=8,
                   box(title="Data table", width=NULL, status="primary", solidHeader=T,
                       DT::dataTableOutput("sumTable"),
                       tags$head(tags$style(type="text/css", "overflow-x: scroll; height:200px; overflow-y: scroll #sumTable table td{line-height:'10px';}"))                
                       )
                   )
            ),
    
    #----------------------------------------
    # Bar chart
    #----------------------------------------
    
    tabItem(tabName="navbar",
#            column(width=8, 
                   box(title="Bar chart", width=NULL, status="primary", solidHeader=T,
                       uiOutput("epiUi"))
#                       plotOutput("epi"))
#                   )
    )
  ),
  
  #----------------------------------------
  # Controls
  #----------------------------------------
  
  conditionalPanel(condition="input.nav!='navinput'",
                   tags$head(tags$style(type="text/css", 
                   '#controls1 {
                     /* Appearance */
                     background-color: white;
                     padding: 20px 20px 20px 20px;
                     opacity: 0.85;
                     border-color: #3c8dbc;
                     border-radius: 10px;
                     border-width: 5px; 
                     }')),
                   absolutePanel(id = "controls1", class = "panel panel-default",
                                 draggable = T, top = 60, left = 'auto', right = 20, bottom = "auto",
                                 width = 330, height = "auto",
                                 style="overflow-y:scroll; max-height: 400px",
                                 radioButtons("durChoice", "Choose time grouping", 
                                              choices=c("Year", "Quarter", "Month", "Day"),
                                              inline=T, selected="Year"),
                                 conditionalPanel(condition="input.nav=='navbar'",
                                                  sliderInput("plWid", label="Plot width (px)", min=50, max=1000, value=400),
                                                  sliderInput("plHt", label="Plot height (px)", min=50, max=1000, value=400)
                                 ),
                                 conditionalPanel(condition="input.nav=='navmap'",
                                                  uiOutput("varcolUi"),
                                                  conditionalPanel(condition="output.venueFileUploaded",
                                                                   radioButtons("disContext", "Display contextual locations:", 
                                                                                choices=c("Yes", "No"), inline=T, selected="No")
                                                  )
                                 ),
                                 radioButtons("disCase", "Display cases:", choices=c("All", "Subset"), inline=T),
                                 conditionalPanel(
                                   condition="input.disCase == 'Subset'",
                                   uiOutput("dateRangeUi"),
                                   actionButton("reset", "Reset groups"),
                                   uiOutput("inputUI"),
                                   uiOutput("filUi")
                                 )
                   )
  )
)

dashboardPage(header, sidebar, body)























