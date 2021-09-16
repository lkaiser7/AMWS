### UI for Avian Malaria Warning App ###

#----------------#
##### SET UP #####
#----------------#

# load necessary packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(plyr)
library(darksky)

# set working directory to app location
# amDir<-"K:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega hard drive
# amDir<-"D:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega personal pc
# DO NOT RUN setwd() ON SHINY SERVER (DOES NOT EXIST): setwd(amDir)

# list stations by ID
stn<-c('Alakai', # Kauai,
       # 'Nakula', # Maui,
       # 'Waikamoi', # Maui,
       'Hanawi', # Maui,
       'Hakalau Lower') # Hawaii

#------------#
##### UI #####
#------------#

shinyUI<-fluidPage(theme = shinythemes::shinytheme("united"), # END theme
                   titlePanel("Avian Malaria Warning System"), # END titlePanel
                   tags$h4(tags$em("Please allow up to 1 minute for new data to load"),
                     style = "color:#dd4814;"),
                   tags$hr(),
                   
                   fluidRow(
                     
                     column(12, tabsetPanel(
                       
                       tabPanel("Overview",
                                column(4, wellPanel(plotOutput("state_map") # END plotOutput
                                                    ) # END wellPanel
                                ), # END column
                                
                                column(8, tags$h4(tags$strong("Current Avian Malaria Conditions",
                                                              style = "color:#dd4814")), 
                                       tableOutput("conditions"),
                                       br(),
                                       p(),
                                       tags$h5(tags$strong("Data Sources and Citations",
                                                           style = "color:#dd4814")),
                                       textOutput("data_info"),
                                       # em(code("NOTE: For data sentsitivity purposes, random jitter has 
                                       #               been added to the mapped point locations displayed. The point locations
                                       #               do not necessarily indicated exactly where ROD has been sampled or detected.
                                       #               To view the precise location data, please enter the password.
                                       #               The data and analysis shown here have inherent caveats and limitations that are 
                                       #               well described in the associated manuscript [DOI LINK].")), 
                                       uiOutput("bc_doi")) # END column
                                ), 
                       
                       tabPanel("Alakai",
                                wellPanel(
                                  tags$h4(tags$strong("Current Status at Alakai",                                  
                                                  style = "color:#dd4814;")),
                                          tags$h4(textOutput("kauai_status"), 
                                                  style = "color:#dd4814;")
                                  ), # END wellPanel
                                
                                dateRangeInput("kauai_range", "Select Date Range:",
                                               start = Sys.Date()+90-365,
                                               end = Sys.Date() + 90,
                                               min = "1950-02-01",
                                               max = Sys.Date() + 90,
                                               format = "mm/dd/yyyy"),
                                textOutput("date_check1"),
                                tags$head(tags$style("#date_check1{
                                                     color: #dd4814;}")),
                                plotOutput("warning_kauai"),
                                tags$hr(),
                                tags$h4(tags$strong("Current 3-Month Avian Malaria Forecast")),
                                tags$h5(tags$em("(number of projected days per suitability status)")),
                                tableOutput("kauai_forecast"),
                                tags$hr(),
                                tags$h4(tags$strong("Ratio of Avian Malaria Status Days (%)")),
                                tableOutput("kauai_table")
                                ),
                       
                       # tabPanel("Nakula",
                       #          wellPanel(
                       #            tags$h4(tags$strong("Current Status at Nakula",                                  
                       #                                style = "color:#dd4814;")),
                       #            tags$h4(textOutput("maui_status"), 
                       #                    style = "color:#dd4814;")
                       #          ), # END wellPanel
                       #          
                       #          dateRangeInput("maui_range", "Select Date Range:",
                       #                         start = Sys.Date()+90-365,
                       #                         end = Sys.Date() + 90,
                       #                         min = "1973-01-01",
                       #                         max = Sys.Date() + 90,
                       #                         format = "mm/dd/yyyy"),
                       #          textOutput("date_check2"),
                       #          tags$head(tags$style("#date_check2{
                       #                               color: #dd4814;}")),
                       #          plotOutput("warning_maui"),
                       #          tags$hr(),
                       #          tags$h4(tags$strong("Current 3-Month Avian Malaria Forecast")),
                       #          tags$h5("(number of projected days per suitability status)"),
                       #          tableOutput("maui_forecast"),
                       #          tags$hr(),
                       #          tags$h4(tags$strong("Ratio of Avian Malaria Status Days (%)")),
                       #          tableOutput("maui_table")
                       #          ),
                       
                       tabPanel("Hakalau",
                                wellPanel(tags$h4(tags$strong("Current Status at Hakalau",                                  
                                                              style = "color:#dd4814;")),
                                          tags$h4(textOutput("hawaii_status"), 
                                                  style = "color:#dd4814;")),
                                
                                dateRangeInput("hawaii_range", "Select Date Range:",
                                               start = Sys.Date()+90-365,
                                               end = Sys.Date() + 90,
                                               min = "1973-01-01",
                                               max = Sys.Date() + 90,
                                               format = "mm/dd/yyyy"),
                                textOutput("date_check3"),
                                tags$head(tags$style("#date_check3{
                                                     color: #dd4814;}")),
                                plotOutput("warning_hawaii"),
                                tags$hr(),
                                tags$h4(tags$strong("Current 3-Month Avian Malaria Forecast")),
                                tags$h5("(number of projected days per suitability status)"),
                                tableOutput("hawaii_forecast"),
                                tags$hr(),
                                tags$h4(tags$strong("Ratio of Avian Malaria Status Days (%)")),
                                tableOutput("hawaii_table")
                                ),
                       
                       # tabPanel("Waikamoi",
                       #          wellPanel(
                       #            tags$h4(tags$strong("Current Status at Waikamoi",                                  
                       #                                style = "color:#dd4814;")),
                       #            tags$h4(textOutput("maui_status2"), 
                       #                    style = "color:#dd4814;")
                       #          ), # END wellPanel
                       #          
                       #          dateRangeInput("maui_range2", "Select Date Range:",
                       #                         start = Sys.Date()+90-365,
                       #                         end = Sys.Date() + 90,
                       #                         min = "1973-01-01",
                       #                         max = Sys.Date() + 90,
                       #                         format = "mm/dd/yyyy"),
                       #          textOutput("date_check0"),
                       #          tags$head(tags$style("#date_check0{
                       #                               color: #dd4814;}")),
                       #          plotOutput("warning_maui2"),
                       #          tags$hr(),
                       #          tags$h4(tags$strong("Current 3-Month Avian Malaria Forecast")),
                       #          tags$h5("(number of projected days per suitability status)"),
                       #          tableOutput("maui_forecast2"),
                       #          tags$hr(),
                       #          tags$h4(tags$strong("Ratio of Avian Malaria Status Days (%)")),
                       #          tableOutput("maui_table2")
                       #          )
                       
                       tabPanel("Hanawi",
                                wellPanel(
                                  tags$h4(tags$strong("Current Status at Hanawi",                                  
                                                      style = "color:#dd4814;")),
                                  tags$h4(textOutput("maui_status2"), 
                                          style = "color:#dd4814;")
                                ), # END wellPanel
                                
                                dateRangeInput("maui_range2", "Select Date Range:",
                                               start = Sys.Date()+90-365,
                                               end = Sys.Date() + 90,
                                               min = "1950-12-01",
                                               max = Sys.Date() + 90,
                                               format = "mm/dd/yyyy"),
                                textOutput("date_check0"),
                                tags$head(tags$style("#date_check0{
                                                     color: #dd4814;}")),
                                plotOutput("warning_maui2"),
                                tags$hr(),
                                tags$h4(tags$strong("Current 3-Month Avian Malaria Forecast")),
                                tags$h5("(number of projected days per suitability status)"),
                                tableOutput("maui_forecast2"),
                                tags$hr(),
                                tags$h4(tags$strong("Ratio of Avian Malaria Status Days (%)")),
                                tableOutput("maui_table2")
                                )
                       
                       ) # END tabsetPanel
                       ) # END column
                     ), # END fluidRow
                   
                   fluidRow(
                     column(4#, wellPanel(class = "wp2",
                                         #tags$img(height = "150%", width = "100%",
                                         #         src = "warning_levels.jpg"),
                                         #tags$head(tags$style(
                                         # ".wp2{height:500px;}")) # END tags
                     #) # END wellPanel
                     ) # END column               
                   ) # END fluidRow
                   
)# END fluidPage

#-----------------------#
##### END UI SCRIPT #####
#-----------------------#