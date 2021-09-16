### SERVER for Avian Malaria Warning App ###

#----------------#
##### SET UP #####
#----------------#

# load necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plyr)
library(darksky)
library(formattable)
library(rgdal)

# set working directory to app location
# amDir<-"G:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega hard drive
# amDir<-"D:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega personal pc
# DO NOT RUN setwd() ON SHINY SERVER (DOES NOT EXIST): setwd(amDir)

# list stations by ID
stn<-c('Alakai', # Kauai,
       # 'Nakula', # Maui,
       # 'Waikamoi', # Maui,
       'Hanawi', # Maui,
       'Hakalau Lower') # Hawaii

#--------------------#
###### LOAD DATA #####
#--------------------#

# collect most up-to-date station data from NOAA ftp site
# source("1_darksky_forecast.R")
source("1_noaa_forecast.R")

# run to update suitability in real-time
source("2_am_warning.R")

# load station information file
stn_info<-read.csv("stns_list_info.csv", header = T)

# load state coasts shapefile
hi_coast<-readOGR("hawaii_coast", "Main_Hawaiian_Islands_simple3")

# load suitability data
kauai_suitability<-read.csv("Alakai_suitability.csv", header = T)
# maui_suitability<-read.csv("Nakula_suitability.csv", header = T)
# maui2_suitability<-read.csv("Waikamoi_suitability.csv", header = T)
maui2_suitability<-read.csv("Hanawi_suitability.csv", header = T)
hawaii_suitability<-read.csv("Hakalau Lower_suitability.csv", header = T)

# format date columns
kauai_suitability$DATE = as.Date(kauai_suitability$DATE)
# maui_suitability$DATE = as.Date(maui_suitability$DATE)
maui2_suitability$DATE = as.Date(maui2_suitability$DATE)
hawaii_suitability$DATE = as.Date(hawaii_suitability$DATE)

# set factor level order
suit_status<-c("Not suitable for vector or disease development",
               "Suitable for Culex vector development only",
               # "Suitable for Malaria disease development only",
               "Suitable for both disease and vector development")

# reorder factor levels 
kauai_suitability$am_state<-factor(kauai_suitability$am_state, levels = suit_status)
# maui_suitability$am_state<-factor(maui_suitability$am_state, levels = suit_status)
maui2_suitability$am_state<-factor(maui2_suitability$am_state, levels = suit_status)
hawaii_suitability$am_state<-factor(hawaii_suitability$am_state, levels = suit_status)

# create color scale for warning state
kauai_colors<-c("black", "blue", "red")
maui_colors<-c("black", "blue", "red")
hawaii_colors<-c("black", "blue", "red")
# set color scale for data
names(kauai_colors)<-levels(as.factor(kauai_suitability$am_state))
# names(maui_colors)<-levels(as.factor(maui_suitability$am_state))
names(maui_colors)<-levels(as.factor(maui2_suitability$am_state))
names(hawaii_colors)<-levels(as.factor(hawaii_suitability$am_state))

# select point shapes per data type
kauai_shapes<-c(24, 21, 22)
maui_shapes<-c(24, 21, 22)
hawaii_shapes<-c(24, 22, 21)
# set shape scale for data
names(kauai_shapes)<-levels(as.factor(kauai_suitability$STATION))
# names(maui_shapes)<-levels(as.factor(maui_suitability$STATION))
names(maui_shapes)<-levels(as.factor(maui2_suitability$STATION))
names(hawaii_shapes)<-levels(as.factor(hawaii_suitability$STATION))

#----------------#
##### SERVER #####
#----------------#

shinyServer<-function(input, output){
  
  # hide (T) or show (F) error text in app
  options(shiny.sanitize.errors = F)
  
  ####################
  ##### OVERVIEW #####
  ####################
  
  #-----STATION MAP-----#
  output$state_map<-renderPlot({
    plot(hi_coast, col = adjustcolor("darkgreen", alpha.f = 0.7), 
         border = "seashell4", bg = "aliceblue", main = "Station Locations")
    points(x = stn_info$LONGITUDE, y = stn_info$LATITUDE, pch = 21, bg = "yellow")
    text(x = stn_info$LONGITUDE[1], y = stn_info$LATITUDE[1], label = stn_info$LOCATION[1], 
         col = 'black', pos = 4, offset = 0.25, cex = 0.85, font = 2)  # Alakai
    text(x = stn_info$LONGITUDE[2], y = stn_info$LATITUDE[2], label = stn_info$LOCATION[2], 
         col = 'black', pos = 4, offset = 0.25, cex = 0.85, font = 2)  # Hanawi
    # text(x = stn_info$LONGITUDE[3], y = stn_info$LATITUDE[3]-0.05, label = stn_info$LOCATION[3], 
    #      col = 'black', pos = 4, offset = 0.25, cex = 0.85, font = 2)  # Nakula
    text(x = stn_info$LONGITUDE[3], y = stn_info$LATITUDE[3], label = stn_info$LOCATION[3], 
         col = 'black', pos = 4, offset = 0.25, cex = 0.85, font = 2)  # Hakalau
    
    text(x = -159.5, y = 21.75, label = "KAUA'I", col = "seashell4")
    text(x = -158.5, y = 21.35, label = "O'AHU", col = "seashell4")
    text(x = -157.05, y = 20.625, label = "MAUI", col = "seashell4")
    # text(x = -157.05, y = 20.475, label = "COUNTY", col = "seashell4")
    text(x = -156.5, y = 19.5, label = "HAWAI'I", col = "seashell4")
  })
  
  #-----WEATHER CONDITIONS-----#
  # table current vector and disease conditions
  output$conditions<-renderTable(spacing = "l",
    data.frame(Date = as.character(Sys.Date()), 
               Alakai = kauai_suitability$am_state[which(kauai_suitability$DATE == Sys.Date())], 
               # Nakula = maui_suitability$am_state[which(maui_suitability$DATE == Sys.Date())],
               # Waikamoi = maui2_suitability$am_state[which(maui2_suitability$DATE == Sys.Date())],
               Hanawi = maui2_suitability$am_state[which(maui2_suitability$DATE == Sys.Date())],
               Hakalau = hawaii_suitability$am_state[which(hawaii_suitability$DATE == Sys.Date())])
   ) # END renderTable
  
  # data disclaimer of sources for warning system
  output$data_info<-renderText(
    paste("Data for this warning system is a compilation of NOAA NCDC 
           Global Historical Climatology Network data (https://www.ncdc.noaa.gov), 
           Dark Sky API 2-week forecast data (https://darksky.net), 
           and calculated historical annual climatic averages based on the NOAA NCDC data.
           All data has been statistically adjust for each individual site location based on collected site data.
           
           For additional information regarding this data and analysis, please see the associated manuscript [DOI]:")
    # For more information, please see CITATION.")
  ) # END renderText
  
  bc_url<-a("Fostering real-time climate adaptation: Analyzing past, current, and forecast temperature 
            to understand the dynamic risk to Hawaiian honeycreepers from avian malaria",
             href = "https://www.journals.elsevier.com/global-ecology-and-conservation")
  output$bc_doi<-renderUI({
    tagList(bc_url)
  })
  
  #########################
  ##### TABSET PANELS #####
  #########################
    
  #-----KAUAI-----#
  
  # update Kauai status
  output$kauai_status<-renderText(
    paste0("As of ", Sys.Date(), ": ", 
          kauai_suitability$am_state[which(kauai_suitability$DATE == Sys.Date())])
    ) # END renderText
  
  # table forecast
  output$kauai_forecast<-renderTable(
    # data.frame(Date = as.character(kauai_suitability$DATE[which(kauai_suitability$DATE >= Sys.Date())]),
    #            Forecast = kauai_suitability$am_state[which(kauai_suitability$DATE >= Sys.Date())])
    table(kauai_suitability$am_state[which(kauai_suitability$DATE >= Sys.Date())]),
    colnames = FALSE
  ) # END renderTable  
  
  # create reactive dataset based on selected dates
  ka_range<-reactive({    
    kauai_suitability[which(kauai_suitability$DATE >= input$kauai_range[1] &
                               kauai_suitability$DATE <= input$kauai_range[2]),]
  }) # END reactive
  
  # check date range is valid
  output$date_check1<-renderText({
    validate(need(input$kauai_range[2] > input$kauai_range[1], 
                  "The selected end date is earlier than the start date. Please select a correct date range to display data."))
  }) # END renderText
  
  # plot warning levels
  output$warning_kauai<-renderPlot({
    
    # plot suitability over selected date range
    kauai_warn = ggplot(ka_range(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
      geom_point(size = 2.5) + scale_colour_manual(name = "Avian Malaria Status", values = kauai_colors) +
      scale_shape_manual(name = "Data Source", values = kauai_shapes,
                         labels = c("14-Day Weather Forecast", "Annual Climatic Average", "Adjusted Airport Data")) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
      ggtitle("Alakai") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    
    # add forecast box
    kauai_warn_box = ggplot(ka_range(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
      geom_rect(aes(xmin = Sys.Date() - 0.5, xmax = Sys.Date() + 92, 
                    ymin = min(ka_range()$lm_TADJ, na.rm = T) - 0.5, 
                    ymax = max(ka_range()$lm_TADJ, na.rm = T) + 0.5),
                linetype = 5, color = "olivedrab", size = 1, fill = "transparent") +
      annotate("text", x = Sys.Date() + 50, y = max(ka_range()$lm_TADJ, na.rm = T) + 1,
               label = "FORECAST", color = "olivedrab", size = 4) +
      geom_point(size = 2.5) + scale_colour_manual(name = "Avian Malaria Status", values = kauai_colors) +
      scale_shape_manual(name = "Data Source", values = kauai_shapes,
                         labels = c("14-Day Weather Forecast", "Annual Climatic Average", "Adjusted Airport Data")) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
      ggtitle("Alakai") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    
    # plot box if within chosen date range
    if(input$kauai_range[1] >= Sys.Date() | input$kauai_range[2] >= Sys.Date()){
      kauai_warn_box
    }else{
      kauai_warn
    }
  
  }) # END renderPlot
  
  # table Kauai data for the selected date range
  output$kauai_table<-renderTable(rownames = TRUE,
                                  cbind(ANNUAL = round(table(ka_range()$am_state)/sum(table(ka_range()$am_state))*100, 2),
                                        WINTER = round(table(ka_range()$am_state[which(as.numeric(format(ka_range()$DATE, format = "%m")) <= 4 |
                                                                                         as.numeric(format(ka_range()$DATE, format = "%m")) >= 11)])/
                                                         sum(table(ka_range()$am_state[which(as.numeric(format(ka_range()$DATE, format = "%m")) <= 4 |
                                                                                               as.numeric(format(ka_range()$DATE, format = "%m")) >= 11)]))*100, 2),
                                        SUMMER = round(table(ka_range()$am_state[which(as.numeric(format(ka_range()$DATE, format = "%m")) >= 5 &
                                                                                         as.numeric(format(ka_range()$DATE, format = "%m")) <= 10)])/
                                                         sum(table(ka_range()$am_state[which(as.numeric(format(ka_range()$DATE, format = "%m")) >= 5 &
                                                                                               as.numeric(format(ka_range()$DATE, format = "%m")) <= 10)]))*100, 2))
  ) # END renderTable
  
  #-----MAUI-----#
  
  # ### NAKULA ###
  # 
  # # update Maui status
  # output$maui_status<-renderText(
  #   paste0("As of ", Sys.Date(), ": ", 
  #          maui_suitability$am_state[which(maui_suitability$DATE == Sys.Date())])
  # ) # END renderText
  # 
  # # table forecast
  # output$maui_forecast<-renderTable(
  #   table(maui_suitability$am_state[which(maui_suitability$DATE >= Sys.Date())]),
  #   colnames = FALSE
  # ) # END renderTable  
  # 
  # # create reactive dataset based on selected dates
  # ma_range<-reactive({    
  #   maui_suitability[which(maui_suitability$DATE >= input$maui_range[1] &
  #                            maui_suitability$DATE <= input$maui_range[2]),]
  # }) # END reactive
  # 
  # # check date range is valid
  # output$date_check2<-renderText({
  #   validate(need(input$maui_range[2] > input$maui_range[1], 
  #                 "The selected end date is earlier than the start date. Please select a correct date range to display data."))
  # }) # END renderText
  # 
  # # plot warning levels
  # output$warning_maui<-renderPlot({
  #   
  #   # plot suitability over selected date range
  #   maui_warn = ggplot(ma_range(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
  #     geom_point(size = 2.5) + scale_colour_manual(name = "Avian Malaria Status", values = maui_colors) +
  #     scale_shape_manual(name = "Data Source", values = maui_shapes,
  #                        labels = c("14-Day Weather Forecast", "Annual Climatic Average", "Adjusted Airport Data")) +
  #     xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
  #     ggtitle("Nakula") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  #   
  #   # add forecast box
  #   maui_warn_box = ggplot(ma_range(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
  #     geom_rect(aes(xmin = Sys.Date() - 0.5, xmax = Sys.Date() + 92, 
  #                   ymin = min(ma_range()$lm_TADJ, na.rm = T) - 0.5, 
  #                   ymax = max(ma_range()$lm_TADJ, na.rm = T) + 0.5),
  #               linetype = 5, color = "olivedrab", size = 1, fill = "transparent") +
  #     annotate("text", x = Sys.Date() + 50, y = max(ma_range()$lm_TADJ, na.rm = T) + 1,
  #              label = "FORECAST", color = "olivedrab", size = 4) +
  #     geom_point(size = 2.5) + scale_colour_manual(name = "Avian Malaria Status", values = maui_colors) +
  #     scale_shape_manual(name = "Data Source", values = maui_shapes,
  #                        labels = c("14-Day Weather Forecast", "Annual Climatic Average", "Adjusted Airport Data")) +
  #     xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
  #     ggtitle("Nakula") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  #   
  #   # plot box if within chosen date range
  #   if(input$maui_range[1] >= Sys.Date() | input$maui_range[2] >= Sys.Date()){
  #     maui_warn_box
  #   }else{
  #     maui_warn  
  #   }
  #   
  # }) # END renderPlot
  # 
  # # table Maui data for the selected date range
  # output$maui_table<-renderTable(rownames = TRUE,
  #                                cbind(ANNUAL = round(table(ma_range()$am_state)/sum(table(ma_range()$am_state))*100, 2),
  #                                      WINTER = round(table(ma_range()$am_state[which(as.numeric(format(ma_range()$DATE, format = "%m")) <= 4 |
  #                                                                                       as.numeric(format(ma_range()$DATE, format = "%m")) >= 11)])/
  #                                                       sum(table(ma_range()$am_state[which(as.numeric(format(ma_range()$DATE, format = "%m")) <= 4 |
  #                                                                                             as.numeric(format(ma_range()$DATE, format = "%m")) >= 11)]))*100, 2),
  #                                      SUMMER = round(table(ma_range()$am_state[which(as.numeric(format(ma_range()$DATE, format = "%m")) >= 5 &
  #                                                                                       as.numeric(format(ma_range()$DATE, format = "%m")) <= 10)])/
  #                                                       sum(table(ma_range()$am_state[which(as.numeric(format(ma_range()$DATE, format = "%m")) >= 5 &
  #                                                                                             as.numeric(format(ma_range()$DATE, format = "%m")) <= 10)]))*100, 2))
  # ) # END renderTable
  # 
  
  ### HANAWI [WAIKAMOI] ###
  
  # update Maui status
  output$maui_status2<-renderText(
    paste0("As of ", Sys.Date(), ": ", 
           maui2_suitability$am_state[which(maui2_suitability$DATE == Sys.Date())])
  ) # END renderText
  
  # table forecast
  output$maui_forecast2<-renderTable(
    table(maui2_suitability$am_state[which(maui2_suitability$DATE >= Sys.Date())]),
    colnames = FALSE
  ) # END renderTable  
  
  # create reactive dataset based on selected dates
  ma_range2<-reactive({    
    maui2_suitability[which(maui2_suitability$DATE >= input$maui_range2[1] &
                            maui2_suitability$DATE <= input$maui_range2[2]),]
  }) # END reactive
  
  # check date range is valid
  output$date_check0<-renderText({
    validate(need(input$maui_range2[2] > input$maui_range2[1], 
                  "The selected end date is earlier than the start date. Please select a correct date range to display data."))
  }) # END renderText
  
  # plot warning levels
  output$warning_maui2<-renderPlot({
    
    # plot suitability over selected date range
    maui_warn2 = ggplot(ma_range2(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
      geom_point(size = 2.5) + scale_colour_manual(name = "Avian Malaria Status", values = maui_colors) +
      scale_shape_manual(name = "Data Source", values = maui_shapes,
                         labels = c("14-Day Weather Forecast", "Annual Climatic Average", "Adjusted Airport Data")) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
      ggtitle("Hanawi") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

    # add forecast box
    maui_warn_box2 = ggplot(ma_range2(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
      geom_rect(aes(xmin = Sys.Date() - 0.5, xmax = Sys.Date() + 92, 
                    ymin = min(ma_range2()$lm_TADJ, na.rm = T) - 0.5, 
                    ymax = max(ma_range2()$lm_TADJ, na.rm = T) + 0.5),
                linetype = 5, color = "olivedrab", size = 1, fill = "transparent") +
      annotate("text", x = Sys.Date() + 50, y = max(ma_range2()$lm_TADJ, na.rm = T) + 1,
               label = "FORECAST", color = "olivedrab", size = 4) +
      geom_point(size = 2.5) + scale_colour_manual(name = "Avian Malaria Status", values = maui_colors) +
      scale_shape_manual(name = "Data Source", values = maui_shapes,
                         labels = c("14-Day Weather Forecast", "Annual Climatic Average", "Adjusted Airport Data")) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
      ggtitle("Hanawi") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    
    # plot box if within chosen date range
    if(input$maui_range2[1] >= Sys.Date() | input$maui_range2[2] >= Sys.Date()){
      maui_warn_box2
    }else{
      maui_warn2  
    }
    
  }) # END renderPlot
  
  # table Maui data for the selected date range
  output$maui_table2<-renderTable(rownames = TRUE,
                                 cbind(ANNUAL = round(table(ma_range2()$am_state)/sum(table(ma_range2()$am_state))*100, 2),
                                       WINTER = round(table(ma_range2()$am_state[which(as.numeric(format(ma_range2()$DATE, format = "%m")) <= 4 |
                                                                                        as.numeric(format(ma_range2()$DATE, format = "%m")) >= 11)])/
                                                        sum(table(ma_range2()$am_state[which(as.numeric(format(ma_range2()$DATE, format = "%m")) <= 4 |
                                                                                              as.numeric(format(ma_range2()$DATE, format = "%m")) >= 11)]))*100, 2),
                                       SUMMER = round(table(ma_range2()$am_state[which(as.numeric(format(ma_range2()$DATE, format = "%m")) >= 5 &
                                                                                        as.numeric(format(ma_range2()$DATE, format = "%m")) <= 10)])/
                                                        sum(table(ma_range2()$am_state[which(as.numeric(format(ma_range2()$DATE, format = "%m")) >= 5 &
                                                                                              as.numeric(format(ma_range2()$DATE, format = "%m")) <= 10)]))*100, 2))
  ) # END renderTable
  
  #-----HAWAII-----#
  
  # update Hawaii status
  output$hawaii_status<-renderText(
    paste0("As of ", Sys.Date(), ": ",
           hawaii_suitability$am_state[which(hawaii_suitability$DATE == Sys.Date())])
  ) # END renderText
  
  # table forecast
  output$hawaii_forecast<-renderTable(
    table(hawaii_suitability$am_state[which(hawaii_suitability$DATE >= Sys.Date())]),
    colnames = FALSE
    ) # END renderTable  
  
  # create reactive dataset based on selected dates
  hi_range<-reactive({    
    hawaii_suitability[which(hawaii_suitability$DATE >= input$hawaii_range[1] &
                               hawaii_suitability$DATE <= input$hawaii_range[2]),]
  }) # END reactive
  
  # check date range is valid
  output$date_check3<-renderText({
    validate(need(input$hawaii_range[2] > input$hawaii_range[1], 
                  "The selected end date is earlier than the start date. Please select a correct date range to display data."))
  }) # END renderText
  
  # plot warning levels 
  output$warning_hawaii<-renderPlot({
    
    #plot suitability over selected date range  
    hawaii_warn = ggplot(hi_range(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
      geom_point(size = 2.5) + scale_color_manual(name = "Avian Malaria Status", values = hawaii_colors) +
      scale_shape_manual(name = "Data Source", values = hawaii_shapes,
                         labels = c("14-Day Weather Forecast", "Adjusted Airport Data", "Annual Climatic Average")) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
      ggtitle("Hakalau") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    
    # add forecast box
    hawaii_warn_box = ggplot(hi_range(), aes(x = DATE, y = lm_TADJ, color = am_state, shape = STATION)) + 
      geom_rect(aes(xmin = Sys.Date() - 0.5, xmax = Sys.Date() + 92, 
                    ymin = min(hi_range()$lm_TADJ, na.rm = T) - 0.5, 
                    ymax = max(hi_range()$lm_TADJ, na.rm = T) + 0.5),
                linetype = 5, color = "olivedrab", size = 1, fill = "transparent") +
      annotate("text", x = Sys.Date() + 50, y = max(hi_range()$lm_TADJ, na.rm = T) + 1,
               label = "FORECAST", color = "olivedrab", size = 4) +
      geom_point(size = 2.5) + scale_color_manual(name = "Avian Malaria Status", values = hawaii_colors) +
      scale_shape_manual(name = "Data Source", values = hawaii_shapes,
                         labels = c("14-Day Weather Forecast", "Adjusted Airport Data", "Annual Climatic Average")) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
      xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) + 
      ggtitle('Hakalau') + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
    
    # plot box if within chosen date range
    if(input$hawaii_range[1] >= Sys.Date() | input$hawaii_range[2] >= Sys.Date()){
      hawaii_warn_box
    }else{
      hawaii_warn  
    }
    
  }) # END renderPlot
  
  # table Hawaii data for the selected date range
  output$hawaii_table<-renderTable(rownames = TRUE,
    cbind(ANNUAL = round(table(hi_range()$am_state)/sum(table(hi_range()$am_state))*100, 2),
          WINTER = round(table(hi_range()$am_state[which(as.numeric(format(hi_range()$DATE, format = "%m")) <= 4 |
                                                                   as.numeric(format(hi_range()$DATE, format = "%m")) >= 11)])/
                           sum(table(hi_range()$am_state[which(as.numeric(format(hi_range()$DATE, format = "%m")) <= 4 |
                                                                         as.numeric(format(hi_range()$DATE, format = "%m")) >= 11)]))*100, 2),
          SUMMER = round(table(hi_range()$am_state[which(as.numeric(format(hi_range()$DATE, format = "%m")) >= 5 &
                                                                   as.numeric(format(hi_range()$DATE, format = "%m")) <= 10)])/
                           sum(table(hi_range()$am_state[which(as.numeric(format(hi_range()$DATE, format = "%m")) >= 5 &
                                                                         as.numeric(format(hi_range()$DATE, format = "%m")) <= 10)]))*100, 2))
  ) # END renderTable  
    
} # END shinyServer

# run shiny web app locally
# shinyApp(ui = shinyUI, server = shinyServer)
# run to properly display static images
# shinyAppDir(".")

#---------------------------#
##### END SERVER SCRIPT #####
#---------------------------#