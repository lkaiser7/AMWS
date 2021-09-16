# Avian Malaria Warning System app via shinyapp.io
# instructions to update the data & the portal app
# will take about ~5 mintues to redeploy app alone
# these scripts will take ~30-45 minutes to update

###########################
##### GETTING STARTED #####
###########################

# shinyapps.io account information
# login: lkaiser7@hawaii.edu (via GitHub)
# account: lauren-r-kaiser

# install R packages for shiny apps
# install.packages('rsconnect')
library('rsconnect')
library(shiny)

# authorize shinyapps.io account 
rsconnect::setAccountInfo(name='lauren-r-kaiser',
                          token='723A263B147A4EADE6BCC82BF09AD20D',
                          secret='tT0HMYz3Kadaw6lEy8va8MrDTBt4lApmdPrJWcdq')

# set root directory 
# rootDir<-"K:/Users/lkaiser/Dropbox/FB_warning_system/avian_malaria_warning/"
# rootDir<-"E:/Dropbox/FB_warning_system/new_am_system/"  # Iomega hard drive
# setwd(rootDir)

############################
##### UPDATED APP DATA #####
############################

# runs scripts in order to update data - done in server script
# 1. update DarkSky forecast data 
# source("1_dakrsky_forecast.R")
# 2. calculate avian malaria warning levels
# source("2_am_warning.R")

######################
##### UPDATE APP #####
######################

# deploy shiny app
#rsconnect::deployApp('C:/Users/lkaiser/Dropbox/FB_warning_system/avian_malaria_warning', 
#                     account = 'lauren-r-kaiser')

# deploy shiny app
#deployApp(appDir = 'C:/Users/lkaiser/Dropbox/FB_warning_system/avian_malaria_warning',
#          account = 'lauren-r-kaiser')

# deploy shiny app
# deployApp(appDir = "K:/Dropbox/FB_warning_system/avian_malaria_warning", account = 'lauren-r-kaiser')
# deployApp(appDir = "D:/Dropbox/FB_warning_system/avian_malaria_warning", account = 'lauren-r-kaiser')
deployApp(appDir = "E:/Dropbox/FB_warning_system/avian_malaria_warning", account = 'lauren-r-kaiser')

##### END AM WARNING SYSTEM PORTAL UPDATE #####
# app last successfully updated and deployed 06/02/2021
# https://lauren-r-kaiser.shinyapps.io/avian_malaria_warning/
# hosted online on server at: avianmalaria.watch 
