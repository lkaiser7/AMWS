# AMWS
Avian Malaria Warning System R Shiny Portal:
https://avianmalaria.watch/

Code to run R Shiny App Portal Site

### DATA PROCESSING ###

"0_am_portal_setup.R"
runs forcast and warning scripts
launches new shiny portal update

"1_noaa_forecast.R"
downloads most up-to-date NOAA station data via FTP
gap fills and downloads forecast from DarkSky API

"2_am_warning.R"
projects avian malaria suitability at each site
base on Ahumada, LaPointe, & Samuel, 2004 thresholds 

### SHINY APP ###

"ui.R" - web page user interface file for shiny app portal
"server.R" - running live R session that displays portal
