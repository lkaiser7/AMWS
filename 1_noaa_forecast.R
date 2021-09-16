# Adding new Dark Sky Data to Stations #
# Building on Temperature Relationships #
# Current data download since 06-02-2021 #

#----------------#
##### SET UP #####
#----------------#

# set working directory
# amDir<-"D:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega personal pc
# amDir<-"E:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega hard drive
# setwd(amDir)

# load necessary packages
library(dplyr)
library(rnoaa)
library(darksky)

# set darksky api key
darksky_api<-readLines("darksky_api_key.txt", warn = F) 
Sys.setenv(DARKSKY_API_KEY = darksky_api)

# load station information
stn_info<-read.csv("stns_list_info.csv", header = T)

#---------------------#
##### FORMAT DATA #####
#---------------------#

for(s in 1:dim(stn_info)[1]){  # set s = 1 for debugging
  # load station data
  stn_data<-read.csv(paste0(stn_info$LOCATION[s], ".csv"), header = T)
  # remove first 'x' column
  stn_data<-stn_data[,-1]
  # format date column
  stn_data$DATE<-as.Date(stn_data$DATE, format = "%Y-%m-%d")
  # stn_data$DATE<-as.Date(stn_data$DATE, format = "%m/%d/%Y")
  # stn_data<-stn_data[complete.cases(stn_data$TMEAN),]
  
  # load station relation
  stn_lm<-read.csv(paste0(stn_info$LOCATION[s], "_lm.csv"), header = T)
  # load darksky station relation
  # ds_lm<-read.csv(paste0(stn_info$STATION[s], "_ds_lm.csv"), header = T)
  
  # update past airport station data to current station (if needed)
  # stn_data$lm_TADJ<-(stn_lm$lm_reg[2]*stn_data$TMEAN) + (stn_lm$lm_reg[1]
  # use if adding/updating station location that references same airport station
  
  # get last data download date
  last_date<-last(stn_data$DATE)
  
  # ----- CURRENT DATA ----- #
  # readme: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt 
  # path to noaa ghcn daily data: ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily 
  
  # make sure dataset is current
  if(Sys.Date() > last_date){
    # create dataset of dates for missing data
    new_data<-data.frame(DATE = seq(last_date + 1, Sys.Date(), by = "days"), STATION = stn_info$AIRPORT[s])
    
    # # check ghcnd station record by airport code if needed
    # # https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt 
    
    # download new data from NOAA NCDC daily summaries ("TAVG" may not be up-to-date)
    # noaa_data<-data.frame(ghcnd_search(stn_info$AIRPORT_ID[s], var = c("TAVG", "TMIN", "TMAX"), date_min = last_date + 1))
    noaa_data<-data.frame(ghcnd_search(stn_info$APT_CODE[s], var = c("TMIN", "TMAX"), 
                                       date_min = last_date + 1, refresh = T))
    
    # order new data by date
    noaa_data<-noaa_data[order(noaa_data$tmin.date),]
    # limit data to current temperature data
    noaa_current<-noaa_data[which(noaa_data$tmin.date <= Sys.Date()),]
    
    # calculate temperature mean from max and min values 
    calc_tmean<-rowMeans(cbind(noaa_current$tmax.tmax, noaa_current$tmin.tmin), na.rm = T)/10

    # check for data reporting delay 
    if(dim(new_data)[1] == dim(noaa_current)[1]){
      new_data$TMEAN<-calc_tmean
    }else{
      # adjust NOAA data for ~2 day reporting delay
      day_diff<-dim(new_data)[1] - dim(noaa_current)[1]
      new_data$TMEAN<-c(calc_tmean, rep(NaN, day_diff))
    }
    
    # adjust NOAA airport station data
    new_data$lm_TADJ<-(stn_lm$lm_reg[2]*new_data$TMEAN) + (stn_lm$lm_reg[1])
    
    # add to station data
    stn_data<-rbind(stn_data, new_data)
    # save updated dataset
    write.csv(stn_data, paste0(stn_info$LOCATION[s], ".csv"))
  }
  
  # check for na values
  noaa_na<-stn_data[which(is.na(stn_data$TMEAN)),]
  
  # gap fill missing values if any
  if(dim(noaa_na)[1] > 0){
    # replace station name with 'DarkSky' to indicate data source
    levels(stn_data$STATION)<-c(levels(stn_data$STATION), "DarkSky")
    # loop through all missing values and gap fill data
    for(g in 1:dim(noaa_na)[1]){  # set g = 1 for debugging 
      # get darksky data for missing values
      fill_data<-get_forecast_for(stn_info$APT_LAT[s], stn_info$APT_LON[s], 
                                  paste0(noaa_na$DATE[g], "T12:00:00"), 
                                  units = "si", add_headers = T)
      # replace na values with mean hourly data
      # noaa_na$TMEAN[g]<-mean(fill_data$hourly$temperature)
      noaa_na$TMEAN[g]<-mean(c(fill_data$daily$temperatureMax, fill_data$daily$temperatureMin))
      
      # adjust darksky data
      # noaa_na$lm_TADJ[g]<-(ds_lm$avg_lm[2]*noaa_na$TMEAN[g]) + (ds_lm$avg_lm[1])
      noaa_na$lm_TADJ[g]<-(stn_lm$lm_reg[2]*noaa_na$TMEAN[g]) + (stn_lm$lm_reg[1])
      
      # gap fill values back into main dataset
      stn_data$STATION[which(stn_data$DATE == noaa_na$DATE[g])]<-"DarkSky"
      stn_data$TMEAN[which(stn_data$DATE == noaa_na$DATE[g])]<-noaa_na$TMEAN[g]
      stn_data$lm_TADJ[which(stn_data$DATE == noaa_na$DATE[g])]<-noaa_na$lm_TADJ[g]
      
      # set factor levels
      stn_data$STATION<-as.factor(stn_data$STATION)
    }
    # save updated dataset
    write.csv(stn_data, paste0(stn_info$LOCATION[s], ".csv"))
  }
  
  # ----- FORECAST DATA ----- #
  
  # get Dark Sky station forecast out to 7-days
  stn_forecast<-get_current_forecast(stn_info$APT_LAT[s], stn_info$APT_LON[s],
                                     units = "si", exclude = "minutely,hourly,alerts,flags")

  # format Dark Sky forecast
  stn_ds<-data.frame(DATE = stn_forecast$daily$time, STATION = "DarkSky",
                     TMEAN = rowMeans(cbind(stn_forecast$daily$temperatureMax, 
                                            stn_forecast$daily$temperatureMin)))
  # remove current date of Dark Sky forecast
  stn_ds<-stn_ds[-1,]
  
  # add additional 14-day forecast
  for(fcast in 1:7){  # set fcast = 1 for debugging
    # set new forecast date
    time_stamp<-paste0(as.Date(last(stn_ds$DATE)) + 1, "T12:00:00")
    # get additional Dark Sky forecast data
    fcast_data<-get_forecast_for(stn_info$APT_LAT[s], stn_info$APT_LON[s],
                                 time_stamp, units = "si", add_headers = T, exclude = "hourly")
    # format additional Dark Sky forecast data
    add_fcast<-data.frame(DATE = fcast_data$daily$time, STATION = "DarkSky",
                          TMEAN = rowMeans(cbind(fcast_data$daily$temperatureMax, 
                                                 fcast_data$daily$temperatureMin)))
    # add additional Dark Sky forecast data
    stn_ds<-rbind(stn_ds, add_fcast)
  }
  
  # relate Dark Sky data
  stn_ds$lm_TADJ<-(stn_lm$lm_reg[2]*stn_ds$TMEAN) + (stn_lm$lm_reg[1])
  # format date column
  # stn_ds$DATE<-as.Date(stn_ds$DATE, format = "%Y-%m-%d")
  
  # add forecast data to complete dataset
  final_stn_data<-rbind(stn_data, stn_ds)
  
  #----- EXTENDED HISTORICAL FORECAST -----#
  ### final_stn_data<-read.csv(paste0(stn_info$LOCATION[s], "_forecast.csv"), header = T)
  ### final_stn_data$DATE<-as.Date(final_stn_data$DATE, format = "%Y-%m-%d")
  ### final_stn_data<-final_stn_data[which(final_stn_data$DATE < as.Date("2018-01-02")),]
  ### final_stn_data<-final_stn_data[,-1]
  
  # read historical daily temperature average
  hist_avg<-read.csv(paste0(stn_info$LOCATION[s], "_annual_temp.csv"))
  # check for any missing NA values
  hist_avg<-hist_avg[complete.cases(hist_avg$AVG_T),]
  
  # find current day of 365 year
  ann_day<-as.numeric(tail(final_stn_data$DATE, n = 1)-as.Date(paste0(format(Sys.Date(), format = "%Y"), "-01-01")))
  # adjust for new year dates if needed
  if(ann_day > 365){
    ann_day<-as.numeric(as.Date(paste0(format(Sys.Date(), format = "%Y"), "-12-31"))-as.Date(paste0(format(Sys.Date(), format = "%Y"), "-01-01")))
  }
  
  # adjust day count if less than 90 day period 
  if(ann_day < 90){
    # calculate number of days missing in 90 day period
    miss_days = 90-ann_day
    # add annual average to 90 day period and crop to current day (historical annual average)
    h_avg<-rbind(tail(hist_avg, n = miss_days + 1), hist_avg[which(hist_avg$DAY < ann_day),])
    # crop to future days (future annual average)
    f_avg<-hist_avg[which(hist_avg$DAY > ann_day - 1 & hist_avg$DAY < ann_day + 90),]
    
    # average 3-month period (90 days) of current data
    # final_stn_data$DATE[which(final_stn_data$DATE > tail(final_stn_data$DATE, n = 1)-90)]
    Ac_t<-mean(final_stn_data$lm_TADJ[which(final_stn_data$DATE > tail(final_stn_data$DATE, n = 1)-90)], na.rm = T)
    # average 3-month period (90 days) of historical average data
    Ah_t<-mean(h_avg$AVG_T)
    # calculate averge difference between current and historical average data
    A_delta<-Ac_t - Ah_t

    # build 90-day historical forecast data frame
    Af_data<-data.frame(# X = seq(from = tail(final_stn_data$X, n = 1) + 1, to = tail(final_stn_data$X, n = 1) + 90, by = 1),
                        DATE = seq(from = tail(final_stn_data$DATE, n = 1) + 1, to = tail(final_stn_data$DATE, n = 1) + 90, by = 1), 
                        STATION = rep("Historical Annual Average", 90), TMEAN = f_avg$AVG_T[which(f_avg$DAY < ann_day + 90)], 
                        lm_TADJ = f_avg$AVG_T + A_delta)
    
  }else if(ann_day > 275){
    # calculate number of days missing in 90 day period
    miss_days = 90-(365-ann_day)
    # add annual average to 90 day period and crop to current day (historical annual average)
    h_avg<-tail(hist_avg[which(hist_avg$DAY < ann_day),], n = 90)
    # crop to future days (future annual average)
    f_avg<-rbind(hist_avg[which(hist_avg$DAY > ann_day),], head(hist_avg, n = miss_days))
    
    # average 3-month period (90 days) of current data
    Ac_t<-mean(final_stn_data$lm_TADJ[which(final_stn_data$DATE > tail(final_stn_data$DATE, n = 1)-90)], na.rm = T)
    # average 3-month period (90 days) of historical average data
    Ah_t<-mean(h_avg$AVG_T)
    # calculate averge difference between current and historical average data
    A_delta<-Ac_t - Ah_t
    
    # build historical forecast data frame
    Af_data<-data.frame(# X = seq(from = tail(final_stn_data$X, n = 1) + 1, to = tail(final_stn_data$X, n = 1) + 90, by = 1),
                        DATE = seq(from = tail(final_stn_data$DATE, n = 1) + 1, to = tail(final_stn_data$DATE, n = 1) + 90, by = 1), 
                        STATION = "Historical Annual Average", TMEAN = f_avg$AVG_T[which(f_avg$DAY < ann_day + 90)], 
                        lm_TADJ = f_avg$AVG_T + A_delta)
    
  }else{
    # crop average to current day (historical annual average)
    h_avg<-hist_avg[which(hist_avg$DAY < ann_day + 1),]
    # crop average to future days (future annual average)
    f_avg<-hist_avg[which(hist_avg$DAY > ann_day),]
    
    # average 3-month period (90 days) of current data
    Ac_t<-mean(final_stn_data$lm_TADJ[which(final_stn_data$DATE > tail(final_stn_data$DATE, n = 1)-90)], na.rm = T)
    # average 3-month period (90 days) of future data
    Af_t<-mean(h_avg$AVG_T[which(h_avg$DAY > ann_day-90)])
    # calculate averge difference between current and future data
    A_delta<-Ac_t - Af_t
    
    # build historical forecast data frame
    Af_data<-data.frame(# X = seq(from = tail(final_stn_data$X, n = 1) + 1, to = tail(final_stn_data$X, n = 1) + 90, by = 1),
                        DATE = seq(from = tail(final_stn_data$DATE, n = 1) + 1, to = tail(final_stn_data$DATE, n = 1) + 90, by = 1), 
                        STATION = "Historical Annual Average", TMEAN = f_avg$AVG_T[which(f_avg$DAY < ann_day + 91)], 
                        lm_TADJ = f_avg$AVG_T[which(f_avg$DAY < ann_day + 91)] + A_delta)
  }

  # add to forecast data
  final_stn_data<-rbind(final_stn_data, Af_data)

  # # check plot current data + forecast
  # ggplot(tail(final_stn_data2, n = 180), aes(x = DATE, y = lm_TADJ, color = STATION, shape = STATION)) +
  #   geom_line() + geom_point(size = 1.5) +
  #   xlab("Date") + ylab(expression("Mean daily temperature " ~degree~C)) +
  #   ggtitle(stn_info$LOCATION[s]) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  # save final dataset
  write.csv(final_stn_data, paste0(stn_info$LOCATION[s], "_forecast.csv"))

}

#-----------------------#
##### END OF SCRIPT #####
#-----------------------#