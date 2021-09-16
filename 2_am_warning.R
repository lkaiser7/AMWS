# Avian Malaria Warning System #
# Mosquito & Disease Threshold #
# Graphing days of suitability #

#----------------#
##### SET UP #####
#----------------#

# set working directory
# wd = "D:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega personal pc
# wd = "E:/Dropbox/FB_warning_system/avian_malaria_warning/"  # Iomega hard drive
# setwd(wd)

# list stations 
stn_list<-c('Alakai', # Kauai
            'Hanawi', # Maui
            'Hakalau Lower') # Hawaii

stn_ids<-c('USC00513099', # Kauai
           'HN163', # Maui
           'HE284') # Hawaii

# SEE: Ahumada, LaPointe, & Samuel, 2004
# set threshold values for mosquitos
MTD_mosquito = 9.9 # minimum temperature for development (MTD)
DD_mosquito = 137  # minimum number of degree days (DD)

# set threshold values for disease
MTD_disease = 12.97 # minimum temperature for development (MTD)
DD_disease = 86.2   # minimum number of degree days (DD)

#--------------------------------#
##### TEMPERATURE THRESHOLDS #####
#--------------------------------#

# loop through all stations and evaluate thresholds
for(stn in 1:length(stn_list)){  # set stn = 1 for debugging
  # load station data
  stn_data<-read.csv(paste0(stn_list[stn], "_forecast.csv"), header = T)
  # format date column
  stn_data$DATE = as.Date(stn_data$DATE)
  
  # create binary of temperatures above minimum thresholds
  stn_data$mosquito_Threshold = as.numeric(stn_data$lm_TADJ > MTD_mosquito)
  stn_data$disease_Threshold = as.numeric(stn_data$lm_TADJ > MTD_disease)
  
  # calculate difference in temperature relative to threshold 
  stn_data$mosquito_T = stn_data$lm_TADJ - MTD_mosquito
  # set negative and NA values = 0 for days below the threshold
  stn_data$mosquito_T[stn_data$mosquito_T < 0] = 0
  stn_data$mosquito_T[is.na(stn_data$mosquito_T)]<-0
  
  # calculate difference in temperature relative to threshold 
  stn_data$disease_T = stn_data$lm_TADJ - MTD_disease
  # set negative and NA values = 0 for days below the threshold
  stn_data$disease_T[stn_data$disease_T < 0] = 0
  stn_data$disease_T[is.na(stn_data$disease_T)]<-0
  
  # create columns for mosquito and disease
  stn_data$mosquito_DD = 0
  stn_data$disease_DD = 0
  
  # set mosquito and disease equivalencies for first day of record
  stn_data$mosquito_DD[1] = stn_data$mosquito_T[1]
  stn_data$disease_DD[1] = stn_data$disease_T[1]

  ### 30-DAY INCUBATION METHOD ###
  # create degree-day counter
  M_dd_count = 0
  D_dd_count = 0
  
  # loop through to find days when degree day threshold is crossed
  for (i in c(2:length(stn_data$lm_TADJ))){  # set i = 2 for debugging
    # for mosquito threshold temperatures greater than 0
    if (stn_data$mosquito_T[i] > 0){
      # determine number of degree days above 0
      stn_data$mosquito_DD[i] = stn_data$mosquito_DD[i - 1] + stn_data$mosquito_T[i]
    }else{
      # if not consecutive, leave degree days constant
      stn_data$mosquito_DD[i] = stn_data$mosquito_DD[i - 1]
    }
    
    # check mosquito degree-day counter
    if(stn_data$mosquito_DD[i] == stn_data$mosquito_DD[i - 1]){
      M_dd_count = M_dd_count + 1
    }else{
      M_dd_count = 0
    }
    # reset degree days to 0 if counter reaches 30
    if(M_dd_count > 29){
      # reset degree days back to 0 after incubation period expires 
      stn_data$mosquito_DD[i] = 0
      M_dd_count = 0
    }
    
    # for disease threshold temperature greater than 0
    if (stn_data$disease_T[i] > 0){
      # determine number of degree days above 0
      stn_data$disease_DD[i] = stn_data$disease_DD[i - 1] + stn_data$disease_T[i]
    }else{
      # if not consecutive, leave degree days constant
      stn_data$disease_DD[i] = stn_data$disease_DD[i - 1]
    }
    
    # check disease degree-day counter
    if(stn_data$disease_DD[i] == stn_data$disease_DD[i - 1]){
      D_dd_count = D_dd_count + 1
    }else{
      D_dd_count = 0
    }
    # reset degree days to 0 if counter reaches 30
    if(D_dd_count > 29){
      # reset degree days back to 0 after incubation period expires 
      stn_data$disease_DD[i] = 0
      D_dd_count = 0
    }
    
  } # END DD LOOP
  
  ### CONSERVATIVE ZERO-OUT METHOD ###
  # # loop through to find days when degree day threshold is crossed
  # for (i in c(2:length(stn_data$lm_TADJ))){  # set i = 2 for debugging 
  #   # for mosquito threshold temperatures greater than 0
  #   if (stn_data$mosquito_T[i] > 0){
  #     # determine number of degree days above 0
  #     stn_data$mosquito_DD[i] = stn_data$mosquito_DD[i - 1] + stn_data$mosquito_T[i]
  #   }else{
  #     # if not consecutive, leave degree days as 0
  #     stn_data$mosquito_DD[i] = 0
  #   }
  #   
  #   # for disease threshold temperature greater than 0
  #   if (stn_data$disease_T[i] > 0){
  #     # determine number of degree days above 0
  #     stn_data$disease_DD[i] = stn_data$disease_DD[i - 1] + stn_data$disease_T[i]
  #   }else{
  #     # if not consecutive, leave degree days as 0
  #     stn_data$disease_DD[i] = 0
  #   }
  # } # END DD LOOP
  
  # determine days with suitable for mosquitos and disease (T/F)
  stn_data$mosquito_suitable = stn_data$mosquito_DD > DD_mosquito
  stn_data$disease_suitable = stn_data$disease_DD > DD_disease
  
  # classify days based on suitability
  stn_data$am_state = "Not suitable for vector or disease development"
  stn_data$am_state[stn_data$mosquito_suitable & !stn_data$disease_suitable] = 
    "Suitable for Culex vector development only"
  stn_data$am_state[!stn_data$mosquito_suitable & stn_data$disease_suitable] = 
    "Suitable for Malaria disease development only"
  stn_data$am_state[stn_data$mosquito_suitable & stn_data$disease_suitable] = 
    "Suitable for both disease and vector development"
  
  # save updated suitability data
  write.csv(stn_data, paste0(stn_list[stn], "_suitability.csv"))

} # END stn for loop 

# # load necessary packages
# library(ggplot2)
# # visuzalization of data
# ggplot(stn_data[12979:16629,]) +
#   geom_line(aes(x = DATE, y = lm_TADJ)) +
#   geom_point(aes(x = DATE, y = lm_TADJ, col = am_state)) +
#   scale_color_manual(values = c('red', 'blue', 'black')) +
#   labs(x = "", y = "Temp (C)") + theme(legend.position = "bottom")
# table(stn_data$am_state)

#-----------------------#
##### END OF SCRIPT #####
#-----------------------#