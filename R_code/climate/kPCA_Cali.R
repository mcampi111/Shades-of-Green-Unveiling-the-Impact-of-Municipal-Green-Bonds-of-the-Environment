library(rnoaa)
library(dplyr)
library(sf)
library(leaflet)
library(lubridate)
library(ggplot2)
library(rworldmap)
library(ggrepel)
library(usmap)
library(maps)
library(ggmap)
library(mapdata)
library(geosphere)
library(purrr)
library(tidyverse)
library(stringr)
library(naniar)
library(GSODR)
library(zoo)
library(reticulate)
library(proxy)
library(pracma)
library(rlist)
library(rlang)
library(reshape2)
library(kableExtra)
library(TTR)
library(xts)
library(forcats)
library(igraph)
library(philentropy)
library(gridExtra)
library(grid)
library(pastecs)
library(DiagrammeR)
library(cowplot)
library(matrixStats)
library(sp)
library(maptools)
library(useful)
library(maptools)


###################
# KPCA CALIFORNIA #
###################


###################################
# isd_history to get the stations #
###################################
load(system.file("extdata", "isd_history.rda", package = "GSODR"))
unique(isd_history$COUNTRY_NAME)

###################################
# select the stations from the us #
###################################

US_station <- subset(isd_history, COUNTRY_NAME == "UNITED STATES")
US_station<- US_station %>% filter((LON < -60) & (LON > -130) &  (LAT < 50) & (LAT > 20) )

states_gg <- map_data("state")

#Map of the available stations
ggplot(data = states_gg) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = US_station,
             aes(x = LON,
                 y = LAT),
             alpha = 0.6,
             size = 2,
             color = 'green') +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Available stations in the US") +
  theme(legend.position = "right") 



load("C:/.../meteo_var_GSOD_us_v2.RData")


df_lat_long_plot = data.frame(id = meteo_variables_GSOD$STNID,
                              lat = meteo_variables_GSOD$LATITUDE,
                              lon = meteo_variables_GSOD$LONGITUDE)

un_id<- unique(df_lat_long_plot)


#####################################################
# Available Dataset with cities population > 250,000 #
#####################################################

major_city_us <- us.cities
#remove outliers
major_city_us <- filter(major_city_us, long > -130)

###########################################################################
# We only select those cities which have a population bigger than pop_max #
########################################################################### 
pop_max = 250000
major_city_us_pop_max = major_city_us %>% filter( pop > pop_max)


ggplot(data = states_gg) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = un_id,
             aes(x = lon,
                 y = lat),
             alpha = 0.6,
             size = 5,
             color = 'green') +
  geom_point(data = major_city_us_pop_max,
             aes(x = long, y = lat, size = pop),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma)  +
  labs(title = "Selected Weather Stations in the US") +
  theme(legend.position = "right") 


###################
#   CALIFORNIA    #
###################

county_gg<- map_data("county")
california <- subset(county_gg, region %in% c("california"))

un_id_cali <-  un_id  %>% filter((lon < -116) & (lon > -125) &  (lat < 42) & (lat > 32) )
major_city_us_pop_max_cali<- major_city_us_pop_max %>% filter(country.etc == "CA")


gg1 = ggplot(data = california) +
     geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
     coord_fixed(1.3) +
     geom_point(data = un_id_cali,
             aes(x = lon,
                 y = lat),
             alpha = 0.6,
             size = 5,
             color = 'green') +
    geom_point(data = major_city_us_pop_max_cali,
             aes(x = long, y = lat, size = pop),
             color = "purple", alpha = 0.5) +
    scale_size_continuous(range = c(5, 10),
                        label = scales::comma)  + theme_bw() +
    labs(title = "Selected Weather Stations in California") +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10))  +
  ylab("Latitude") + xlab("Longitude") 

gg1

ggsave("C:\\...\\cali_weath_stations.pdf",
       gg1, width = 5 , height = 5)

dev.off()

############################################
# SELECT ONLY THE VARIABLE OF INTEREST     #
############################################

my_meteo_var_GSODR<- meteo_variables_GSOD[,c(1:15,19,21,29,31,33,42:44)]

#########################################
#       plots of the time series        #     ADD IN HERE!!!!
#########################################


##############################################################
#  I Group them by states --> I contruct an index per state  #
##############################################################

list_my_met_var_bystate<- my_meteo_var_GSODR %>% group_split(STATE)



####################################################################################
#  Select California --> Group it by Counties --> I construct one index per county #
####################################################################################

list_my_met_var_cali<- list_my_met_var_bystate[[4]]

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


stations_cali_coord<- data.frame(list_my_met_var_cali[,6:5])

stats_cali_counties = latlong2county(stations_cali_coord)

list_my_met_var_cali$COUNTY = sapply(strsplit(stats_cali_counties, ","), "[", 2)

list_my_met_var_cali_county<- list_my_met_var_cali %>% group_split(COUNTY)

###########################################################################
# TODO:   NEED TO DO MORE INVESTIGATION HERE - NOT SURE WHAT IS HAPPENING #
###########################################################################

gne = cbind(unique(list_my_met_var_cali_county[[22]]$LONGITUDE), unique(list_my_met_var_cali_county[[22]]$LATITUDE))
latlong2county(gne)
unique(list_my_met_var_cali_county[[22]]$NAME)


list_my_met_var_cali_county = list_my_met_var_cali_county[-22]
length(list_my_met_var_cali_county)

###########################   
#      Data Cleaning      #   
###########################   

#BEFORE COMPUTING HISTORICAL VOLATILITY AND OTHER SUMMARY STATISTICS, WE NEED TO FURTHER
#CLEAN THE DATA AND REMOVE STATIONS WHICH DO NOT HAVE THE DUE AMOUNT OF DAYS PER YEAR

new_list_my_met_var_cali_county_stnid<- lapply(1:length(list_my_met_var_cali_county), function(i) 
  list_my_met_var_cali_county[[i]] %>% group_split(STNID) )


new_list_my_met_var_cali_county_stnid_year<- lapply(1:length(new_list_my_met_var_cali_county_stnid), function(i)
  lapply(1:length(new_list_my_met_var_cali_county_stnid[[i]]), function(j)
    new_list_my_met_var_cali_county_stnid[[i]][[j]] %>% group_split(YEAR) ))


n_year_by_stnid = lapply(1:length(new_list_my_met_var_cali_county_stnid_year), function(h)
  lapply(1:length(new_list_my_met_var_cali_county_stnid_year[[h]]), function(j)
    sapply(1:length(new_list_my_met_var_cali_county_stnid_year[[h]][[j]]), 
           function(i) dim(new_list_my_met_var_cali_county_stnid_year[[h]][[j]][[i]])[1])))


l_name_station<- lapply(1:length(new_list_my_met_var_cali_county_stnid), function(j)
  sapply(1:length(new_list_my_met_var_cali_county_stnid[[j]]), function(i) 
    unique(new_list_my_met_var_cali_county_stnid[[j]][[i]]$NAME))  )



for(i in 1:length(n_year_by_stnid)){
  names(n_year_by_stnid[[i]]) <- l_name_station[[i]]
}


list_stat_by_year_stnid_final<- lapply(1:length(n_year_by_stnid), function(i) 
  t(sapply(n_year_by_stnid[[i]], "length<-", max(lengths(n_year_by_stnid[[i]])))))


list_stat_by_year_stnid_final
#we remove the ones that have less than 30 days
#is some are NA --> no problem, we average any way across what we have

to_rmv_less_30<- sapply(1:length(list_stat_by_year_stnid_final), function(i) 
  which(list_stat_by_year_stnid_final[[i]] < 30, arr.ind = T))

names_to_rmv_less_30<- lapply(1:length(to_rmv_less_30), function(i) rownames(to_rmv_less_30[[i]]))


names_selected_stations<- lapply(1:length(l_name_station), function(i)
  l_name_station[[i]][!(l_name_station[[i]]) %in% names_to_rmv_less_30[[i]]])



#Now we filter the original dataset by states (list_my_met_var_cali_county) according to
#names_selected_stations

new_list_my_met_var_bycounty<- lapply(1:length(list_my_met_var_cali_county), function(i)
  list_my_met_var_cali_county[[i]] %>% filter(NAME %in% names_selected_stations[[i]])  )



final_dataset_all_stations<- do.call(rbind, new_list_my_met_var_bycounty)


###########################    #######################################################
#final number of stations #  - # I do this later again for another step of filtering #
###########################    #######################################################

n_stat2<- sapply(1:length(names_selected_stations), function(i) length(names_selected_stations[[i]]) )

min_nstat<- min(n_stat2)

df_station_bar2<- data.frame(NSTAT = n_stat2,  NAMES = unique(final_dataset_all_stations$COUNTY) )

ggplot(data=df_station_bar2, aes(x=NAMES, y=NSTAT, fill=NAMES)) +
  geom_bar(stat="identity")+
  theme_minimal() + theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Number of Stations")+
  xlab("California Counties")+
  geom_text(aes(label=NSTAT), vjust=1.6, color="blue", size=2.5) 

###########################   
#    Summary Statistics   #   
###########################   

#SPLINE were MAX and MIN are missing by STNID, MONTH
new_list_my_met_var_bycounty_spline = lapply(1:length(new_list_my_met_var_bycounty), function(i) 
  new_list_my_met_var_bycounty[[i]] %>% group_by(STNID, MONTH) %>% mutate(new_MAX = na.spline(MAX),
                                                                         new_MIN = na.spline(MIN)))

#COMPUTE HISTORICAL VOLATILITY/PK VOLATILITY by grouping them by year, stnid

new_list_my_met_var_bycounty_vol = lapply(1:length(new_list_my_met_var_bycounty_spline), function(i)
  new_list_my_met_var_bycounty_spline[[i]] %>% group_by(STNID) %>%
    mutate(High = ifelse(new_MAX == 0, new_MAX + runif(1, 0, 0.05), new_MAX),
           Low = ifelse(new_MIN == 0, new_MIN + runif(1, 0, 0.05), new_MIN )) %>% 
    mutate(Zero_1 =  rep(0, length(High)),
           Zero_2 =  rep(0, length(High))) %>% 
    mutate(Vol = volatility(cbind(Zero_1, abs(High),abs(Low), Zero_2),
                            n = 30,
                            N =1,
                            calc = "parkinson"))  )

#we will have 30 days which are NA for vol corresponding to the sliding window
#NOW:
#shift the volatility of 30 days so to have them aligne

new_list_my_met_var_bycounty_vol_shift<- lapply(1:length(new_list_my_met_var_bycounty_vol), function(i)
  shift.column(new_list_my_met_var_bycounty_vol[[i]], "Vol", len = 30, up = TRUE, newNames = "Vol_shift")   )



#remove all the day without Vol, i.e. NA per Vol --> we kill VOl_shift and don't need Vol anymore
new_list_my_met_var_bycounty_vol_na =  lapply(1:length(new_list_my_met_var_bycounty_vol_shift), function(i)
  new_list_my_met_var_bycounty_vol_shift[[i]] %>% drop_na(Vol_shift)  )

#We compute again our final dataframe and then take:
#Compute maximum unique number of days - do this here

final_dataset_all_stations_vol<- do.call(rbind, new_list_my_met_var_bycounty_vol_na)


un_year<- unique(final_dataset_all_stations_vol$YEARMODA)

ts <- seq(ymd(un_year[1]), ymd(un_year[length(un_year)]), by="day")

new_list_my_met_var_bycounty_vol_complete<- lapply(1:length(new_list_my_met_var_bycounty_vol_na), function(i)
  new_list_my_met_var_bycounty_vol_na[[i]] %>%  complete(YEARMODA = seq(ymd(ts[1]), 
                                                                       ymd(ts[length(ts)]),
                                                                       by="day")) )



#Now we compute average temp per day, tot prep per day and average of hist volatility per day

df_avg_temp_prep_vol = lapply(1:length(new_list_my_met_var_bycounty_vol_complete), function(i)
  new_list_my_met_var_bycounty_vol_complete[[i]] %>% group_by(YEARMODA) %>%
    summarise( temp_avg = mean(TEMP, na.rm=TRUE),
               tot_prcp = sum(PRCP,na.rm=TRUE),
               avg_vol  = mean(Vol_shift,na.rm=TRUE)) )


#n days per state
sapply(1:length(df_avg_temp_prep_vol), function(i) dim(df_avg_temp_prep_vol[[i]])[1])


#which NA
sapply(1:length(df_avg_temp_prep_vol), function(i) which(is.na(df_avg_temp_prep_vol[[i]])))


#number of NAs
sapply(1:length(df_avg_temp_prep_vol), function(i) length(which(is.na(df_avg_temp_prep_vol[[i]]))))



df_avg_temp_prep_vol_na<- lapply(1:length(df_avg_temp_prep_vol), function(i) 
                                     na.omit(df_avg_temp_prep_vol[[i]]))


#n days per state
sapply(1:length(df_avg_temp_prep_vol_na), function(i) dim(df_avg_temp_prep_vol_na[[i]])[1])

#which is NA 
sapply(1:length(df_avg_temp_prep_vol_na), function(i) which(is.na(df_avg_temp_prep_vol_na[[i]])))

#number of Nas
sapply(1:length(df_avg_temp_prep_vol_na), function(i) length(which(is.na(df_avg_temp_prep_vol_na[[i]]))))



df_avg_temp_prep_vol_2 = lapply(1:length(df_avg_temp_prep_vol_na), function(i)
                            df_avg_temp_prep_vol_na[[i]] %>%
                        mutate( temp_avg_s = scale(temp_avg),
                                tot_prcp_s = scale(tot_prcp) ,
                                avg_vol_s  =  scale(avg_vol)  )  )



#n days per state
sapply(1:length(df_avg_temp_prep_vol_2), function(i) dim(df_avg_temp_prep_vol_2[[i]])[1])

#which is NA 
sapply(1:length(df_avg_temp_prep_vol_2), function(i) which(is.na(df_avg_temp_prep_vol_2[[i]])))

#number of Nas
sapply(1:length(df_avg_temp_prep_vol_2), function(i) length(which(is.na(df_avg_temp_prep_vol_2[[i]]))))



#Note:
#In the counties where NAs are found, this is due to the scaling of tot_prcp which is 0
#almost everywhere --> I therefore substitute those NAs with 0

df_avg_temp_prep_vol_3 <- lapply(1:length(df_avg_temp_prep_vol_2), function(i)
                                    df_avg_temp_prep_vol_2[[i]] %>%
                        mutate(tot_prcp_s_2 = ifelse(is.na(tot_prcp_s), 0 , tot_prcp_s)  )  )

#n days per state
sapply(1:length(df_avg_temp_prep_vol_3), function(i) dim(df_avg_temp_prep_vol_3[[i]])[1])

#which is NA 
sapply(1:length(df_avg_temp_prep_vol_3), function(i) which(is.na(df_avg_temp_prep_vol_3[[i]])))

#number of Nas
sapply(1:length(df_avg_temp_prep_vol_3), function(i) length(which(is.na(df_avg_temp_prep_vol_3[[i]]))))

#remove tot_prcp_s and keep only tot_prcp_s_2
df_avg_temp_prep_vol_4 <- lapply(1:length(df_avg_temp_prep_vol_3), function(i)
                                         select(df_avg_temp_prep_vol_3[[i]], -tot_prcp_s)  )

#number of Nas
sapply(1:length(df_avg_temp_prep_vol_4), function(i) length(which(is.na(df_avg_temp_prep_vol_4[[i]]))))



df_final_percounty<- lapply(1:length(df_avg_temp_prep_vol_4), function(i) 
                       cbind( COUNTY = rep(unique(new_list_my_met_var_bycounty_vol_na[[i]]$COUNTY),
                                                 dim(df_avg_temp_prep_vol_4[[i]])[1] ),
                              df_avg_temp_prep_vol_4[[i]]) ) 




length(df_final_percounty)

final_dataset<- do.call(rbind, df_final_percounty)

final_dataset <- readRDS("C:\\...\\df_final_percounty.rds")



###########################  
#final number of stations #  
###########################  

l_name_station_final <- l_name_station


n_stat2<- sapply(1:length(l_name_station_final), function(i) length(l_name_station_final[[i]]) )

min_nstat<- min(n_stat2)

df_station_bar2<- data.frame(NSTAT = n_stat2,  NAMES = unique(final_dataset$COUNTY) )


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


df_station_bar2$NAMES2 = sapply(1:length(df_station_bar2$NAMES),function(i) simpleCap(df_station_bar2$NAMES[i]) ) 


gg = ggplot(data=df_station_bar2, aes(x=NAMES2, y=NSTAT, fill=NAMES)) +
     geom_bar(stat="identity")+
     theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 50),
        axis.title.x = element_text(size = 65),
        axis.title.y = element_text(size = 65),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
     ylab("Number of Stations Climate")+
     xlab("California Counties") +
  geom_text(aes(label=NSTAT), 
            position=position_dodge(width=0.7),
            vjust=-0.25,  color="blue", size=16)


gg

ggsave("C:\\...\\cali_hist_stations.pdf",
       gg, width = 20 , height = 18)
dev.off()

# library(jsonlite)
# write_json(df_final_perstate, "C:\\...\\df_final_perstate.json")


saveRDS(df_final_percounty, file = "C:\\...\\df_final_percounty.rds")

#df_final_percounty<- readRDS("C:\\...\\df_final_percounty.rds")

##########################
#        kPCA            #
##########################

load("C:/.../kpca_files_cali_allcounties_3features.RData")

# import python modules
pd <- import("pandas")
np <- import("numpy")
sk_dec<-  import("sklearn.decomposition")

#8 values for gamma
#5 values for alfa
#21 kpca one for each county

gamma_vec = c(0.01, 0.1, 0.5, 1, 5, 10, 30, 50 )

alfa_vec = c(0.01, 0.1, 1, 10, 100)


kpca_function<- function(df, kernel, hyper1, hyper2, ncomp){
  
  df_mat<- as.matrix(df)
  kpca_machine<- sk_dec$KernelPCA( kernel= kernel,
                                   fit_inverse_transform= "True",  
                                   gamma= hyper1, 
                                   n_components = int(ncomp),
                                   alpha = hyper2)
  
  
  X_kpca<- kpca_machine$fit_transform( df_mat )
  X_back<- kpca_machine$inverse_transform(X_kpca)
  
  X_list<- list(as.matrix(X_kpca), as.matrix(X_back))
  names(X_list)<- c("X_kpca", "X_back")
  
  return(X_list)
  
}


X_kpca_back =  lapply(1:length(alfa_vec), function (h) 
                 lapply(1:length(gamma_vec), function(i) 
                     lapply(1:length(df_final_percounty), function(j)
                          kpca_function(df = as.matrix(df_final_percounty[[j]][,c(6:8)]), 
                                        kernel = "rbf", 
                                        hyper1 = gamma_vec[i],
                                        hyper2 = alfa_vec[h], 
                                        ncomp = 3  ) )))




#######################################
# MSE distances - Euclidean distances #   
#######################################


dist_kpca<- lapply(1:length(alfa_vec), function(h)
                  lapply(1:length(gamma_vec), function(i) 
                    lapply(1:length(df_final_percounty),  function(j)
                          sapply(1:nrow(X_kpca_back[[h]][[i]][[j]]$X_back), function(k) 
                                          dist( rbind(X_kpca_back[[h]][[i]][[j]]$X_back[k,],
                                                      as.matrix( df_final_percounty[[j]][k,c(6:8)]) ) ) ))))




save(X_kpca_back, dist_kpca, file = "C:\\...\\kpca_files_cali_allcounties_3features.RData" )


#################
#final distances#
#################

d_final =lapply(1:length(alfa_vec), function(h)
                    lapply(1:length(gamma_vec), function(i) 
                                  sapply(1:length(df_final_percounty), function(j)
                                        sum(dist_kpca[[h]][[i]][[j]])/nrow(X_kpca_back[[h]][[i]][[j]]$X_back)   )))

names(d_final)<- alfa_vec


l_d_final<- lapply(1:length(alfa_vec), function(i) data.frame(do.call(rbind, d_final[[i]])) )
 
name_counties<- sapply(1:length(df_final_percounty),  function(i) unique(df_final_percounty[[i]]$COUNTY))

for(i in 1:length(l_d_final)){ colnames(l_d_final[[i]])<- name_counties }
for(i in 1:length(l_d_final)){ rownames(l_d_final[[i]])<- gamma_vec }
for(i in 1:length(l_d_final)){ l_d_final[[i]]<- tibble::rownames_to_column(l_d_final[[i]], "gamma") }




param_df = lapply(1:length(alfa_vec), function(j)
                       data.frame( County = colnames(l_d_final[[j]])[-1],
                       gamma = gamma_vec[sapply(2:ncol(l_d_final[[j]]), function(i) which.min(l_d_final[[j]][,i]))],
                       dist = sapply(2:ncol(l_d_final[[j]]), function(i) min(l_d_final[[j]][,i])),
                       alfa = alfa_vec[j]) )


param_df_2<- do.call(rbind, param_df)

param_df_county = param_df_2 %>% group_split(County)

param_df_county_min =  lapply(1:length(param_df_county), function(i) subset(param_df_county[[i]], 
                                                                            dist==min(dist)) )
param_df_final = as.data.frame(do.call(rbind, param_df_county_min))

param_df_final = format( param_df_final[-3], scientific = F)

save(param_df_final, file = "C:\\...\\param_df_final_cali.RData")

load("C:\\...\\param_df_final_cali.RData")

param_df_final_for_kpca<- param_df_final


param_df_final_for_kpca$gamma_pos = sapply(1:dim(param_df_final_for_kpca)[1], function(i) which(gamma_vec == as.numeric( param_df_final_for_kpca$gamma[i])))

param_df_final_for_kpca$alfa_pos = sapply(1:dim(param_df_final_for_kpca)[1], function(i) which(alfa_vec == as.numeric( param_df_final_for_kpca$alfa[i])))


###################################
# Select the optimal kpca indices #
###################################

kpca_index<- lapply(1:3, function(j) lapply(1:dim(param_df_final_for_kpca)[1], function(i)
  X_kpca_back[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]][[i]]$X_kpca[,j]  ))

for(i in 1:3){
  
  names(kpca_index[[i]]) <-param_df_final_for_kpca$County
  
}

save(kpca_index, file = "C:\\...\\kpca_opt_index_cali_weath.RData")

load( "C:\\...\\kpca_opt_index_cali_weath.RData")




#####################################
# Select the optimal GAMMA hyper.   # 
#####################################

gamma_optimal =   as.numeric(param_df_final_for_kpca$gamma)
alfa_optimal =   as.numeric(param_df_final_for_kpca$alfa)


save(gamma_optimal, alfa_optimal, file = "C:\\...\\opthyper_cali_weath.RData")

load("C:\\...\\opthyper_cali_weath.RData")

########################################
# We know evaluate a new set of points #
########################################

#how to set a mesh? I selected minimum and maximum of each state per variables (tot_prep_s, avg_temp_s, avg_vol_s)
#and then select the biggest minimum and the smallest maximum so to have an in-sample
#grid that applies to each state and evaluate that for each state


range_col<- lapply(1:length(df_final_percounty), function(i)
                    colRanges(as.matrix(df_final_percounty[[i]][,c(6:8)])))

features<- colnames(df_final_percounty[[1]][,c(6:8)])

for(i in 1:length(df_final_percounty)){ row.names(range_col[[i]])<- features }
for(i in 1:length(df_final_percounty)){ colnames(range_col[[i]])<- c("min", "max") }


list_ranges_min = t(sapply(range_col, function(x) x[,1]))
list_ranges_max = t(sapply(range_col, function(x) x[,2]))


min_new_mesh = matrix(colMins(list_ranges_min), 1, 3) 
max_new_mesh= matrix(colMaxs(list_ranges_max), 1, 3)


colnames(min_new_mesh)<- features
colnames(max_new_mesh)<- features


############
# New Mesh #
############

N<- 100

N <- 1000

new_meshes<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))

final_meshes<- do.call(cbind, new_meshes)
colnames(final_meshes) <- features


##########################
#  Evaluate optimal kpca #
##########################

eigen_dec<- readRDS("C:\\...\\eigen_dec_kpca_opt_cali.rds") 

source_python('C:\\...\\stepwise_kpca.py')


#I now compute the eigenvalues and the eigenvectors of the optimals kpca evaluated at the points
#of the original df_Final_perstate

eigen_dec <- lapply(1:length(gamma_optimal), function(i)
                      stepwise_kpca(X = as.matrix(df_final_percounty[[i]][, c(6:8)]),
                      gamma = gamma_optimal[i],
                      n_components = int(3))  )

saveRDS(eigen_dec, file = "C:\\...\\eigen_dec_kpca_opt_cali.rds") 

################################################################
# We now construct a projection of the new mesh by using these #
#sets of eigenvalues and eigenfunctions to check results above #
################################################################

#new_X_kpca<- readRDS("C:\\...\\California\\new_X_kpca_cali_100.rds")
##new_X_kpca<- readRDS("C:\\...\\California\\new_X_kpca_cali_1000.rds")


source_python('C:\\...\\project_x.py')

lambdas = lapply(1:length(eigen_dec), function(i) do.call(cbind, eigen_dec[[i]][[2]]))

new_X_kpca =   lapply(1:length(df_final_percounty), function(i) sapply(1:N, function(j)   
                                      project_x(x_new = final_meshes[j,],
                                                X= as.matrix(df_final_percounty[[i]][, c(6:8)]),
                                                gamma = gamma_optimal[i],
                                                alphas = eigen_dec[[i]][[1]],
                                                lambdas = lambdas[[i]]) ))

saveRDS(new_X_kpca, file = "C:\\...\\new_X_kpca_cali_100.rds") 

saveRDS(new_X_kpca, file = "C:\\...\\new_X_kpca_cali_1000.rds") 

###################################
#  PLOT EIGENVALUES*EIGENVECTORS  # 
###################################


eigen_val_scaled<- lapply(1:length(eigen_dec),function(i)
                        sapply(1:3, function(j) lambdas[[i]][,j]* eigen_dec[[i]][[1]][,j] ))

feat_kpca_name = c("Eigen 1", "Eigen 2", "Eigen 3" )

eigen_val_scaled_2<- lapply(1:length(eigen_val_scaled), function(i) as.data.frame(eigen_val_scaled[[i]]))
for(i in 1:length(eigen_val_scaled_2)){colnames(eigen_val_scaled_2[[i]])<- feat_kpca_name}
for(i in 1:length(eigen_val_scaled_2)){eigen_val_scaled_2[[i]]$County<- rep(unique(df_final_percounty[[i]]$COUNTY),
                                                                     dim(eigen_val_scaled_2[[i]])[1])}


eigen_val_scaled_2<- lapply(1:length(eigen_val_scaled_2), function(i) reshape2::melt(eigen_val_scaled_2[[i]]) )
for(i in 1:length(eigen_val_scaled_2)){ eigen_val_scaled_2[[i]]$x = rep(1:(dim(eigen_val_scaled_2[[i]])[1]/3), 3) }

eigen_val_scaled_2<-  do.call(rbind, eigen_val_scaled_2)
eigen_val_scaled_2$ind_var = ifelse(eigen_val_scaled_2$variable == "Eigen 1", 1, 
                                    ifelse(eigen_val_scaled_2$variable == "Eigen 2", 2,3))


to_plot_eigen_dec<- function(n_var, df){
   
  df_toplot<- if(n_var == 1){
                       subset(df, ind_var == 1 ) 
                  }else if(n_var == 2){
                             subset(df, ind_var == 2)
                  } else if (n_var == 3) {subset(df, ind_var == 3)
                    }else {subset(df, (ind_var == 1 | ind_var == 2  | ind_var == 3))}
  return(df_toplot = df_toplot)
  
}

df_toplot = to_plot_eigen_dec(4, eigen_val_scaled_2)


ggplot(df_toplot, aes(x = as.Date(x) ,  y= value, color = as.factor(variable) )) +
  facet_wrap(~County) +
  geom_line() + 
  theme_bw() +
  scale_x_date(name = "") +
  theme(legend.position = "left",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_color_discrete(name="Variable") +
  ylab( expression(alpha*lambda))





###############################
#   Plot of the new Kpca      #    
###############################         

features[3]<- "tot_prcp_s"

to_plot_kpca<- lapply(1:length(new_X_kpca), function(i) t(new_X_kpca[[i]]) )

to_plot_kpca<- lapply(1:length(new_X_kpca), function(i) as.data.frame(to_plot_kpca[[i]]) )

feat_to_plot_kpca<- c(expression(phi_1), expression(phi_2), expression(phi_3))

for(i in 1:length(to_plot_kpca)){ colnames(to_plot_kpca[[i]]) <- feat_to_plot_kpca }

for(i in 1:length(to_plot_kpca)){to_plot_kpca[[i]]$County<- rep(unique(df_final_percounty[[i]]$COUNTY),
                                                                           dim(to_plot_kpca[[i]])[1])}

to_plot_kpca<- lapply(1:length(to_plot_kpca), function(i) reshape2::melt(to_plot_kpca[[i]]) )

for(i in 1:length(to_plot_kpca)){ to_plot_kpca[[i]]$x = rep(1:(dim(to_plot_kpca[[i]])[1]/3), 3) }


to_plot_kpca<-  do.call(rbind, to_plot_kpca)
to_plot_kpca$ind_var = ifelse(to_plot_kpca$variable == "phi_1", 1, 
                                    ifelse(to_plot_kpca$variable == "phi_2", 2,3))


to_plot_new_kpca<- function(n_var, df){
  
  df_toplot<- if(n_var == 1){
    subset(df, ind_var == 1 ) 
  }else if(n_var == 2){
    subset(df, ind_var == 2)
  } else if (n_var == 3) {subset(df, ind_var == 3)
  }else {subset(df, (ind_var == 1 | ind_var == 2  | ind_var == 3))}
  return(df_toplot = df_toplot)
  
}

df_toplot = to_plot_new_kpca(4, to_plot_kpca)


ggplot(df_toplot, aes(x = as.Date(x) ,  y= value, color = as.factor(variable) )) +
  facet_wrap(~County) +
  geom_line() + 
  theme_bw() +
  scale_x_date(name = "") +
  theme(legend.position = "left",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_color_discrete(name="Variable", 
                                                             labels = c('phi_1' = expression(varphi[1]),
                                                                        'phi_2' = expression(varphi[2]),
                                                                        'phi_3' = expression(varphi[3])))




##################################################
#             SCATTERPLOTs                      # -
#################################################


toplot_scatter_kpca<- function(kpc_n, df , var1, var2){
  
  df_toplot <- do.call(rbind, df)
  df_toplot_filtered<- df_toplot[,c("COUNTY",var1,var2)]       
  df_toplot_kpca<- melt(kpca_index[[kpc_n]])
  names(df_toplot_kpca) <- c( paste("KPC",kpc_n, sep = ""), "County")
  final_toplot<- cbind(df_toplot_kpca,
                       df_toplot_filtered)[,-3]
  gg = ggplot(final_toplot, aes(x= final_toplot[,3] ,  y= final_toplot[,4], color = final_toplot[,1]) ) + 
       facet_wrap(~County, scales = "free")+
       geom_point(size=2) +
       theme_bw() +
       scale_color_gradient(low="blue", high="red") +
       theme(legend.position = "left") +
       xlab(names(final_toplot)[3])+
       ylab(names(final_toplot)[4])  + labs(color=names(final_toplot)[1]) 
       return(gg = gg)
  
}

gg<- toplot_scatter_kpca(1, df_final_percounty, "temp_avg_s", "avg_vol_s")
gg

gg<- toplot_scatter_kpca(1, df_final_percounty, "temp_avg", "avg_vol")
gg

gg<- toplot_scatter_kpca(1, df_final_percounty, "temp_avg", "tot_prcp")
gg


gg<- toplot_scatter_kpca(1, df_final_percounty, "tot_prcp", "avg_vol")
gg




