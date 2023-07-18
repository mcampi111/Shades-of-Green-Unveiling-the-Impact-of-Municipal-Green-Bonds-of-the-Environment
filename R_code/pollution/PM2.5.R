library(rnoaa)
library(dplyr)
library(sp)
library(lawn)
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
library(plyr)


setwd("C:\\...\\PM2.5_FRM_FEM") #set working directory

mydir<- list.files(getwd())

n_files<- length(mydir)

myfile<-  lapply(1:n_files, function(i) read_csv(mydir[[i]]))


#########################################
# Available monitors within 2010 - 2020 #
#########################################

states_gg <- map_data("state")

# In this page -->https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files
# summary of the data
#
# Note that :The daily summary files contain (at least) one record for each monitor that reported
# data for the given day. There may be multiple records for the monitor.

sapply(1:n_files, function(i) unique(myfile[[i]]$`Sample Duration`))

sapply(1:n_files, function(i) unique(myfile[[i]]$`Pollutant Standard`))


# Note that :The daily summary files contain (at least) one record for each monitor that reported
# data for the given day. There may be multiple records for the monitor.
#
# In this case I chose for Pollutant Standard: "PM25 24-hour 2012"
# which gives the same results of Sample Duration: "24 HOUR" -->more complete than 1 HOUR


myfile_v1<- lapply(1:n_files, function(i) filter(myfile[[i]], `Sample Duration` == "24 HOUR") )


all_stat_10_year<- lapply(1:n_files, function(i) data.frame(Lat = myfile_v1[[i]]$Latitude[which(diff(myfile_v1[[i]]$Latitude) != 0)],
                                                           Lon = myfile_v1[[i]]$Longitude[which(diff(myfile_v1[[i]]$Longitude) != 0)] ) )

all_stat_10_year<- do.call(rbind, all_stat_10_year)
all_stat_10_year<- all_stat_10_year %>% filter((Lon < -60) & (Lon > -130) &  (Lat < 50) & (Lat > 20) )
all_stat_10_year<- unique(all_stat_10_year)

all_stat_10_year_pm25 = all_stat_10_year

save(all_stat_10_year_pm25, file = "C:\\...\\all_stat_pm25.RData")


#Map of the available stations
ggplot(data = states_gg) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = all_stat_10_year,
             aes(x = Lon,
                 y = Lat),
             alpha = 0.6,
             size = 2,
             color = 'green') +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Available stations in the US for the PM 2.5") +
  theme(legend.position = "right") 


###################
#   CALIFORNIA    #
###################

county_gg<- map_data("county")
california <- subset(county_gg, region %in% c("california"))

all_stat_10_year_cali <-  all_stat_10_year  %>% filter((Lon < -115) & (Lon > -125) &  (Lat < 42) & (Lat > 32) )


ggplot(data = california) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = all_stat_10_year_cali,
             aes(x = Lon,
                 y = Lat),
             alpha = 0.6,
             size = 2,
             color = 'green') +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Available stations in California for the PM 2.5") +
  theme(legend.position = "right") 




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

n_city = dim(major_city_us_pop_max)[1]

dist_mon_city = lapply(1:n_city, function(j)
                  sapply(1:dim(all_stat_10_year)[1], function(i)
                    distVincentyEllipsoid( cbind(major_city_us_pop_max$long[j], major_city_us_pop_max$lat[j]),
                                       rev(all_stat_10_year[i,]))))


dist_mon_city_km = lapply(1:n_city, function(j) dist_mon_city[[j]]/1000)


tresh = 50

dist_mon_city_km_less_trs =  lapply(1:n_city, function(j) which(dist_mon_city_km[[j]]< tresh))


stats_by_city_lat_lon<- lapply(1:n_city, function(i) all_stat_10_year[dist_mon_city_km_less_trs[[i]],] )

empty_cities = which(sapply(1:n_city, function(i) empty(stats_by_city_lat_lon[[i]])) == TRUE)

n_empty_cities = major_city_us_pop_max[empty_cities,]$name
n_empty_cities

final_latlon_stats<- do.call(rbind, stats_by_city_lat_lon)

final_latlon_stats_pm25 = final_latlon_stats

save(final_latlon_stats_pm25, file = "C:\\...\\final_stat_pm25.RData")

###############################################
# FINAL MAP SELECTED STATIONS --> RADIUS 50KM #
###############################################

ggplot(data = states_gg) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = final_latlon_stats,
             aes(x = Lon,
                 y = Lat),
             alpha = 0.6,
             size = 5,
             color = 'green') +
  geom_point(data = major_city_us_pop_max,
             aes(x = long, y = lat, size = pop),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma)  +
  labs(title = "Selected PM 2.5 Monitors in the US") +
  theme(legend.position = "right") 


###############################################
#                CALIFORNIA                   #
###############################################
###############################################
# FINAL MAP SELECTED STATIONS --> RADIUS 50KM #
###############################################

final_latlon_stats_cali <- final_latlon_stats  %>% filter((Lon < -116) & (Lon > -125) &  (Lat < 42) & (Lat > 32) )

major_city_us_pop_max_cali<- major_city_us_pop_max %>% filter(country.etc == "CA")

gg = ggplot(data = california) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = final_latlon_stats_cali,
             aes(x = Lon,
                 y = Lat),
             alpha = 0.6,
             size = 5,
             color = 'green') +
  geom_point(data = major_city_us_pop_max_cali,
             aes(x = long, y = lat, size = pop),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma)  +
  labs(title = "Selected PM 2.5 Monitors in California") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))  +
  ylab("Latitude") + xlab("Longitude") 
gg

ggsave("C:\\...\\cali_pm25_stations.pdf",
       gg, width = 5 , height = 5)





#####################################################
#  Filter according to the final dataset of lon/lat #
#####################################################

#Divided per n_files --> year 
#Divided per sit_num --> monitor

filter_my_files<- lapply(1:n_files, function(i)
                    lapply(1:dim(final_latlon_stats)[1], function(j) 
                      filter(myfile_v1[[i]] , Latitude == final_latlon_stats$Lat[j] )))

#Divided only per n_files --> year

filter_my_files_year <- lapply(1:n_files, function(i) do.call(rbind, filter_my_files[[i]]))


## All together

filter_my_files_final<- do.call(rbind, filter_my_files_year)

####################
# Divided by State #
####################


my_file_state <- filter_my_files_final %>% group_split(`State Name`)  


n_states<- length(my_file_state)

# N stations x N days per state --> rows
sapply(1:n_states, function(i) dim(my_file_state[[i]])[1])

########################################
# I applied this step at the beginning #
########################################
#my_file_state_1h<- lapply(1:n_states, function(i) filter(my_file_state[[i]], `Sample Duration` == "1 HOUR") )

names_states =  sapply(1:n_states, function(i) unique(my_file_state[[i]]$`State Name`))

l_name_stations<- lapply(1:n_states, function(i)  unique(my_file_state[[i]]$`Local Site Name` ))

names_states_pm25 = names_states
l_name_stations_pm25 = l_name_stations

save(names_states_pm25, l_name_stations_pm25, file = "C:\\...\\hist_pm25.RData")

###########################    
#final number of stations #  
###########################  

n_stats<- sapply(1:n_states, function(i) length(l_name_stations[[i]]) )


df_station_bar<- data.frame(NSTAT = n_stats,  NAMES = names_states )

gg = ggplot(data=df_station_bar, aes(x=NAMES, y=NSTAT, fill=NAMES)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 40),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Number of Stations PM 2.5")+
  xlab("US States")+
  geom_text(aes(label=NSTAT), 
            position=position_dodge(width=0.7),
            vjust=-0.25, color="blue", size=6)

gg

ggsave("C:\\...\\pm25_n_stations_US.pdf",
       gg, width = 18 , height = 16)
dev.off()



#########################################################
# Average the value of PM 2.5 across stations per each day #   
#########################################################

#dimension of each state
sapply(1:n_states, function(i) dim(my_file_state[[i]])[1])


#number of days (unique) per state (across the sations)
sapply(1:n_states, function(i) length(unique(my_file_state[[i]]$`Date Local`)))

#years per statte
sapply(1:n_states, function(i) unique(year(my_file_state[[i]]$`Date Local`)))


pm_25_avg = lapply(1:n_states, function(i)
  my_file_state[[i]] %>% group_by(`Date Local`) %>% dplyr::summarise(pm_25_avg = mean(`Arithmetic Mean`, na.rm=TRUE)))


final_pm25avg_state = lapply(1:n_states, function(i) 
  data.frame(State = unique(my_file_state[[i]]$`State Name`), pm_25_avg[[i]] ))


save(final_pm25avg_state, my_file_state, 
     file = "C:/.../pm25_avg_files.RData")


##################################
#  STATE EXPERIMENTS: CALIFORNIA #
##################################


###################
#   CALIFORNIA    #
###################


cali_pm25 = my_file_state[[3]]

#Separate by counties

cali_pm25_county<- cali_pm25 %>% group_split(`County Name`)

names_counties =  sapply(1:length(cali_pm25_county), function(i) unique(cali_pm25_county[[i]]$`County Name`))

#note: need to aggregate them for the city or to select them for the city
sapply(1:length(cali_pm25_county), function(i) unique(cali_pm25_county[[i]]$`City Name`))
###############################################################################################
#same in AQI and CO and PM2.5


l_name_stations_cali<- lapply(1:length(cali_pm25_county), function(i)  
  unique(cali_pm25_county[[i]]$`Local Site Name` ))


n_stats_cali<- sapply(1:length(cali_pm25_county), function(i) length(l_name_stations_cali[[i]]) )


df_station_bar_cali<- data.frame(NSTAT = n_stats_cali,  NAMES = names_counties )

gg = ggplot(data=df_station_bar_cali, aes(x=NAMES, y=NSTAT, fill=NAMES)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 50),
        axis.title.x = element_text(size = 65),
        axis.title.y = element_text(size = 65),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Number of Stations PM 2.5")+
  xlab("California Counties")+
  geom_text(aes(label=NSTAT), 
            position=position_dodge(width=0.7),
            vjust=-0.25,  color="blue", size=16)


gg

ggsave("C:\\...\\pm25_n_stations_Cali.pdf",
       gg, width = 20 , height = 18)
dev.off()



# Average the value of PM25 across stations per each day 

pm25_avg_st_cali = lapply(1:length(cali_pm25_county), function(i)
  cali_pm25_county[[i]] %>% group_by(`Date Local`) %>% dplyr::summarise(pm_25_avg = mean(`Arithmetic Mean`, na.rm=TRUE)))


final_pm25avg_cali = lapply(1:length(cali_pm25_county), function(i) 
  data.frame(County = unique(cali_pm25_county[[i]]$`County Name`), pm25_avg_st_cali[[i]] ))

save(final_pm25avg_cali, cali_pm25_county, 
     file = "C:/.../pm25_avg_files_cali.RData")






