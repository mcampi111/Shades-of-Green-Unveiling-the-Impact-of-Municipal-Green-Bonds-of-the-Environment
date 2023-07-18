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


###################
# KPCA by US City #
###################

########################### 
#     FUNCTIONS UTILS     #
###########################

sigma_par<- function(low,high){
  
  N<- 1
  n<- 30
  const<- N/(4*n*log(2))
  ll<- log( abs( high/low   )  )^2
  ll[which(ll == Inf)] = 0
  sig_var<- sqrt( const *sum( ll , na.rm = TRUE ) )
  
  return(sig_var)
}





lags <- function(var, n=10){
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("lag_%s_%02d", quo_text(var), indices))
  
}


#FINAL FUNCTION 
calculate_sigma_pk<- function(df){
  
  df2 = df %>% group_by(YEAR, MONTH) %>% 
    mutate(!!!lags(MIN, 30), !!!lags(MAX,30))
  
  df2[["id"]] <- 1:nrow(df2)
  
  df_lag_min <- df2 %>%
    ungroup()  %>%
    select(id, contains("lag_MIN")) %>%
    gather(key = "name", value = "lag_MIN", -id) %>%    
    mutate(number = gsub("lag_MIN_", "" ,name) )  %>%
    mutate(number2 = as.numeric(number)) %>%
    mutate(number3 = rep(df2$YEAR,30))  %>%
    mutate(number4 = rep(df2$MONTH,30)) %>%
    select(-name)
  
  
  df_lag_max <- df2 %>%
    ungroup()  %>%
    select(id, contains("lag_MAX")) %>%
    gather(key = "name", value = "lag_MAX", -id) %>%
    mutate(number = gsub("lag_MAX_", "" ,name) )  %>%
    mutate(number2 = as.numeric(number))  %>%
    mutate(number3 = rep(df2$YEAR,30)) %>%
    mutate(number4 = rep(df2$MONTH,30)) %>%
    select(-name)
  
  
  df.fin<- left_join(df_lag_min , df_lag_max)   %>% group_by(number3, number4, number2  ) %>% 
    mutate(sigma_pk = sigma_par(low = lag_MIN, high = lag_MAX  ))
  
  sigma_pk_final<- unique(df.fin$sigma_pk)
  
  return(sigma_pk_final)
  
}




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

states_gg <- map_data("state")

#Map of the cities
ggplot(data = states_gg) +
      geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
      coord_fixed(1.3) +
      geom_point(data = major_city_us_pop_max,
             aes(x = long, y = lat, size = pop),
             color = "purple", alpha = 0.5) +
      scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
      labs(title = "Most Populous City in Each US State",
       subtitle = "Source: US Census 2010",
       size = "City Population") +
       theme(legend.position = "right") 


#Map of the cities and the available stations
ggplot(data = states_gg) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3) +
  geom_point(data = US_station,
             aes(x = LON,
                 y = LAT),
             alpha = 0.6,
             size = 2,
             color = 'green') +
  geom_point(data = major_city_us_pop_max,
             aes(x = long, y = lat, size = pop),
             color = "purple", alpha = 0.5) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Most Populous City in Each US State",
       subtitle = "Source: US Census 2010",
       size = "City Population") +
  theme(legend.position = "right") 


#Extracting stations close (within 50km) to the given cities#
#years: 2010 -2020                                         #
############################################################
#We want 10 sensors at least per city

lat_lon_df = data.frame(id = cbind(major_city_us_pop_max$name), 
                        latitude = cbind(major_city_us_pop_max$lat),
                        longitude = cbind(major_city_us_pop_max$long))


mt_stat_mj_city_pmax_GSODR = lapply(1:(dim(lat_lon_df)[1]),function(i) 
                                                    nearest_stations(LAT = lat_lon_df$latitude[i],
                                                                    LON = lat_lon_df$longitude[i],
                                                                    distance = 50))


list_meteo_stat_GSODR <- lapply(mt_stat_mj_city_pmax_GSODR, as.data.frame)
names(list_meteo_stat_GSODR)<- lat_lon_df$id 

n_stat<- sapply(1:length(list_meteo_stat_GSODR), function(i) dim(list_meteo_stat_GSODR[[i]])[1] )

min_nstat<- min(n_stat)

df_station_bar<- data.frame(NSTAT = n_stat,  NAMES =names(list_meteo_stat_GSODR) )

ggplot(data=df_station_bar, aes(x=NAMES, y=NSTAT, fill=NAMES)) +
           geom_bar(stat="identity")+
           theme_minimal() +
           theme(legend.position = "none",
                 axis.text.x = element_text(angle = 90, size = 8))+
           ylab("Number of Stations")+
           xlab("US Cities")+
           geom_text(aes(label=NSTAT), vjust=1.6, color="blue", size=2.5)


l_mt_stat_mj_city_pmax_GSODR = as.data.frame(do.call(rbind, list_meteo_stat_GSODR))
colnames(l_mt_stat_mj_city_pmax_GSODR)<- "id"
l_mt_stat_mj_city_pmax_GSODR = rownames_to_column(l_mt_stat_mj_city_pmax_GSODR, "station") 



meteo_variables_GSOD <- get_GSOD(years = 2010:2020, station = l_mt_stat_mj_city_pmax_GSODR$id)

#save(meteo_variables_GSOD,  file = "C:\\...\\meteo_var_GSOD_us_v2.RData")


