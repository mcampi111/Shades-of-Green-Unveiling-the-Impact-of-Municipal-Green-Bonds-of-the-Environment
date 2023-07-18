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
library(matrixStats)



#################
# KPCA by state #
#################


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

my_meteo_var_GSODR<- meteo_variables_GSOD[,c(1:15,19,21,29,31,33,42:44)]

#########################################
#       plots of the time series        #     ADD IN HERE!!!!
#########################################


##############################################################
#  I Group them by states --> I contruct an index per state  #
##############################################################

list_my_met_var_bystate<- my_meteo_var_GSODR %>% group_split(STATE)

###########################   
#      Data Cleaning      #   
###########################   

#BEFORE COMPUTING HISTORICAL VOLATILITY AND OTHER SUMMARY STATISTICS, WE NEED TO FURTHER
#CLEAN THE DATA AND REMOVE STATIONS WHICH DO NOT HAVE THE DUE AMOUNT OF DAYS PER YEAR

new_list_my_met_var_bystate_stnid<- lapply(1:length(list_my_met_var_bystate), function(i) 
  list_my_met_var_bystate[[i]] %>% group_split(STNID) )


new_list_my_met_var_bystate_stnid_year<- lapply(1:length(new_list_my_met_var_bystate_stnid), function(i)
  lapply(1:length(new_list_my_met_var_bystate_stnid[[i]]), function(j)
    new_list_my_met_var_bystate_stnid[[i]][[j]] %>% group_split(YEAR) ))


n_year_by_stnid = lapply(1:length(new_list_my_met_var_bystate_stnid_year), function(h)
  lapply(1:length(new_list_my_met_var_bystate_stnid_year[[h]]), function(j)
    sapply(1:length(new_list_my_met_var_bystate_stnid_year[[h]][[j]]), 
           function(i) dim(new_list_my_met_var_bystate_stnid_year[[h]][[j]][[i]])[1])))


l_name_station<- lapply(1:length(new_list_my_met_var_bystate_stnid), function(j)
  sapply(1:length(new_list_my_met_var_bystate_stnid[[j]]), function(i) 
    unique(new_list_my_met_var_bystate_stnid[[j]][[i]]$NAME))  )



for(i in 1:length(n_year_by_stnid)){
  names(n_year_by_stnid[[i]]) <- l_name_station[[i]]
}


list_stat_by_year_stnid_final<- lapply(1:length(n_year_by_stnid), function(i) 
  t(sapply(n_year_by_stnid[[i]], "length<-", max(lengths(n_year_by_stnid[[i]])))))



to_rmv_less_30<- sapply(1:length(list_stat_by_year_stnid_final), function(i) 
  which(list_stat_by_year_stnid_final[[i]] < 30, arr.ind = T))

names_to_rmv_less_30<- lapply(1:length(to_rmv_less_30), function(i) rownames(to_rmv_less_30[[i]]))


names_selected_stations<- lapply(1:length(l_name_station), function(i)
  l_name_station[[i]][!(l_name_station[[i]]) %in% names_to_rmv_less_30[[i]]])



#Now we filter the original dataset by states (list_my_met_var_bystate) according to
#names_selected_stations

new_list_my_met_var_bystate<- lapply(1:length(list_my_met_var_bystate), function(i)
  list_my_met_var_bystate[[i]] %>% filter(NAME %in% names_selected_stations[[i]])  )



final_dataset_all_stations<- do.call(rbind, new_list_my_met_var_bystate)


###########################    #######################################################
#final number of stations #  - # I do this later again for another step of filtering #
###########################    #######################################################

n_stat2<- sapply(1:length(names_selected_stations), function(i) length(names_selected_stations[[i]]) )

min_nstat<- min(n_stat2)

df_station_bar2<- data.frame(NSTAT = n_stat2,  NAMES = unique(final_dataset_all_stations$STATE) )

ggplot(data=df_station_bar2, aes(x=NAMES, y=NSTAT, fill=NAMES)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 8))+
  ylab("Number of Stations")+
  xlab("US States")+
  geom_text(aes(label=NSTAT), vjust=1.6, color="blue", size=2.5)


###########################   
#    Summary Statistics   #   
###########################   

#SPLINE were MAX and MIN are missing by STNID, MONTH
new_list_my_met_var_bystate_spline = lapply(1:length(new_list_my_met_var_bystate), function(i) 
  new_list_my_met_var_bystate[[i]] %>% group_by(STNID, MONTH) %>% mutate(new_MAX = na.spline(MAX),
                                                                         new_MIN = na.spline(MIN)))

#COMPUTE HISTORICAL VOLATILITY/PK VOLATILITY by grouping them by year, stnid

new_list_my_met_var_bystate_vol = lapply(1:length(new_list_my_met_var_bystate_spline), function(i)
  new_list_my_met_var_bystate_spline[[i]] %>% group_by(STNID) %>%
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


new_list_my_met_var_bystate_vol_shift<- lapply(1:length(new_list_my_met_var_bystate_vol), function(i)
  shift.column(new_list_my_met_var_bystate_vol[[i]], "Vol", len = 30, up = TRUE, newNames = "Vol_shift")   )



#remove all the day without Vol, i.e. NA per Vol --> we kill VOl_shift and don't need Vol anymore
new_list_my_met_var_bystate_vol_na =  lapply(1:length(new_list_my_met_var_bystate_vol_shift), function(i)
  new_list_my_met_var_bystate_vol_shift[[i]] %>% drop_na(Vol_shift)  )

#We compute again our final dataframe and then take:
#Compute maximum unique number of days - do this here

final_dataset_all_stations_vol<- do.call(rbind, new_list_my_met_var_bystate_vol_na)


un_year<- unique(final_dataset_all_stations_vol$YEARMODA)

ts <- seq(ymd(un_year[1]), ymd(un_year[length(un_year)]), by="day")


new_list_my_met_var_bystate_vol_complete<- lapply(1:length(new_list_my_met_var_bystate_vol_na), function(i)
  new_list_my_met_var_bystate_vol_na[[i]] %>%  complete(YEARMODA = seq(ymd(ts[1]), 
                                                                       ymd(ts[length(ts)]),
                                                                       by="day")) )



#Now we compute average temp per day, tot prep per day and average of hist volatility per day

df_avg_temp_prep_vol = lapply(1:length(new_list_my_met_var_bystate_vol_complete), function(i)
  new_list_my_met_var_bystate_vol_complete[[i]] %>% group_by(YEARMODA) %>%
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
#In the three states where NAs are found, this is due to the scaling of tot_prcp which is 0
#almost everywhere --> I therefore substitute those NAs with 0t

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



df_final_perstate<- lapply(1:length(df_avg_temp_prep_vol_4), function(i) 
                       cbind( STATE = rep(unique(new_list_my_met_var_bystate_vol_na[[i]]$STATE),
                                                 dim(df_avg_temp_prep_vol_4[[i]])[1] ),
                              df_avg_temp_prep_vol_4[[i]]) ) 


#I drop the first one since it contains not relevant information (CAN)

df_final_perstate <- df_final_perstate[-1]

length(df_final_perstate)

final_dataset<- do.call(rbind, df_final_perstate)




###########################  
#final number of stations #  
###########################  

l_name_station_final <- l_name_station[-1]


n_stat2<- sapply(1:length(l_name_station_final), function(i) length(l_name_station_final[[i]]) )

min_nstat<- min(n_stat2)

df_station_bar2<- data.frame(NSTAT = n_stat2,  NAMES = unique(final_dataset$STATE) )

ggplot(data=df_station_bar2, aes(x=NAMES, y=NSTAT, fill=NAMES)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, size = 8))+
  ylab("Number of Stations")+
  xlab("US States")+
  geom_text(aes(label=NSTAT), vjust=1.6, color="blue", size=2.5)





##########################
#        kPCA            #
##########################


# import pytho modules
pd <- import("pandas")
np <- import("numpy")
sk_dec<-  import("sklearn.decomposition")


gamma_vec = c(0.01, 0.1, 0.5, 1, 5, 10, 30, 50 )

alfa_vec = c(0.01, 0.1, 1, 10, 100)


kpca<- lapply(1:length(alfa_vec), function(j)
  lapply(1:length(gamma_vec), function(i) sk_dec$KernelPCA( kernel="rbf",
                                                            fit_inverse_transform= "True", 
                                                            gamma= gamma_vec[i], 
                                                            n_components = 3 ,
                                                            alpha = alfa_vec[j])) )


#8 values for gamma
#5 values for alfa
#35 kpca one for each state

#NOTE: IF YOU USE FIT INSTEAD OF FIT TRANSFORM --> IT WILL OVERWRITE THE EXISTING 
#LIST AND THE RESULTS WILL ALL BE EQUAL

#NOTE: FIT_TRANSFORM RETURNS PHI(X) AND NOT EIGENVECTORS

X_kpca<- lapply(1:length(alfa_vec), function(h)
                     lapply(1:length(gamma_vec), function(i) 
                         lapply(1:length(df_final_perstate), function(j)
                            kpca[[h]][[i]]$fit_transform( as.matrix(df_final_perstate[[j]][,c(6:8)]) )  )) ) 




X_back<-  lapply(1:length(alfa_vec), function(h)
                    lapply(1:length(gamma_vec), function(i) 
                         lapply(1:length(df_final_perstate),  function(j)
                           kpca[[h]][[i]]$inverse_transform(X_kpca[[h]][[i]][[j]] ) )))



#######################################
# MSE distances - Euclidean distances #   
#######################################


dist_kpca<- lapply(1:length(alfa_vec), function(h)
                  lapply(1:length(gamma_vec), function(i) 
                    lapply(1:length(df_final_perstate),  function(j)
                          sapply(1:nrow(X_back[[h]][[i]][[j]]), function(k) 
                                          dist( rbind( X_back[[h]][[i]][[j]][k,],
                                                      as.matrix( df_final_perstate[[j]][k,c(6:8)]) ) ) ))))




#save(X_kpca,X_back, dist_kpca, file = "C:\\...\\kpca_files_allstates_3features_new.RData" )


#################
#final distances#
#################

d_final =lapply(1:length(alfa_vec), function(h)
                    lapply(1:length(gamma_vec), function(i) 
                                  sapply(1:length(df_final_perstate), function(j)
                                        sum(dist_kpca[[h]][[i]][[j]])/nrow(X_back[[h]][[i]][[j]])   )))

names(d_final)<- alfa_vec


l_d_final<- lapply(1:length(alfa_vec), function(i) data.frame(do.call(rbind, d_final[[i]])) )
 
name_states<- sapply(1:length(df_final_perstate),  function(i) unique(df_final_perstate[[i]]$STATE))

for(i in 1:length(l_d_final)){ colnames(l_d_final[[i]])<- name_states }
for(i in 1:length(l_d_final)){ rownames(l_d_final[[i]])<- gamma_vec }
for(i in 1:length(l_d_final)){ l_d_final[[i]]<- tibble::rownames_to_column(l_d_final[[i]], "gamma") }




param_df = lapply(1:length(alfa_vec), function(j)
                       data.frame( State = colnames(l_d_final[[j]])[-1],
                       gamma = gamma_vec[sapply(2:ncol(l_d_final[[j]]), function(i) which.min(l_d_final[[j]][,i]))],
                       dist = sapply(2:ncol(l_d_final[[j]]), function(i) min(l_d_final[[j]][,i])),
                       alfa = alfa_vec[j]) )


param_df_2<- do.call(rbind, param_df)

param_df_state = param_df_2 %>% group_split(State)

param_df_state_min =  lapply(1:length(param_df_state), function(i) subset(param_df_state[[i]], 
                                                                          dist==min(dist)) )
param_df_final = as.data.frame(do.call(rbind, param_df_state_min))

param_df_final = format( param_df_final[-3], scientific = F)

param_df_final_for_kpca<- param_df_final


param_df_final_for_kpca$gamma_pos = sapply(1:dim(param_df_final_for_kpca)[1], function(i) which(gamma_vec == as.numeric( param_df_final_for_kpca$gamma[i])))

param_df_final_for_kpca$alfa_pos = sapply(1:dim(param_df_final_for_kpca)[1], function(i) which(alfa_vec == as.numeric( param_df_final_for_kpca$alfa[i])))


###################################
# Select the optimal kpca indices #
###################################

kpca_index<- lapply(1:3, function(j) lapply(1:dim(param_df_final_for_kpca)[1], function(i)
  X_kpca[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]][[i]][,j]  ))

for(i in 1:3){
  
  names(kpca_index[[i]]) <-param_df_final_for_kpca$State
  
}


#####################################
# Select the optimal kpca functions #
#####################################


kpca_optimal_fun = lapply(1:dim(param_df_final_for_kpca)[1], function(i) 
                   kpca[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]])



########################################
# We know evaluate a new set of points #
########################################

#how to set a mesh? I selected minimum and maximum of each state per variables (tot_prep_s, avg_temp_s, avg_vol_s)
#and then select the biggest minimum and the smallest maximum so to have an in-sample
#grid that applies to each state and evaluate that for each state


range_col<- lapply(1:length(df_final_perstate), function(i)
                    colRanges(as.matrix(df_final_perstate[[i]][,c(6:8)])))

features<- colnames(df_final_perstate[[1]][,c(6:8)])

for(i in 1:length(df_final_perstate)){ row.names(range_col[[i]])<- features }
for(i in 1:length(df_final_perstate)){ colnames(range_col[[i]])<- c("min", "max") }


list_ranges_min = t(sapply(range_col, function(x) x[,1]))
list_ranges_max = t(sapply(range_col, function(x) x[,2]))

min_new_mesh = matrix(colMaxs(list_ranges_min), 1, 3)
max_new_mesh= matrix(colMins(list_ranges_max), 1, 3)

colnames(min_new_mesh)<- features
colnames(max_new_mesh)<- features

#need to substitute the one for tot_prcp_s since there are some states in which it is always 0

min_new_mesh[3] <- max(list_ranges_min[,3][list_ranges_min[,3] < 0])
max_new_mesh[3] <- min(list_ranges_max[,3][list_ranges_max[,3] > 0])


############
# New Mesh #
############

N <- 1000

new_meshes<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))

final_meshes<- do.call(cbind, new_meshes)
colnames(final_meshes) <- features



fit_new_kpca_optimal <- lapply(1:length(kpca_optimal_fun), function(i) 
                                kpca_optimal_fun[[i]]$fit(as.matrix(df_final_perstate[[i]][, c(6:8)])))

new_X_kpca<- lapply(1:length(kpca_optimal_fun), function(i) 
                          fit_new_kpca_optimal[[i]]$transform(as.matrix(final_meshes)) )


eigen_dec<- readRDS("C:\\...\\eigen_dec_kpca_opt_allstates.rds") 


#REMEMBER --> WHEN YOU USE PYTHON IN R, ALWAYS CHECK THE TYPE OF THE ARGUMENTS THAT YOU PASS
source_python('C:\\...\\stepwise_kpca.py')


#I now compute the eigenvalues and the eigenvectors of the optimals kpca evaluated at the points
#of the original df_Final_perstate

eigen_dec <- lapply(1:length(kpca_optimal_fun), function(i)
                     stepwise_kpca(X = as.matrix(df_final_perstate[[i]][, c(6:8)]),
                     gamma = kpca_optimal_fun[[i]]$gamma,
                     n_components = int(3))  )

#saveRDS(eigen_dec, file = "C:\\...\\eigen_dec_kpca_opt_allstates.rds") 




################################################################
# We now construct a projection of the new mesh by using these #
#sets of eigenvalues and eigenfunctions to check results above #
################################################################

new_X_kpca<- readRDS("C:\\...\\new_X_kpca_allstates.rds")

source_python('C:\\...\\project_x.py')

lambdas = lapply(1:length(eigen_dec), function(i) do.call(cbind, eigen_dec[[i]][[2]]))

new_X_kpca =   lapply(1:length(df_final_perstate), function(i) sapply(1:N, function(j)
                                     project_x(x_new = final_meshes[j,],
                                               X= as.matrix(df_final_perstate[[i]][, c(6:8)]),
                                               gamma = kpca_optimal_fun[[i]]$gamma,
                                               alphas = eigen_dec[[i]][[1]],
                                               lambdas = lambdas[[i]]) ))

saveRDS(new_X_kpca, file = "C:\\...\\new_X_kpca_allstates.rds") 


###################################
#  PLOT EIGENVALUES*EIGENVECTORS  # 
###################################

eigen_val_scaled<- lapply(1:length(eigen_dec),function(i)
                        sapply(1:3, function(j) lambdas[[i]][,j]* eigen_dec[[i]][[1]][,j] ))

feat_kpca_name = c("Eigen 1", "Eigen 2", "Eigen 3" )

eigen_val_scaled_2<- lapply(1:length(eigen_val_scaled), function(i) as.data.frame(eigen_val_scaled[[i]]))
for(i in 1:length(eigen_val_scaled_2)){colnames(eigen_val_scaled_2[[i]])<- feat_kpca_name}
for(i in 1:length(eigen_val_scaled_2)){eigen_val_scaled_2[[i]]$State<- rep(unique(df_final_perstate[[i]]$STATE),
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
  facet_wrap(~State) +
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

for(i in 1:length(to_plot_kpca)){to_plot_kpca[[i]]$State<- rep(unique(df_final_perstate[[i]]$STATE),
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
  facet_wrap(~State) +
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
#             SCATTERPLOTs                      # 
#################################################


toplot_scatter_kpca<- function(kpc_n, df , var1, var2){
  
  df_toplot <- do.call(rbind, df)
  df_toplot_filtered<- df_toplot[,c("STATE",var1,var2)]       
  df_toplot_kpca<- melt(kpca_index[[kpc_n]])
  names(df_toplot_kpca) <- c( paste("KPC",kpc_n, sep = ""), "State")
  final_toplot<- cbind(df_toplot_kpca,
                       df_toplot_filtered)[,-3]
  gg = ggplot(final_toplot, aes(x= final_toplot[,3] ,  y= final_toplot[,4], color = final_toplot[,1]) ) + 
       facet_wrap(~State, scales = "free")+
       geom_point(size=2) +
       theme_bw() +
       scale_color_gradient(low="blue", high="red") +
       theme(legend.position = "left") +
       xlab(names(final_toplot)[3])+
       ylab(names(final_toplot)[4])  + labs(color=names(final_toplot)[1]) 
       return(gg = gg)
  
}

gg<- toplot_scatter_kpca(1, df_final_perstate, "temp_avg_s", "avg_vol_s")
gg


