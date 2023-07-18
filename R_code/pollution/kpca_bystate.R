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
library(matrixStats)

#LOAD FEATURES FOR KPCA:
#
# AQI averaged over the stations by state
# CO averaged over the stations by state
# NO2 averaged over the stations by state
# PM 2.5 averaged over the stations by state



load("C:/.../aqi_avg_files.RData")
my_file_state_aqi = my_file_state
rm(my_file_state)

load("C:/.../co_avg_files.RData")
my_file_state_co = my_file_state_1h
rm(my_file_state_1h)


load("C:/.../no2_avg_files.RData")
my_file_state_no2 = my_file_state_1h
rm(my_file_state_1h)


load("C:/.../pm25_avg_files.RData")
my_file_state_pm25 = my_file_state
rm(my_file_state)


############################################
# Select common states across the features #
############################################
# Note they differ given that the non-presence of monitors for some of the features
# in some states

aqi_states<- sapply(1:length(my_file_state_aqi), function(i) unique(my_file_state_aqi[[i]]$`State Name`) )
co_states<- sapply(1:length(my_file_state_co), function(i) unique(my_file_state_co[[i]]$`State Name`) )
no2_states<- sapply(1:length(my_file_state_no2), function(i) unique(my_file_state_no2[[i]]$`State Name`) )
pm25_states<- sapply(1:length(my_file_state_pm25), function(i) unique(my_file_state_pm25[[i]]$`State Name`) )

#final states to select
final_states = intersect(intersect(intersect(pm25_states, no2_states),aqi_states), co_states)


#filtered averaged features according to the common states

filt_aqiavg<- lapply(1:length(final_aqiavg_state), function(i)
         final_aqiavg_state[[i]][which(sapply(final_aqiavg_state[[i]]$State, `[[`, 1) %in% final_states),] )

filt_aqiavg = Filter(function(k) dim(k)[1]>0, filt_aqiavg)


filt_coavg<- lapply(1:length(final_coavg_state), function(i)
  final_coavg_state[[i]][which(sapply(final_coavg_state[[i]]$State, `[[`, 1) %in% final_states),] )

filt_coavg = Filter(function(k) dim(k)[1]>0, filt_coavg)


filt_no2avg<- lapply(1:length(final_no2avg_state), function(i)
  final_no2avg_state[[i]][which(sapply(final_no2avg_state[[i]]$State, `[[`, 1) %in% final_states),] )

filt_no2avg = Filter(function(k) dim(k)[1]>0, filt_no2avg)


filt_pm25avg<- lapply(1:length(final_pm25avg_state), function(i)
  final_pm25avg_state[[i]][which(sapply(final_pm25avg_state[[i]]$State, `[[`, 1) %in% final_states),] )

filt_pm25avg = Filter(function(k) dim(k)[1]>0, filt_pm25avg)


###########################################################################
# The number of days differs across averaged features --> make them equal #  
###########################################################################
#
# Note: in order to make them equal, I compute for each state which database 
# has the maximum number of days and then take it and do a left_join according
# to that. (Checked that the number of maximum days is indeed the one corresponding
# to the one of the "longest" dataframe)
#

sapply(1:30, function(i) dim(filt_aqiavg[[i]])[1])

sapply(1:30, function(i) dim(filt_coavg[[i]])[1])

sapply(1:30, function(i) dim(filt_no2avg[[i]])[1])

sapply(1:30, function(i) dim(filt_pm25avg[[i]])[1])



max_date<- sapply(1:30, function(i)which.max(c(dim(filt_coavg[[i]])[1], 
                                               dim(filt_no2avg[[i]])[1], 
                                               dim(filt_aqiavg[[i]])[1], 
                                               dim(filt_pm25avg[[i]])[1])))



order_join<- lapply(1:30, function(i) c(max_date[i], expand.grid(1:4)$Var1[-max_date[i]]))


for(i in 1:30){
   
  names(filt_coavg[[i]])[2] <- "Date"
  names(filt_no2avg[[i]])[2] <- "Date"
  names(filt_pm25avg[[i]])[2] <- "Date"
  
}


all_list<- list(filt_coavg, filt_no2avg,  filt_aqiavg,  filt_pm25avg)




all_joined  = lapply(1:30, function(i)  plyr::join_all( list( all_list[[ order_join[[i]][1] ]][[i]], 
                                                        all_list[[ order_join[[i]][2] ]][[i]],
                                                        all_list[[ order_join[[i]][3] ]][[i]],
                                                        all_list[[ order_join[[i]][4] ]][[i]]),
                                                   type = "left") )



##############################################################################
# Put a spline through the NAs introduced so to have the same number of days #
##############################################################################


all_joined_spline = lapply(1:30, function(i) 
                            all_joined[[i]] %>% mutate(aqi_avg_new = na.spline(aqi_avg),
                                                       co2_avg_new = na.spline(co2_avg),
                                                       no2_avg_new = na.spline(no2_avg),
                                                       pm_25_avg_new = na.spline(pm_25_avg)))


all_joined_spline_sc = lapply(1:30, function(i) 
                               all_joined_spline[[i]] %>% mutate(aqi_avg_sc = scale(aqi_avg_new),
                                                                 co2_avg_sc = scale(co2_avg_new),
                                                                 no2_avg_sc = scale(no2_avg_new),
                                                                 pm_25_avg_sc = scale(pm_25_avg_new)))

save(all_joined_spline_sc, file = "C:\\Users\\Marta\\Desktop\\Environmental_experiments\\epa_web\\all_join_spline_sc.RData")
##########################
#        kPCA            #
##########################

load("C:\\...\\kpca_files_allstates_poll.RData")


# import python modules
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
#30 kpca one for each state

#NOTE: IF YOU USE FIT INSTEAD OF FIT TRANSFORM --> IT WILL OVERWRITE THE EXISTING 
#LIST AND THE RESULTS WILL ALL BE EQUAL

#NOTE: FIT_TRANSFORM RETURNS PHI(X) AND NOT EIGENVECTORS

X_kpca<- lapply(1:length(alfa_vec), function(h)
             lapply(1:length(gamma_vec), function(i) 
                 lapply(1:length(all_joined_spline_sc), function(j)
                    kpca[[h]][[i]]$fit_transform( as.matrix(all_joined_spline_sc[[j]][,c(11:14)]) )  )) ) 


X_back<-  lapply(1:length(alfa_vec), function(h)
             lapply(1:length(gamma_vec), function(i) 
                lapply(1:length(all_joined_spline_sc),  function(j)
                      kpca[[h]][[i]]$inverse_transform(X_kpca[[h]][[i]][[j]] ) )))



#######################################
# MSE distances - Euclidean distances #   
#######################################


dist_kpca<- lapply(1:length(alfa_vec), function(h)
               lapply(1:length(gamma_vec), function(i) 
                  lapply(1:length(all_joined_spline_sc),  function(j)
                     sapply(1:nrow(X_back[[h]][[i]][[j]]), function(k) 
                             dist( rbind( X_back[[h]][[i]][[j]][k,],
                                as.matrix( all_joined_spline_sc[[j]][k,c(11:14)]) ) ) ))))




save(X_kpca,X_back, dist_kpca, file = "C:\\...\\kpca_files_allstates_poll.RData" )



#################
#final distances#
#################

d_final =lapply(1:length(alfa_vec), function(h)
            lapply(1:length(gamma_vec), function(i) 
                 sapply(1:length(all_joined_spline_sc), function(j)
                     sum(dist_kpca[[h]][[i]][[j]])/nrow(X_back[[h]][[i]][[j]])   )))

names(d_final)<- alfa_vec


l_d_final<- lapply(1:length(alfa_vec), function(i) data.frame(do.call(rbind, d_final[[i]])) )

name_states<- sapply(1:length(all_joined_spline_sc),  function(i) unique(all_joined_spline_sc[[i]]$State))

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

save(param_df_final, file = "C:\\...\\param_df_final.RData")

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


save(kpca_index, kpca_optimal_fun, file = "C:\\...\\kpca_index_&_optfun.RData")

########################################
# We know evaluate a new set of points #
########################################

range_col<- lapply(1:length(all_joined_spline_sc), function(i)
  colRanges(as.matrix(all_joined_spline_sc[[i]][,c(11:14)])))

features<- colnames(all_joined_spline_sc[[1]][,c(11:14)])

for(i in 1:length(all_joined_spline_sc)){ row.names(range_col[[i]])<- features }
for(i in 1:length(all_joined_spline_sc)){ colnames(range_col[[i]])<- c("min", "max") }


list_ranges_min = t(sapply(range_col, function(x) x[,1]))
list_ranges_max = t(sapply(range_col, function(x) x[,2]))

min_new_mesh = matrix(colMaxs(list_ranges_min), 1, 4)
max_new_mesh= matrix(colMins(list_ranges_max), 1, 4)

colnames(min_new_mesh)<- features
colnames(max_new_mesh)<- features


############
# New Mesh #
############


N <- 100
N <- 1000

new_meshes<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))

final_meshes<- do.call(cbind, new_meshes)
colnames(final_meshes) <- features


##########################
#  Evaluate optimal kpca #
##########################

eigen_dec<- readRDS("C:\\...\\eigen_dec_kpca_opt_allstates.rds") 
source_python('C:\\Users\\...\\stepwise_kpca.py')

#I now compute the eigenvalues and the eigenvectors of the optimals kpca evaluated at the points
#of the original all_joined_spline_sc

eigen_dec <- lapply(1:length(kpca_optimal_fun), function(i)
                      stepwise_kpca(X = as.matrix(all_joined_spline_sc[[i]][,c(11:14)]),
                      gamma = kpca_optimal_fun[[i]]$gamma,
                      n_components = int(3))  )

saveRDS(eigen_dec, file = "C:\\...\\eigen_dec_kpca_opt_allstates.rds") 


################################################################
# We now construct a projection of the new mesh by using these #
#sets of eigenvalues and eigenfunctions to check results above #
################################################################

#new_X_kpca<- readRDS("C:\\...\\new_X_kpca_allstates.rds")

source_python('C:\\...\\project_x.py')

lambdas = lapply(1:length(eigen_dec), function(i) do.call(cbind, eigen_dec[[i]][[2]]))


new_X_kpca =   lapply(1:length(all_joined_spline_sc), function(i) sapply(1:N, function(j)   
                                      project_x(x_new = final_meshes[j,],
                                                X= as.matrix(all_joined_spline_sc[[i]][, c(11:14)]),
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
for(i in 1:length(eigen_val_scaled_2)){eigen_val_scaled_2[[i]]$State<- rep(unique(all_joined_spline_sc[[i]]$State),
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
        axis.ticks.x=element_blank()) + scale_color_discrete(name="Eigenvectors") + #FIX HERE
  ylab( expression(alpha*lambda))




###############################
#   Plot of the new Kpca      #     
###############################


to_plot_kpca<- lapply(1:length(new_X_kpca), function(i) t(new_X_kpca[[i]]) )

to_plot_kpca<- lapply(1:length(new_X_kpca), function(i) as.data.frame(to_plot_kpca[[i]]) )

feat_to_plot_kpca<- c(expression(phi_1), expression(phi_2), expression(phi_3))

for(i in 1:length(to_plot_kpca)){ colnames(to_plot_kpca[[i]]) <- feat_to_plot_kpca }

for(i in 1:length(to_plot_kpca)){to_plot_kpca[[i]]$State<- rep(unique(all_joined_spline_sc[[i]]$State),
                                                               dim(to_plot_kpca[[i]])[1])}

to_plot_kpca<- lapply(1:length(to_plot_kpca), function(i) reshape2::melt(to_plot_kpca[[i]]) )

for(i in 1:length(to_plot_kpca)){ to_plot_kpca[[i]]$x = rep(1:(dim(to_plot_kpca[[i]])[1]/3), 3) }


to_plot_kpca<-  do.call(rbind, to_plot_kpca)
to_plot_kpca$ind_var = ifelse(to_plot_kpca$variable == "phi_1", 1, 
                              ifelse(to_plot_kpca$variable ==  "phi_2", 2,3))


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


ggplot(df_toplot, aes(x = as.Date(x) ,  y= value, color = (variable) )) +
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
  df_toplot_filtered<- df_toplot[,c("State",var1,var2)]       
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

gg<- toplot_scatter_kpca(1, all_joined_spline_sc, "aqi_avg_sc", "pm_25_avg_sc")
gg


