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
library(useful)
library(matrixStats)
library(readxl)
library(gplots) 
library(CCA)
library(GGally)
library(kernlab)

###############################################
###############################################
#    GB Bonds, Pollution and Climate DATA     # 
###############################################
###############################################


load("C:/...\\all_join_spline_sc_cali.RData")

poll_counties = sapply(1:length(all_joined_spline_sc), function(i) unique(all_joined_spline_sc[[i]]$County))
poll_counties

load("C:/...\\gb_data_final_CA_TX_NY.RData")
issuer_names_my_county_final_cali = issuer_names_my_county_final[c(1,12,14,17,18,19,20,21,22)]
my_gb_data_county_final_cali = my_gb_data_county_final_3[c(1,12,14,17,18,19,20,21,22)]

iss_names_cali = sapply(strsplit(issuer_names_my_county_final_cali, ","), "[", 1)
iss_names_cali


df_final_weather<- readRDS("C:/...\\df_final_percounty.rds")

weath_counties = sapply(1:length(df_final_weather), function(i) unique(df_final_weather[[i]]$COUNTY))

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

weath_counties<-  sapply(1:length(weath_counties), function(i) simpleCap(weath_counties[i]))


###############################################
# Select common counties across the databases #
###############################################

final_counties_old = intersect(intersect(poll_counties, iss_names_cali), weath_counties)


county_gg<- map_data("county")
california <- subset(county_gg, region %in% c("california"))

california$subregion_cap = sapply(1:dim(california)[1], function(i) simpleCap(california[i,]$subregion))

final_counties_pollweath = intersect(poll_counties, weath_counties)

lat_long_weathpoll_county = lapply(1:length(final_counties_pollweath), function(i)
                                california %>% filter(subregion_cap == final_counties_pollweath[i]) )

lat_long_weathpoll_county2 = lapply(1:length(final_counties_pollweath), function(i)
                                         lat_long_weathpoll_county[[i]] %>% 
                                              group_by(region, subregion, subregion_cap,)%>%
                                                 summarize(lat_mean = mean(lat), long_mean = mean(long)) )

lat_lon_county_pollweath_final =  do.call(rbind, lat_long_weathpoll_county2)



lat_long_gb_county = lapply(1:length(iss_names_cali), function(i)
                               california %>% filter(subregion_cap == iss_names_cali[i]) )

lat_long_gb_county2 = lapply(1:length(iss_names_cali), function(i)
                                        lat_long_gb_county[[i]] %>% 
                                         group_by(region, subregion, subregion_cap,)%>%
                                            summarize(lat_mean = mean(lat), long_mean = mean(long)) )


lat_lon_county_gb_final =  do.call(rbind, lat_long_gb_county2)


dist_counties_gb_weathpoll =  do.call(rbind,  lapply(1:dim(lat_lon_county_pollweath_final)[1], function(j) 
                                      sapply(1:dim(lat_lon_county_gb_final)[1], function(i)
                                         distVincentyEllipsoid(rev(lat_lon_county_pollweath_final[j,c(4,5)]),
                                                               rev(lat_lon_county_gb_final[i,c(4,5)]))/1000 )))



#threshold is 111 - we can lower this if we want

dist_counties_gb_weathpoll2 = ifelse(dist_counties_gb_weathpoll < 111 ,1, 0)

##############################################################################
# I consider 2 final matches - one with repeated entrances and one with none #
##############################################################################


final_match = do.call(rbind, sapply(1:dim(lat_lon_county_gb_final)[1], function(i)
                     cbind(rep(iss_names_cali[i], sum(dist_counties_gb_weathpoll2[,i] == 1) ),
                     final_counties_pollweath[as.logical(dist_counties_gb_weathpoll2[,i])])) )


final_match2 = final_match[c(1,8,14,16,21,27,31,36,38),]


#filtering weather and pollution according to the same counties for consistency

filt_poll_data = lapply(1:length(all_joined_spline_sc), function(i)
       all_joined_spline_sc[[i]][which(sapply(all_joined_spline_sc[[i]]$County, `[[`, 1) %in% final_counties_pollweath),] )

filt_poll_data = Filter(function(k) dim(k)[1]>0, filt_poll_data)


df_final_weather_2 = list()
for(i in 1:length(df_final_weather)){df_final_weather_2[[i]] = as.data.frame(df_final_weather[[i]]) }
for(i in 1:length(df_final_weather_2)){df_final_weather_2[[i]]$County_2 <- weath_counties[i]  }


filt_weath_data = lapply(1:length(df_final_weather_2), function(i)
  df_final_weather_2[[i]][which(sapply(df_final_weather_2[[i]]$County_2, `[[`, 1) %in% final_counties_pollweath),] )

filt_weath_data = Filter(function(k) dim(k)[1]>0, filt_weath_data)

#check:
sapply(1:length(final_counties_pollweath), function(i) unique(filt_poll_data[[i]]$County))
sapply(1:length(final_counties_pollweath), function(i) unique(filt_weath_data[[i]]$County_2))


# the following is just done for data format coming from the previous version - in practice I do not need
# it since I do not need to filter these

my_gb_data_county_final_cali_2 = list()
for(i in 1:length(my_gb_data_county_final_cali)){my_gb_data_county_final_cali_2[[i]] = as.data.frame(my_gb_data_county_final_cali[[i]]) }
for(i in 1:length(my_gb_data_county_final_cali_2)){my_gb_data_county_final_cali_2[[i]]$County <- iss_names_cali[i]  }

filt_gb_data = lapply(1:length(my_gb_data_county_final_cali_2), function(i)
  my_gb_data_county_final_cali_2[[i]][which(sapply(my_gb_data_county_final_cali_2[[i]]$County, `[[`, 1) %in% final_counties_pollweath),] )

filt_gb_data = Filter(function(k) dim(k)[1]>0, filt_gb_data)


sapply(1:length(filt_gb_data), function(i) unique(filt_gb_data[[i]]$County))




###############################################
###############################################
## 2)           CCA on KPCs                 ###
###############################################
###############################################


#new_X_kpca_gb = readRDS("C:/.../new_X_kpca_CA_TX_NY_100.rds")
new_X_kpca_gb = readRDS("C:/.../new_X_kpca_CA_TX_NY_1000.rds")

new_X_kpca_cali_gb = new_X_kpca_gb[c(1,12,14,17,18,19,20,21,22)]
#new_X_kpca_ny_gb = new_X_kpca[c(2,7,11,15,16)]
#new_X_kpca_tx_gb = new_X_kpca[c(3,4,5,6,8,9,10,13,23,24)]


#new_X_kpca_cali_poll = readRDS("C:\\...\\new_X_kpca_cali_100.rds")
new_X_kpca_cali_poll = readRDS("C:\\...\\new_X_kpca_cali_1000.rds")


#new_X_kpca_cali_weath = readRDS("C:\\...\\new_X_kpca_cali_100.rds")
new_X_kpca_cali_weath = readRDS("C:\\...\\new_X_kpca_cali_1000.rds")


###############################################
# Select common counties across the databases #
###############################################


#################
# final_match_2 # 
#################

final_match2
final_match2[,1] # --> for the green finance KPCs
final_match2[,2] # --> for the weath/poll KPCs

which(sapply(poll_counties, `[[`, 1) %in% final_match2[,2])

which(sapply(iss_names_cali, `[[`, 1) %in% final_match2[,1])

which(sapply(weath_counties, `[[`, 1) %in% final_match2[,2])


filt_new_kpca_gb = new_X_kpca_cali_gb[ c(which(sapply(iss_names_cali, `[[`, 1) %in% final_match2[,1])) ] 

filt_new_kpca_poll =new_X_kpca_cali_poll[ c(which(sapply(poll_counties, `[[`, 1) %in% final_match2[,2])) ] 

filt_new_kpca_weath =new_X_kpca_cali_weath[ c(which(sapply(weath_counties, `[[`, 1) %in% final_match2[,2])) ] 


filt_new_kpca_gb_df <- lapply(1:length(filt_new_kpca_gb), function(i) as.data.frame( t(filt_new_kpca_gb[[i]])))
filt_new_kpca_poll_df <- lapply(1:length(filt_new_kpca_poll), function(i) as.data.frame( t(filt_new_kpca_poll[[i]])))
filt_new_kpca_weath_df <- lapply(1:length(filt_new_kpca_weath), function(i) as.data.frame( t(filt_new_kpca_weath[[i]])))


#############
#    CCA    #
#############


GB_mat_cca = do.call(Map, c(f = cbind, filt_new_kpca_gb_df))

for(i in 1:length(GB_mat_cca)){colnames(GB_mat_cca[[i]]) <- final_match2[,1]}

Poll_mat_cca = do.call(Map, c(f = cbind, filt_new_kpca_poll_df))

for(i in 1:length(Poll_mat_cca)){colnames(Poll_mat_cca[[i]]) <- final_match2[,2]}

Weath_mat_cca = do.call(Map, c(f = cbind, filt_new_kpca_weath_df))

for(i in 1:length(Weath_mat_cca)){colnames(Weath_mat_cca[[i]]) <- final_match2[,2]}



##################################
#  RESULTS  USING YACCA LIBRARY  #
##################################

library(yacca)


cca_gb_weath = lapply(1:(length(GB_mat_cca)-1), function(i)
                     cca(as.matrix(GB_mat_cca[[i]]), as.matrix(Weath_mat_cca[[i]])))

cca_gb_poll = lapply(1:(length(GB_mat_cca)-1), function(i)
                      cca(as.matrix(GB_mat_cca[[i]]), as.matrix(Poll_mat_cca[[i]])))


save(cca_gb_weath, cca_gb_poll, file= "C:\\...\\cca_gbweath_gbpoll.RData")


####################
# GB - WEATH PLOTS #
####################

######
#KPC1#
######

attach(mtcars)
par(mfrow=c(3,2), mar = c(5, 0.5, 1, 0.5)) 

helio.plot(cca_gb_weath[[1]], 
           x.name="GB KPC 1", 
           y.name="Weather KPC 1", cv = 1, main = "") 

helio.plot(cca_gb_weath[[1]], 
           x.name="GB KPC 1", 
           y.name="Weather KPC 1", cv = 2,  main = "") 

helio.plot(cca_gb_weath[[1]], 
           x.name="GB KPC 1", 
           y.name="Weather KPC 1", cv = 3, main = "") 

helio.plot(cca_gb_weath[[1]], 
           x.name="GB KPC 1", 
           y.name="Weather KPC 1", cv = 4, main = "") 

helio.plot(cca_gb_weath[[1]], 
           x.name="GB KPC 1", 
           y.name="Weather KPC 1", cv = 5, main = "") 



########
# KPC2 #
########

attach(mtcars)
par(mfrow=c(3,2))

helio.plot(cca_gb_weath[[2]], 
           x.name="GB KPC 2", 
           y.name="Weather KPC 2",cv = 1)

helio.plot(cca_gb_weath[[2]], 
           x.name="GB KPC 2", 
           y.name="Weather KPC 2",cv = 2)

helio.plot(cca_gb_weath[[2]], 
           x.name="GB KPC 2", 
           y.name="Weather KPC 2",cv = 3)

helio.plot(cca_gb_weath[[2]], 
           x.name="GB KPC 2", 
           y.name="Weather KPC 2",cv = 4)

helio.plot(cca_gb_weath[[2]], 
           x.name="GB KPC 2", 
           y.name="Weather KPC 2",cv = 5)








###################
# GB - POLL PLOTS #
###################

######
#KPC1#
######

attach(mtcars)
par(mfrow=c(3,2))

helio.plot(cca_gb_poll[[1]], 
           x.name="GB KPC 1", 
           y.name="Pollution KPC 1", cv = 1) 

helio.plot(cca_gb_poll[[1]], 
           x.name="GB KPC 1", 
           y.name="Pollution KPC 1", cv = 2) 

helio.plot(cca_gb_poll[[1]], 
           x.name="GB KPC 1", 
           y.name="Pollution KPC 1", cv = 3) 

helio.plot(cca_gb_poll[[1]], 
           x.name="GB KPC 1", 
           y.name="Pollution KPC 1", cv = 4) 

helio.plot(cca_gb_poll[[1]], 
           x.name="GB KPC 1", 
           y.name="Pollution KPC 1", cv = 5) 





######
#KPC2#
######

attach(mtcars)
par(mfrow=c(3,2))

helio.plot(cca_gb_poll[[2]], 
           x.name="GB KPC 2", 
           y.name="Pollution KPC 2", cv = 1) 

helio.plot(cca_gb_poll[[2]], 
           x.name="GB KPC 2", 
           y.name="Pollution KPC 2", cv = 2) 

helio.plot(cca_gb_poll[[2]], 
           x.name="GB KPC 2", 
           y.name="Pollution KPC 2", cv = 3) 

helio.plot(cca_gb_poll[[2]], 
           x.name="GB KPC 2", 
           y.name="Pollution KPC 2", cv = 4) 

helio.plot(cca_gb_poll[[2]], 
           x.name="GB KPC 2", 
           y.name="Pollution KPC 2", cv = 5) 






################################
#  RESULTS  USING CCA LIBRARY  #
################################

img.matcor(matcor(Weath_mat_cca[[1]], GB_mat_cca[[1]]), type = 2)
img.matcor(matcor(Weath_mat_cca[[2]], GB_mat_cca[[2]]), type = 2)

img.matcor(matcor(Poll_mat_cca[[1]], GB_mat_cca[[1]]), type = 2)
img.matcor(matcor(Poll_mat_cca[[2]], GB_mat_cca[[2]]), type = 2)

cca_res_gb_weat = lapply(1:2, function(i) cc(GB_mat_cca[[i]], Weath_mat_cca[[i]])  )
cca_res_gb_poll = lapply(1:2, function(i) cc(GB_mat_cca[[i]], Poll_mat_cca[[i]])  )



kable(rbind(cbind(cca_gb_poll[[1]]$xstructcorr[,c(1:2)],cca_gb_poll[[1]]$xstructcorrsq[,c(1:2)]),
            cbind(cca_gb_poll[[1]]$ystructcorr[,c(1:2)],cca_gb_poll[[1]]$ystructcorrsq[,c(1:2)])), format = 'latex',
      digits = 3)



kable(rbind(cbind(cca_gb_poll[[2]]$xstructcorr[,c(1:2)],cca_gb_poll[[2]]$xstructcorrsq[,c(1:2)]),
            cbind(cca_gb_poll[[2]]$ystructcorr[,c(1:2)],cca_gb_poll[[2]]$ystructcorrsq[,c(1:2)])), format = 'latex',
      digits = 3)


kable(rbind(cbind(cca_gb_weath[[1]]$xstructcorr[,c(1:2)],cca_gb_weath[[1]]$xstructcorrsq[,c(1:2)]),
            cbind(cca_gb_weath[[1]]$ystructcorr[,c(1:2)],cca_gb_weath[[1]]$ystructcorrsq[,c(1:2)])), format = 'latex',
      digits = 3)



kable(rbind(cbind(cca_gb_weath[[2]]$xstructcorr[,c(1:2)],cca_gb_weath[[2]]$xstructcorrsq[,c(1:2)]),
            cbind(cca_gb_weath[[2]]$ystructcorr[,c(1:2)],cca_gb_weath[[2]]$ystructcorrsq[,c(1:2)])), format = 'latex',
      digits = 3)


heatmap.2(matcor(Poll_mat_cca[[1]], GB_mat_cca[[1]])$XYcor[c(10:18),c(1:9)],
          dendrogram = "none",
          #Rowv=FALSE, Colv = FALSE,
          trace='none',
          col =  rev(hcl.colors(20*10, "viridis", rev = TRUE)),
          density.info='histogram', 
          denscol="black",
          margins=c(12,5),
          key.par=list(mar=c(3.5,0,3,3)),
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 5), lwid=c(1, 10, 1),
          cexRow = 1.1,
          cexCol = 1.1
          )
heatmap.2(matcor(Poll_mat_cca[[2]], GB_mat_cca[[2]])$XYcor)
heatmap.2(matcor(Weath_mat_cca[[1]], GB_mat_cca[[1]])$XYcor)
heatmap.2(matcor(Weath_mat_cca[[2]], GB_mat_cca[[2]])$XYcor)


colnames(Poll_mat_cca[[1]]) = colnames(GB_mat_cca[[1]])
colnames(Poll_mat_cca[[2]]) = colnames(GB_mat_cca[[1]])
colnames(Weath_mat_cca[[1]]) = colnames(GB_mat_cca[[1]])
colnames(Weath_mat_cca[[2]]) = colnames(GB_mat_cca[[1]])


heatmap.2(matcor(Poll_mat_cca[[1]], GB_mat_cca[[1]])$XYcor, 
  dendrogram = "none",
  col =  rev(hcl.colors(20*10, "viridis", rev = TRUE)), 
  Rowv=F, 
  Colv = F,
  margins=c(12,5), # ("margin.Y", "margin.X")
  trace='none', 
  symkey=FALSE, 
  symbreaks=FALSE, 
  density.info='histogram', 
  denscol="black",
  keysize=1, 
  #( "bottom.margin", "left.margin", "top.margin", "left.margin" )
  key.par=list(mar=c(3.5,0,3,3)),
  # lmat -- added 2 lattice sections (5 and 6) for padding
  lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
  lhei=c(2.5, 5), lwid=c(1, 10, 1),
  cexRow = 1.2, cexCol = 1.2
  )



heatmap.2(matcor(Poll_mat_cca[[2]], GB_mat_cca[[2]])$XYcor, 
          dendrogram = "none",
          col =  rev(hcl.colors(20*10, "viridis", rev = TRUE)), 
          Rowv=F, Colv = F,
          margins=c(12,5), # ("margin.Y", "margin.X")
          trace='none', 
          symkey=FALSE, 
          symbreaks=FALSE, 
          density.info='histogram', 
          denscol="black",
          keysize=1, 
          #( "bottom.margin", "left.margin", "top.margin", "left.margin" )
          key.par=list(mar=c(3.5,0,3,3)),
          # lmat -- added 2 lattice sections (5 and 6) for padding
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 5), lwid=c(1, 10, 1),
          cexRow = 1.2, cexCol = 1.2
          )



heatmap.2(matcor(Weath_mat_cca[[1]], GB_mat_cca[[1]])$XYcor, 
          dendrogram = "none",
          col =  rev(hcl.colors(20*10, "blues", rev = TRUE)), 
          Rowv=F, Colv = F, 
          margins=c(12,5), # ("margin.Y", "margin.X")
          trace='none', 
          symkey=FALSE, 
          symbreaks=FALSE, 
          density.info='histogram', 
          denscol="black",
          keysize=1, 
          #( "bottom.margin", "left.margin", "top.margin", "left.margin" )
          key.par=list(mar=c(3.5,0,3,3)),
          # lmat -- added 2 lattice sections (5 and 6) for padding
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 5), lwid=c(1, 10, 1),
          cexRow = 1.2, cexCol = 1.2
          )


heatmap.2(matcor(Weath_mat_cca[[2]], GB_mat_cca[[2]])$XYcor, 
          dendrogram = "none",
          col =  rev(hcl.colors(20*10, "blues", rev = TRUE)), 
          Rowv=F, Colv = F,
          margins=c(12,5), # ("margin.Y", "margin.X")
          trace='none', 
          symkey=FALSE, 
          symbreaks=FALSE, 
          density.info='histogram', 
          denscol="black",
          keysize=1, 
          #( "bottom.margin", "left.margin", "top.margin", "left.margin" )
          key.par=list(mar=c(3.5,0,3,3)),
          # lmat -- added 2 lattice sections (5 and 6) for padding
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 5), lwid=c(1, 10, 1),
          cexRow = 1.2, cexCol = 1.2
          )




###############################
# the canonical correlations  #
###############################


attach(mtcars)
par(mfrow=c(1,2), mar = c(5, 2.5, 2, 1.5)) 

barplot(cca_res_gb_weat[[1]]$cor, xlab = "Dimension", ylab = "Canonical correlations",
        main = "GB and Weather data with KPC1", names.arg = 1:9, ylim = c(0,1) )
barplot(cca_res_gb_weat[[2]]$cor, xlab = "Dimension", ylab = "Canonical correlations",
        main = "GB and Weather data with KPC2", names.arg = 1:9, ylim = c(0,1))
# barplot(cca_res_gb_weat[[3]]$cor, xlab = "Dimension", ylab = "Canonical correlations",
#         main = "GB and Weather data with KPC3", names.arg = 1:9, ylim = c(0,1))



attach(mtcars)
par(mfrow=c(1,2), mar = c(5, 2.5, 2, 1.5)) 

barplot(cca_res_gb_poll[[1]]$cor, xlab = "Dimension", ylab = "Canonical correlations",
        main = "GB and Pollution data with KPC1", names.arg = 1:9, ylim = c(0,1))
barplot(cca_res_gb_poll[[2]]$cor, xlab = "Dimension", ylab = "Canonical correlations",
        main = "GB and Pollution data with KPC2", names.arg = 1:9, ylim = c(0,1))
# barplot(cca_res_gb_poll[[3]]$cor, xlab = "Dimension", ylab = "Canonical correlations",
#         main = "GB and Pollution data with KPC3", names.arg = 1:9, ylim = c(0,1))



##############################
# raw canonical coefficients #
##############################

cca_res_gb_weat[[1]][3:4]
cca_res_gb_weat[[2]][3:4]
# cca_res_gb_weat[[3]][3:4]


cca_res_gb_poll[[1]][3:4]
cca_res_gb_poll[[2]][3:4]
# cca_res_gb_poll[[3]][3:4]


##############################
#     CANONICAL LOADINGS     #
##############################

can_load_gb_weat <- lapply(1:2, function(i) comput(GB_mat_cca[[i]], Weath_mat_cca[[i]], cca_res_gb_weat[[i]])) 

can_load_gb_poll <- lapply(1:2, function(i) comput(GB_mat_cca[[i]], Poll_mat_cca[[i]], cca_res_gb_poll[[i]])) 


can_load_gb_weat[[1]][3:6]
can_load_gb_weat[[2]][3:6]
# can_load_gb_weat[[3]][3:6]


can_load_gb_poll[[1]][3:6]
can_load_gb_poll[[2]][3:6]
# can_load_gb_poll[[3]][3:6]



#################
#     TESTS     #
#################

#Require this library
library(CCP)

# tests of canonical dimensions
rho_gb_weat <- lapply(1:2, function(i) cca_res_gb_weat[[i]]$cor)
rho_gb_poll <- lapply(1:2, function(i) cca_res_gb_poll[[i]]$cor)


## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(GB_mat_cca[[1]])[1]
p <- dim(GB_mat_cca[[1]])[2]
q <- dim(Weath_mat_cca[[1]])[2]

## Calculate p-values using the F-approximations of different test statistics:

wilks_gb_weat<- lapply(1:2, function(i) p.asym(rho_gb_weat[[i]], n, p, q, tstat = "Wilks") )

wilks_gb_poll<- lapply(1:2, function(i) p.asym(rho_gb_poll[[i]], n, p, q, tstat = "Wilks") )


hotelling_gb_weat<- lapply(1:2, function(i) p.asym(rho_gb_weat[[i]], n, p, q, tstat = "Hotelling") )

hotelling_gb_poll<- lapply(1:2, function(i) p.asym(rho_gb_poll[[i]], n, p, q, tstat = "Hotelling") )


pillai_gb_weat<- lapply(1:2, function(i) p.asym(rho_gb_weat[[i]], n, p, q, tstat = "Pillai") )

pilla_gb_poll<- lapply(1:2, function(i) p.asym(rho_gb_poll[[i]], n, p, q, tstat = "Pillai") )


roy_gb_weat<- lapply(1:2, function(i) p.asym(rho_gb_weat[[i]], n, p, q, tstat = "Roy") )

roy_gb_poll<- lapply(1:2, function(i) p.asym(rho_gb_poll[[i]], n, p, q, tstat = "Roy") )



#standardized GB_mat canonical coefficients diagonal matrix of GM_mat sd's
s_gb_mat  <-lapply( 1:2, function(i) diag(sqrt(diag(cov(GB_mat_cca[[i]])))))

#weather
s_gb_mat[[1]] %*% cca_res_gb_weat[[1]]$xcoef
s_gb_mat[[2]] %*% cca_res_gb_weat[[2]]$xcoef
# s_gb_mat[[3]] %*% cca_res_gb_weat[[3]]$xcoef


#pollution
s_gb_mat[[1]] %*% cca_res_gb_poll[[1]]$xcoef
s_gb_mat[[2]] %*% cca_res_gb_poll[[2]]$xcoef
# s_gb_mat[[3]] %*% cca_res_gb_poll[[3]]$xcoef


#standardized Weat_mat canonical coefficients diagonal matrix of Weat_mat sd's
s_weat_mat  <-lapply( 1:2, function(i) diag(sqrt(diag(cov(Weath_mat_cca[[i]])))))

#GB_data
s_weat_mat[[1]] %*% cca_res_gb_weat[[1]]$xcoef
s_weat_mat[[2]] %*% cca_res_gb_weat[[2]]$xcoef
# s_weat_mat[[3]] %*% cca_res_gb_weat[[3]]$xcoef


#standardized Poll_mat canonical coefficients diagonal matrix of Poll_mat sd's
s_poll_mat  <-lapply( 1:2, function(i) diag(sqrt(diag(cov(Poll_mat_cca[[i]])))))

#GB_data
s_poll_mat[[1]] %*% cca_res_gb_poll[[1]]$xcoef
s_poll_mat[[2]] %*% cca_res_gb_poll[[2]]$xcoef
# s_poll_mat[[3]] %*% cca_res_gb_poll[[3]]$xcoef




