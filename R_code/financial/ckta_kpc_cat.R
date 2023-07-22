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
library(forecast)
library(feasts)
library(tsibble)
library(tidyr)
library(viridis)
library(ggcorrplot)
library(gplots)
library(combinat)
library(ggstance)

###########
# PALTTES #
###########

drsimonj_colors <- c(
  `red`        = "#d11141",
  `green`      = "#00b159",
  `blue`       = "#00aedb",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c",
  `violet`     = "#EE82EE",
  `cyan`       = "#00FFFF" ,
  `darkblue`   = "#00008b")


drsimonj_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (drsimonj_colors)
  
  drsimonj_colors[cols]
}

drsimonj_cols()

drsimonj_palettes <- list(
  `main`  = drsimonj_cols("lightgreen", "green", "darkgreen"),
  
  `cool`  = drsimonj_cols("blue", "azure"),
  
  `hot`   = drsimonj_cols("yellow", "orange", "red"),
  
  `mixed` = drsimonj_cols("pink", "violet", "purple"),
  
  `grey`  = drsimonj_cols("light grey", "dark grey")
)


drsimonj_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- drsimonj_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

drsimonj_pal("cool")(10)
drsimonj_pal("main")(10)

###############
#  FUNCTIONS # 
###############

new_ckta = function(Kx,Ky){  m = dim(Kx)[1]
one = rep(1,m)
I = diag(m)
first = I - t(t(one))%*%one/m

Kxc = first %*% Kx %*% first 
Kyc = first %*% Ky %*% first

numer<- sum(diag(Kxc %*% Kyc))
denomx<- norm(Kxc, type = "F")
denomy<- norm(Kyc, type = "F")

ckta<- numer/ (denomx * denomy)

return(ckta)

}



calculate_CKTA = function(Kx,Ky){  Kxc = t(t(Kx)-colMeans(Kx))
Kxc = Kxc-rowMeans(Kxc)
Kyc = t(t(Ky)-colMeans(Ky))
Kyc = Kyc-rowMeans(Kyc)

numer <- sum(Kx*Kyc)
denomx <- sum(Kx*Kxc)
denomy <- sum(Ky*Kyc)
return(  numer/sqrt(denomx*denomy) ) 
}




simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}



#############
# DATA LOAD #
#############

#my_gb_data_county_hot<- readRDS("C:\\...\\California\\my_gb_data_county_hot.rds") #NOT AVAILABLE
load("C:\\...\\param_df_final_CA_TX_NY_gb.RData")
load("C:\\...\\kpca_opt_index_CA_TX_NY_gb.RData")



#I do the same thing I did in the ckta var but only for one variable by one
#Compute the jaccard distance between all the data and each of the variable present to observe
#the contribution to the variation within each country

#I could do the same for the numerical variables - discuss with Gareth


#####################
# DATA construction #
#####################

to_remove = which(sapply(my_gb_data_county_hot, dim)[1,]< 2)
my_gb_data_county_hot2 = my_gb_data_county_hot[- to_remove]

sapply(1:length(my_gb_data_county_hot2), function(i) unique(my_gb_data_county_hot2[[i]]$COUNTY_LOCATION_ISSUER))
my_gb_data_county_hot3 = my_gb_data_county_hot2[c(1,13,14,16,19,20,21,22,23,24,25)]
sapply(1:length(my_gb_data_county_hot3), function(i) unique(my_gb_data_county_hot3[[i]]$COUNTY_LOCATION_ISSUER))

county_all = lapply(1:length(my_gb_data_county_hot3), function(i) 
  sapply(strsplit( my_gb_data_county_hot3[[i]]$COUNTY_LOCATION_ISSUER, "," ), "[[" , 1 ))


county_all

#remove Madera from everything
my_gb_data_county_hot4 = my_gb_data_county_hot3[-3]
county_all = county_all[-3]

#remove Sacramento ALso from everything
my_gb_data_county_hot4 = my_gb_data_county_hot4[-4]
county_all = county_all[-4]



county_all



#select California only
kpca_index_new = lapply(1:length(kpca_index), function(i) kpca_index[[i]][c(1,12,14,17:22)])
names(kpca_index_new[[1]])

county_all_kpca = sapply(strsplit(names(kpca_index_new[[1]]), ","), "[[", 1)



kpc1_df = lapply(1:length(kpca_index_new[[1]]), function(i)
  data.frame(kpca1 = as.numeric(unlist(kpca_index_new[[1]][i])),
             County = county_all_kpca[[i]]) )



kpc2_df = lapply(1:length(kpca_index_new[[1]]), function(i)
  data.frame(kpca2 = as.numeric(unlist(kpca_index_new[[2]][i])),
             County = county_all_kpca[[i]]) )


kpc3_df = lapply(1:length(kpca_index_new[[1]]), function(i)
  data.frame(kpca3 = as.numeric(unlist(kpca_index_new[[3]][i])),
             County = county_all_kpca[[i]]) )





##########
#  ckta  #
##########

K_all = lapply(1:length(my_gb_data_county_hot4), function(i)
  1 - distance(my_gb_data_county_hot4[[i]][,c(19:63)], method = "jaccard"))



map_to_cols = list(c(19:22),c(23:34),c(35:48),c(49:51),c(52:63))




K_dist_vars = lapply(1:length(my_gb_data_county_hot4),
                 function(i) lapply(1:length(map_to_cols), 
                                    function(j)
                                      1 - distance(my_gb_data_county_hot4[[i]][,unlist(map_to_cols[[j]])],
                                                   method = "jaccard")))



ckta_1var = sapply(1:5, function(i) mapply(new_ckta, K_all, lapply(K_dist_vars, "[[", i)))

ckta_1var

ckta_1var = ckta_1var[-7,]


name_vars = c("Muny Iss. Type",
              "Issuer Ind.",
              "Muny Source",
              "Muny Off. Type",
              "Risk BB 5y")

colnames(ckta_1var) = name_vars
ckta_1var = as.data.frame(ckta_1var)
ckta_1var$County = sapply(county_all[-7],unique)

melted_ckta_cat_var = melt(ckta_1var)

saveRDS(melted_ckta_cat_var,
        file = "C:\\...\\melted_ckta_cat_var.rds")

ggplot(melted_ckta_cat_var, aes(x =  value, y =variable)) +
  geom_barh(stat = "identity", fill = "steelblue") + 
  facet_grid(County~ .,  scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, hjust = 1),
        axis.text.y = element_text(size = 13, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, vjust = 2.5), 
        strip.text = element_text(face="bold", size=12, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Categorical Variables")+
  xlab("CKTA with original categorical data") 

#this is the contribution of each variable to the entire original covariance matrix

#now we check what the kPCs detect

K_kPc1 = lapply(1:length(kpc1_df), function(i)
  as.matrix(kpc1_df[[i]][,1]) %*% as.matrix(t(kpc1_df[[i]][,1])))

K_kPc2=  lapply(1:length(kpc1_df), function(i)
  as.matrix(cbind(kpc1_df[[i]][,1], 
                  kpc2_df[[i]][,1])) %*% as.matrix(t(cbind(kpc1_df[[i]][,1], 
                                                           kpc2_df[[i]][,1]))))
K_kPc3=  lapply(1:length(kpc1_df), function(i)
  as.matrix(cbind(kpc1_df[[i]][,1], 
                  kpc2_df[[i]][,1],
                  kpc3_df[[i]][,1])) %*% as.matrix(t(cbind(kpc1_df[[i]][,1], 
                                                           kpc2_df[[i]][,1],
                                                           kpc3_df[[i]][,1]))))


ckta_kpc1 = sapply(1:5, function(i) mapply(new_ckta, K_kPc1, lapply(K_dist_vars, "[[", i)))
ckta_kpc2 = sapply(1:5, function(i) mapply(new_ckta, K_kPc2, lapply(K_dist_vars, "[[", i)))
ckta_kpc3 = sapply(1:5, function(i) mapply(new_ckta, K_kPc3, lapply(K_dist_vars, "[[", i)))



ckta_kpc1 = ckta_kpc1[-7,]
ckta_kpc2 = ckta_kpc2[-7,]
ckta_kpc3 = ckta_kpc3[-7,]


colnames(ckta_kpc1) = name_vars
colnames(ckta_kpc2) = name_vars
colnames(ckta_kpc3) = name_vars

ckta_kpc1 = as.data.frame(ckta_kpc1)
ckta_kpc1$County = sapply(county_all[-7],unique)
ckta_kpc1$col = "kPC1"

ckta_kpc1[c(1:3),c(1:5)] = ckta_kpc1[c(1:3),c(1:5)]*3
ckta_kpc1[c(5:8),c(1:5)]= ckta_kpc1[c(5:8),c(1:5)]*3

ckta_kpc2 = as.data.frame(ckta_kpc2)
ckta_kpc2$County = sapply(county_all[-7],unique)
ckta_kpc2$col = "kPC2"

ckta_kpc2[c(1:3),c(1:5)] = ckta_kpc2[c(1:3),c(1:5)]*3
ckta_kpc2[c(5:8),c(1:5)]= ckta_kpc2[c(5:8),c(1:5)]*3


ckta_kpc3 = as.data.frame(ckta_kpc3)
ckta_kpc3$County = sapply(county_all[-7],unique)
ckta_kpc3$col = "kPC3"

ckta_1var$col = "data"

new_melted_var = melt(ckta_1var)
melted_ckta_kpc1 = melt(ckta_kpc1)
melted_ckta_kpc2 = melt(ckta_kpc2)
melted_ckta_kpc3 = melt(ckta_kpc3)


all_melt = rbind(melted_ckta_kpc1,melted_ckta_kpc2)

saveRDS(all_melt,
        file = "C:\\...\\all_melt_kpcs_ckta_cat.rds")


ggplot(all_melt, aes(x =  abs(value), y =variable, fill = col)) +
  geom_barh(stat = "identity", position = "dodge") + 
  scale_fill_manual("", values = c("kPC1" = "green", "kPC2" = "blue")) + 
  facet_grid(County~ .,  scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text=element_text(size=19),
        axis.text.x = element_text(size = 12, hjust = 1),
        axis.text.y = element_text(size = 17, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 17, vjust = 2.5), 
        strip.text = element_text(face="bold", size=14, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Categorical Variables")+
  xlab("CKTAs") 

















