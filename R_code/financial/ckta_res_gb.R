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

#my_gb_data_county_hot<- readRDS("C:\\...\\my_gb_data_county_hot.rds") #not available
load("C:/...\\pcs_gbs.RData")
load("C:\\...\\param_df_final_CA_TX_NY_gb.RData")
load("C:\\...\\kpca_opt_index_CA_TX_NY_gb.RData")



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

names(pcs)

#remove Madera from everything
pcs_new = pcs[-3]
my_gb_data_county_hot4 = my_gb_data_county_hot3[-3]
county_all = county_all[-3]

#remove Sacramento ALso from everything
pcs_new = pcs_new[-4]
my_gb_data_county_hot4 = my_gb_data_county_hot4[-4]
county_all = county_all[-4]


pc1_df = lapply(1:length(my_gb_data_county_hot4), function(i)
  data.frame(pca1 = pcs_new[[i]][,1],
             County = county_all[[i]])  )

pc2_df = lapply(1:length(my_gb_data_county_hot4), function(i)
  data.frame(pca2 = pcs_new[[i]][,2],
             County = county_all[[i]])  )




param_df_final$County
names(kpca_index[[1]])

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



################
# COMPUTE CKTA #
################ 


head(my_gb_data_county_hot4[[1]])
head(pc1_df[[1]])
head(kpc1_df[[1]])

#check
sapply(1:length(pc1_df), function(i) unique(pc1_df[[i]]$County)  )
sapply(1:length(kpc1_df), function(i) unique(kpc1_df[[i]]$County)  )
sapply(1:length(my_gb_data_county_hot4), function(i) unique(my_gb_data_county_hot4[[i]]$COUNTY_LOCATION_ISSUER)  )


K_Kraw = lapply(1:length(my_gb_data_county_hot4), function(i)
           as.matrix(my_gb_data_county_hot4[[i]][,c(4:13,19:63)]) %*% as.matrix(t(my_gb_data_county_hot4[[i]][,c(4:13,19:63)])))


K_Pc1 = lapply(1:length(pc1_df), function(i)
              as.matrix(pc1_df[[i]][,1]) %*% as.matrix(t(pc1_df[[i]][,1])))

K_Pc2=  lapply(1:length(pc1_df), function(i)
              as.matrix(cbind(pc1_df[[i]][,1], 
                              pc2_df[[i]][,1])) %*% as.matrix(t(cbind(pc1_df[[i]][,1], 
                                                                      pc2_df[[i]][,1]))))
K_Pc3=  lapply(1:length(pc1_df), function(i)
  as.matrix(cbind(pc1_df[[i]][,1], 
                  pc2_df[[i]][,1])) %*% as.matrix(t(cbind(pc1_df[[i]][,1], 
                                                          pc2_df[[i]][,1],
                                                          pc3_df[[i]][,1]))))
K_kPc1 = lapply(1:length(pc1_df), function(i)
              as.matrix(kpc1_df[[i]][,1]) %*% as.matrix(t(kpc1_df[[i]][,1])))

K_kPc2=  lapply(1:length(pc1_df), function(i)
              as.matrix(cbind(kpc1_df[[i]][,1], 
                              kpc2_df[[i]][,1])) %*% as.matrix(t(cbind(kpc1_df[[i]][,1], 
                                                                      kpc2_df[[i]][,1]))))


ckta_pc1 = lapply(1:length(pc1_df), function(i) new_ckta(K_Kraw[[i]],
                                                       K_Pc1[[i]]))


save(ckta_pc1, file = "C:\\..\\ckta_pc1.RData")

ckta_pc2 = lapply(1:length(pc1_df), function(i) new_ckta(K_Kraw[[i]],
                                                    K_Pc2[[i]]))

save(ckta_pc2, file = "C:\\Users\\..\\ckta_pc2.RData")


ckta_pc3 = lapply(1:length(pc1_df), function(i) new_ckta(K_Kraw[[i]],
                                                         K_Pc3[[i]]))

save(ckta_pc2, file = "C:\\Users\\..\\ckta_pc3.RData")



ckta_kpc1 = lapply(1:length(pc1_df), function(i) new_ckta(K_Kraw[[i]], K_kPc1[[i]]))
save(ckta_kpc1, file = "C:\\..\ckta_kpc1.RData")


ckta_kpc2 = lapply(1:length(pc1_df), function(i) new_ckta(K_Kraw[[i]], K_kPc2[[i]]))
save(ckta_kpc2, file = "C:\\..\\ckta_kpc2.RData")


ckta_kpc3 = lapply(1:length(pc1_df), function(i)new_ckta(K_Kraw[[i]],  K_kPc3[[i]]))
save(ckta_kpc3, file = "C:\\...\\ckta_kpc3.RData")




res = data.frame( "pc1" = round(do.call(rbind,ckta_pc1), digits = 3),
            "pc1-pc2" =round(do.call(rbind,ckta_pc2), digits = 3),
          "pc1-pc2-pc3" =round(do.call(rbind,ckta_pc3), digits = 3),
            "kpc1" =round(do.call(rbind,ckta_kpc1), digits = 3),
            "kpc1-kpc2" =round(do.call(rbind,ckta_kpc2), digits = 3),
            "kpc1-kpc2-kpc3" =round(do.call(rbind,ckta_kpc3), digits = 3) )

res

rownames(res) = sapply(1:length(pc1_df), function(i) unique(pc1_df[[i]]$County)  )

kable(res, format = "latex")









