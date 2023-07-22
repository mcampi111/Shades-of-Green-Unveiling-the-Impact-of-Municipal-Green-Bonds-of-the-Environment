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

#my_gb_data_county_hot<- readRDS("C:\\...\\my_gb_data_county_hot.rds") #NOT AVAILABLE



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


#########################################
# ordering ckta per variable per county #
#########################################


n_var = c(1,2,3,4,5)

comb1 = combn(n_var,1)
comb2 = combn(n_var,2)
comb3 = combn(n_var,3)
comb4 = combn(n_var,4)
comb5 = combn(n_var,5)

map_to_cols = list(c(19:22),c(23:34),c(35:48),c(49:51),c(52:63))


K_all = lapply(1:length(my_gb_data_county_hot4), function(i)
                1 - distance(my_gb_data_county_hot4[[i]][,c(19:63)], method = "jaccard"))


K_comb2 = lapply(1:length(my_gb_data_county_hot4),
          function(i) lapply(1:dim(comb2)[2], 
             function(j)
               1 - distance(my_gb_data_county_hot4[[i]][,unlist(map_to_cols[c(comb2[1,j], 
                                                                              comb2[2,j])])],
                                              method = "jaccard")))

K_comb3 = lapply(1:length(my_gb_data_county_hot4),
                 function(i) lapply(1:dim(comb3)[2], 
                 function(j)
               1 - distance(my_gb_data_county_hot4[[i]][,unlist(map_to_cols[c(comb3[1,j], 
                                                                              comb3[2,j],
                                                                              comb3[3,j])])],
                                                   method = "jaccard")))

K_comb4 = lapply(1:length(my_gb_data_county_hot4),
                 function(i) lapply(1:dim(comb4)[2], 
                        function(j)
                 1 - distance(my_gb_data_county_hot4[[i]][,unlist(map_to_cols[c(comb4[1,j], 
                                                                                comb4[2,j],
                                                                                comb4[3,j],
                                                                                comb4[4,j])])],
                                                   method = "jaccard")))

K_comb5 = lapply(1:length(my_gb_data_county_hot4),
                 function(i) 1 - distance(my_gb_data_county_hot4[[i]][,unlist(map_to_cols[c(comb5[1], 
                                                                                            comb5[2],
                                                                                            comb5[3],
                                                                                            comb5[4],
                                                                                            comb5[5])])],
                                                   method = "jaccard"))




ckta_comb2 = sapply(1:10, function(i) mapply(new_ckta, K_all, lapply(K_comb2, "[[", i)))
ckta_comb3 = sapply(1:10, function(i) mapply(new_ckta, K_all, lapply(K_comb3, "[[", i)))
ckta_comb4 = sapply(1:5, function(i) mapply(new_ckta, K_all, lapply(K_comb4, "[[", i)))
ckta_comb5 = mapply(new_ckta, K_all, K_comb5)

ckta_comb2 = ckta_comb2[-7,]
ckta_comb3 = ckta_comb3[-7,]
ckta_comb4 = ckta_comb4[-7,]
ckta_comb5 = ckta_comb5[-7]


name_vars = c("Muny Iss. Type",
               "Issuer Ind.",
               "Muny Source",
               "Muny Off. Type",
               "Risk BB 5y")


mat_comb_namevars = lapply(1:5, function(i) combn(name_vars,i))
mat_comb_namevars2 = lapply(1:4, function(i) apply(mat_comb_namevars[[i]], 2, paste, collapse = "/"))
mat_comb_namevars2[[5]] = mat_comb_namevars2[[1]]
mat_comb_namevars2 = mat_comb_namevars2[-1]
mat_comb_namevars2

colnames(ckta_comb2) = mat_comb_namevars2[[1]]
colnames(ckta_comb3) = mat_comb_namevars2[[2]]
colnames(ckta_comb4) = mat_comb_namevars2[[3]]

ckta_comb5 = as.matrix(ckta_comb5)

colnames(ckta_comb5) = "All"


ckta_comb2 = as.data.frame(ckta_comb2)
ckta_comb2$County = sapply(county_all[-7],unique)

melt_comb2 = melt(ckta_comb2)


ggplot(data=melt_comb2, aes(x=County, y=value, color="blue", fill = variable)) +
  geom_bar(stat="identity", color="blue",
           position=position_dodge()) + 
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, 
                                   size = 12,
                                   vjust = 0.5),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) + 
  scale_fill_manual("legend", values = drsimonj_pal("cool")(10)) + 
  ylab("CKTA") + xlab("County")  +
  guides(fill=guide_legend(title="Variables",
                           nrow=4,
                           byrow=TRUE))




ckta_comb3 = as.data.frame(ckta_comb3)
ckta_comb3$County = sapply(county_all[-7],unique)

melt_comb3 = melt(ckta_comb3)

ggplot(data=melt_comb3, aes(x=County, y=value, color="blue", fill = variable)) +
  geom_bar(stat="identity", color="blue",
           position=position_dodge()) + 
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, 
                                   size = 12,
                                   vjust = 0.5),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) + 
  scale_fill_manual("legend", values = drsimonj_pal("cool")(10)) + 
  ylab("CKTA") + xlab("County")  +
  guides(fill=guide_legend(title="Variables",nrow=5,byrow=TRUE))



ckta_comb4 = as.data.frame(ckta_comb4)
ckta_comb4$County = sapply(county_all[-7],unique)

melt_comb4 = melt(ckta_comb4)

ggplot(data=melt_comb4, aes(x=County, y=value, color="blue", fill = variable)) +
  geom_bar(stat="identity", color="blue",
           position=position_dodge()) + 
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, 
                                   size = 12,
                                   vjust = 0.5),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) + 
  scale_fill_manual("legend", values = drsimonj_pal("cool")(10)) + 
  ylab("CKTA") + xlab("County")  +
  guides(fill=guide_legend(title="Variables",nrow=5,byrow=TRUE))

ckta_comb5 = as.data.frame(ckta_comb5)
ckta_comb5$County = sapply(county_all[-7],unique)

ggplot(data=ckta_comb5, aes(x=County, y=All , color= "blue" , fill = "cyan" )) +
  geom_bar(stat="identity", color= "blue" , fill = "cyan") + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, 
                                   size = 12,
                                   vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ylab("CKTA") + xlab("County") 

