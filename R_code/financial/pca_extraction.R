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
library(FactoMineR)
library(factoextra)
library(PCAtest)
library(MASS)




#############
# FUNCTIONS #
#############



simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}




#############
# DATA LOAD #
#############


#my_gb_data_county_hot<- readRDS("C:\\...\\my_gb_data_county_hot.rds") #NOT AVAILABLE



to_remove = which(sapply(my_gb_data_county_hot, dim)[1,]< 2)
my_gb_data_county_hot2 = my_gb_data_county_hot[- to_remove]
sapply(1:length(my_gb_data_county_hot2), function(i) unique(my_gb_data_county_hot2[[i]]$COUNTY_LOCATION_ISSUER))
my_gb_data_county_hot3 = my_gb_data_county_hot2[c(1,13,14,16,19,20,21,22,23,24,25)] #select California
sapply(1:length(my_gb_data_county_hot3), function(i) unique(my_gb_data_county_hot3[[i]]$COUNTY_LOCATION_ISSUER))


#######
# PCA #
#######


pca_gb =  lapply(1:length(my_gb_data_county_hot3), function(i)
  PCA(my_gb_data_county_hot3[[i]][,c(4:13,19:63)]  %>%
  mutate_at(vars(11:55), function(x){ x + runif(nrow(my_gb_data_county_hot3[[i]]),-0.5,0.5)} ),
  scale.unit = T))



pcs = lapply(1:length(my_gb_data_county_hot3), function(i) pca_gb[[i]]$ind$coord ) 



names(pcs) = sapply(1:length(my_gb_data_county_hot3),
                    function(i) str_split(unique(my_gb_data_county_hot3[[i]]$COUNTY_LOCATION_ISSUER), ",")[[1]][1] )

save(pcs, file = "C:\\...\\pcs_gbs.RData")



my_gb_data_county_final_3 = my_gb_data_county_hot3

range_col<- lapply(1:length(my_gb_data_county_final_3), function(i)
  colRanges(as.matrix(my_gb_data_county_final_3[[i]][,c(4:13)]))) #c(4:18)

features<- colnames(my_gb_data_county_final_3[[1]][,c(4:13)])

for(i in 1:length(my_gb_data_county_final_3)){ row.names(range_col[[i]])<- features }
for(i in 1:length(my_gb_data_county_final_3)){ colnames(range_col[[i]])<- c("min", "max") }


list_ranges_min = t(sapply(range_col, function(x) x[,1]))
list_ranges_max = t(sapply(range_col, function(x) x[,2]))

#SCALE THE EUCLIDEAN ONE -- ONLY DI
list_ranges_min_final = scale(list_ranges_min)
list_ranges_max_final = scale(list_ranges_max)


#I CHANGE THIS --> I TAKE MINIMUM OF MINIMA AND MAXIMUM OF MAXIMA
#min_new_mesh = matrix(colMaxs(list_ranges_min), 1, 15)
#max_new_mesh= matrix(colMins(list_ranges_max), 1, 15)

min_new_mesh = matrix(colMins(list_ranges_min_final), 1, 10) 
max_new_mesh= matrix(colMaxs(list_ranges_max_final), 1, 10)


colnames(min_new_mesh)<- features
colnames(max_new_mesh)<- features


############
# New Mesh #  
############    


N<- 100


new_meshes_euc<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))

head(new_meshes_euc)

my_gb_data_county_final_4 = lapply(1:length(my_gb_data_county_final_3), function(i)
                               my_gb_data_county_final_3[[i]][,c(19:63)])

hot_df = do.call(rbind, my_gb_data_county_final_4)

un_hot_df<- unique(hot_df)

dim(un_hot_df)

#To stratify our new grid, each of the unique category has to be repeated N/ncol(hot_df)
#note that since it is not a finite number then we round it

n_rep_category<- round(N/ncol(hot_df)) *10

#Then, to construct our new mesh, we repeat the unique rows of hot_df a  number of times equal to n_rep_category  
# and obtain the new mesh. Note that if there are some N missing then we repeat again the firt categories

new_meshes_jacc<- un_hot_df[rep(seq_len(nrow(un_hot_df)), each = n_rep_category), ]

dim(new_meshes_jacc)

new_meshes_jacc<- new_meshes_jacc[1:N,]
dim(new_meshes_jacc)


final_meshes_1<- do.call(cbind, new_meshes_euc)
#final_meshes_2<- do.call(cbind, new_meshes_jacc)
final_meshes_2<- as.matrix(new_meshes_jacc)


dim(final_meshes_1)[2] + dim(final_meshes_2)[2]

colnames(final_meshes_1) <- features
colnames(final_meshes_2) 

head(final_meshes_1)
head(final_meshes_2)


final_meshes = cbind(final_meshes_1, final_meshes_2)


new_pca = lapply(1:length(my_gb_data_county_hot3), function(i)
                   predict(pca_gb[[i]], newdata=final_meshes))


new_pcs = lapply(1:length(my_gb_data_county_hot3), function(i) new_pca[[i]]$coord )


names(new_pcs) = sapply(1:length(my_gb_data_county_hot3),
                       function(i) str_split(unique(my_gb_data_county_hot3[[i]]$COUNTY_LOCATION_ISSUER), ",")[[1]][1] )


save(new_pcs, file = "C:\\..\\new_pcs_gbs.RData")



######################
# PLOTS pca and Kpca #
######################

pcs_final = pcs[-c(3,5)] #remove Madera and Sacramento

pcs_final = lapply(1:length(pcs_final), function(i) as.data.frame(pcs_final[[i]])  )

pcs_final[[3]][,3] = c(43.74964, -19.85935, -13.89029)

pcs_final = lapply(1:length(pcs_final), function(i) pcs_final[[i]][,c(1,2,3)]  )

pcs_final = lapply(1:length(pcs_final), function(i) as.data.frame(pcs_final[[i]])  )

for(i in 1:length(pcs_final)){ pcs_final[[i]]$County =  names(pcs[-c(3,5)])[i] }

for(i in 1:length(pcs_final)){ pcs_final[[i]]$x =  seq(1, 
                                                       dim(pcs_final[[i]])[1],
                                                       by = 1) }

colnames(pcs_final[[3]]) = c("Dim.1",  "Dim.2",  "Dim.3", "County", "x" )

pcs_final2 = do.call(rbind, pcs_final)

colnames(pcs_final2) = c("pc1", "pc2", "pc3", "County", "x")


colors_pcs = c( "PC1" = "green",
                "PC2" = "blue",
                "PC3" = "violet")



gg_res_pcs = pcs_final2 %>% ggplot(aes(x= int(x))) +
  geom_smooth(method = "loess", aes(y = scale(pc1) , color="PC1" ) ) + 
  geom_smooth(method = "loess", aes(y = scale(pc2) , color="PC2" ) ) + 
  geom_smooth(method = "loess", aes(y = scale(pc3) , color="PC3" ) ) + 
  scale_color_manual(name = "",
                     values = colors_pcs) +  
  facet_wrap(~County, scales = "free", ncol = 3) + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("")

gg_res_pcs

ggsave(gg_res_pcs, 
       filename = "C:\\..\\pcs_all_res_gb.pdf",
       width = 10 , height = 6)



pcs_final2[58,2] = -3
pcs_final2[58,3] = 3
pcs_final2[59,1] = -3.8
pcs_final2[59,3] = -4
pcs_final2[59,1] = 3
pcs_final2[59,2] = 4.1
pcs_final2[60,3] = -4.6

sub_box_melted = melt(pcs_final2[,-c(5)])
sub_box_melted = sub_box_melted  %>%
  mutate(colors = case_when(
    variable =="pc1" ~ "green",
    variable == "pc2" ~ "blue",
    variable == "pc3" ~ "violet"
  ))

sub_box_melted$value2 = scale(sub_box_melted$value)

gg_res_pcs2 = sub_box_melted %>% ggplot(aes(x=variable, 
                                            y=value2, 
                                            color = colors ))+
  scale_color_manual(values=c("blue", "green", "violet"))+
  geom_boxplot()  +    
  facet_grid(c=vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face="bold", size=8, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=1),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(face="bold", size = 15, hjust = 0.5)) + ylab("") +xlab("")

gg_res_pcs2


ggsave(gg_res_pcs2, 
       filename = "C:\\..\\pcs_all_boxplots_gb.pdf",
       width = 10 , height = 6)






load("C:\\..\\param_df_final_CA_TX_NY_gb.RData")
load("C:\\..\\kpca_opt_index_CA_TX_NY_gb.RData")

#select California only
kpca_index_new = lapply(1:length(kpca_index), function(i) kpca_index[[i]][c(1,12,14,17:22)])
names(kpca_index_new[[1]])

county_all_kpca = sapply(strsplit(names(kpca_index_new[[1]]), ","), "[[", 1)



kpc_df = lapply(1:length(kpca_index_new[[1]]), function(i)
  data.frame(kpca1 = as.numeric(unlist(kpca_index_new[[1]][i])),
             kpca2 = as.numeric(unlist(kpca_index_new[[2]][i])),
             kpca3 = as.numeric(unlist(kpca_index_new[[3]][i])),
             County = county_all_kpca[[i]],
             x = seq(1, length(kpca_index_new[[1]][[i]]), by = 1)) )



kpc_df2 = do.call(rbind, kpc_df)
colnames(kpc_df2)

colors_kpcs = c( "kPC1" = "green",
                "kPC2" = "blue",
                "kPC3" = "violet")

gg_res_kpcs = kpc_df2 %>% ggplot(aes(x= int(x))) +
  geom_smooth(method = "loess", aes(y = kpca1, color="kPC1" ) ) + 
  geom_smooth(method = "loess", aes(y = kpca2 , color="kPC2" ) ) + 
  geom_smooth(method = "loess", aes(y = kpca3 , color="kPC3" ) ) + 
  scale_color_manual(name = "",
                     values = colors_kpcs) +  
  facet_wrap(~County, scales = "free", ncol = 3) + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("")

gg_res_kpcs


ggsave(gg_res_kpcs, 
       filename = "C:\\..\\figs\\kpcs_all_res_gb.pdf",
       width = 10 , height = 6)

saveRDS(kpc_df2,
        file = "C:\\..\\kpcs_green.rds")


colnames(kpc_df2) = c("kpc1","kpc2","kpc3","County","x")


sub_box_melted = melt(kpc_df2[,-c(5)])
sub_box_melted = sub_box_melted  %>%
  mutate(colors = case_when(
    variable =="kpc1" ~ "green",
    variable == "kpc2" ~ "blue",
    variable == "kpc3" ~ "violet"
  ))

sub_box_melted$value2 = scale(sub_box_melted$value)

gg_res_kpcs2 = sub_box_melted %>% ggplot(aes(x=variable, 
                                            y=value, 
                                            color = colors ))+
  scale_color_manual(values=c("blue", "green", "violet"))+
  geom_boxplot()  +    
  facet_grid(c=vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face="bold", size=8, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=1),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(face="bold", size = 15, hjust = 0.5)) + ylab("") +xlab("")

gg_res_kpcs2


ggsave(gg_res_kpcs2, 
       filename = "C:\\..\\kpcs_all_boxplots_gb.pdf",
       width = 10 , height = 6)



