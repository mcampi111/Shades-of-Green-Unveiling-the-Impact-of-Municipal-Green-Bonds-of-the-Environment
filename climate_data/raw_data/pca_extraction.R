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


df_final_percounty<- readRDS("C:/...\\df_final_percounty.rds")



##########
#   PCA  #
##########

n_county = length(df_final_percounty)

pca_climate =  lapply(1:length(df_final_percounty), function(i)
                        PCA(df_final_percounty[[i]][,c(6:8)], 
                            graph = FALSE,
                            scale.unit = F))

pcs = lapply(1:length(df_final_percounty), function(i) pca_climate[[i]]$ind$coord ) 


names(pcs) = sapply(1:n_county, function(i) unique(df_final_percounty[[i]]$COUNTY) )

save(pcs, file = "C:/...\\California\\pcs_climate.RData")




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

N <- 100

new_meshes<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))

final_meshes<- do.call(cbind, new_meshes)
colnames(final_meshes) <- features



new_pca = lapply(1:length(df_final_percounty), function(i)
  predict(pca_climate[[i]], newdata=final_meshes))


new_pcs = lapply(1:length(df_final_percounty), function(i) new_pca[[i]]$coord )


names(new_pcs) = sapply(1:length(df_final_percounty),
                        function(i) unique(df_final_percounty[[i]]$COUNTY) )

save(new_pcs, file = "C:/...\\new_pcs_climate.RData")





######################
# DATA construction #
#####################


counties<- sapply(1:n_county, function(i) unique(df_final_percounty[[i]]$COUNTY) )

counties<- sapply(1:n_county, function(i) simpleCap(counties[i]))



df_means<- lapply(1:n_county, function(i) df_final_percounty[[i]] %>% 
                    group_by(COUNTY) %>% 
                    dplyr::summarise(across(everything(), list(mean))) )

df_means<- do.call(rbind,df_means)



df_final_percounty2 = df_final_percounty

for (i in 1:n_county) {
  
  df_final_percounty2[[i]]$year = format(df_final_percounty2[[i]]$YEARMODA, "%Y")
  
}

df_means_years<- lapply(1:n_county, function(i) df_final_percounty2[[i]] %>% 
                          group_by(COUNTY,year) %>% 
                          dplyr::summarise(across(everything(), list(mean))) )



################
# PLOTS as KPCA #
################

df3 <- lapply(1:length(df_final_percounty2), function(i) df_final_percounty2[[i]]%>%
                mutate(date = yearmonth(YEARMODA), 
                       Quarter = yearquarter(YEARMODA)) %>%
                group_by(Quarter) %>%
                mutate_if(is.numeric, mean) %>%
                mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                distinct() %>%
                filter(row_number()==1))


subset_df4<- do.call(rbind, df3)

subset_df4_bis = subset_df4

subset_df4_bis$Quarter2 = sapply(str_split(subset_df4$Quarter, " "), "[[", 2)


unique(subset_df4$COUNTY)


sapply(1:length(df_final_percounty), function(i) dim(df_final_percounty[[i]])[1])
sapply(1:length(df_final_percounty), function(i) length(pcs[[i]][,1]) )



pc1_df = lapply(1:length(df_final_percounty), function(i)
                 data.frame(pca1 = pcs[[i]][,1],
                            County = df_final_percounty2[[i]]$COUNTY, 
                            Date = df_final_percounty2[[i]]$YEARMODA)   ) 



pc1_df2 <- lapply(1:length(pc1_df), function(i) pc1_df[[i]]%>%
                     mutate(date = yearmonth(Date), 
                            Quarter = yearquarter(Date)) %>%
                     group_by(Quarter) %>%
                     mutate_if(is.numeric, mean) %>%
                     mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                     distinct() %>%
                     filter(row_number()==1))



pc2_df = lapply(1:length(df_final_percounty), function(i)
  data.frame(pca2 = pcs[[i]][,2],
             County = df_final_percounty2[[i]]$COUNTY, 
             Date = df_final_percounty2[[i]]$YEARMODA)   ) 



pc2_df2 <- lapply(1:length(pc2_df), function(i) pc2_df[[i]]%>%
                    mutate(date = yearmonth(Date), 
                           Quarter = yearquarter(Date)) %>%
                    group_by(Quarter) %>%
                    mutate_if(is.numeric, mean) %>%
                    mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                    distinct() %>%
                    filter(row_number()==1))




pc3_df = lapply(1:length(df_final_percounty), function(i)
  data.frame(pca3 = pcs[[i]][,3],
             County = df_final_percounty2[[i]]$COUNTY, 
             Date = df_final_percounty2[[i]]$YEARMODA)   ) 



pc3_df2 <- lapply(1:length(pc3_df), function(i) pc3_df[[i]]%>%
                    mutate(date = yearmonth(Date), 
                           Quarter = yearquarter(Date)) %>%
                    group_by(Quarter) %>%
                    mutate_if(is.numeric, mean) %>%
                    mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                    distinct() %>%
                    filter(row_number()==1))




pc1_df3<- do.call(rbind, pc1_df2)
pc2_df3<- do.call(rbind, pc2_df2)
pc3_df3<- do.call(rbind, pc3_df2)


subset_df4_bis2 = subset_df4_bis

subset_df4_bis2$pc1 = pc1_df3$pca1
subset_df4_bis2$pc2 = pc2_df3$pca2
subset_df4_bis2$pc3 = pc3_df3$pca3


#delete for consistency with the pollution dataset

todelete = c("madera", "marin", 
             "yolo", "stanislaus")

subset_df4_bis2 = subset_df4_bis2[!subset_df4_bis2$COUNTY %in% todelete, ]

subset_df4_bis2$County = sapply(1:dim(subset_df4_bis2)[1], function(i) simpleCap(subset_df4_bis2$COUNTY[i]))


colors_pc1 = c("Temperature" = "chartreuse4", 
               "Precipitation" = "dodgerblue",
               "Volatility" = "lightpink3", 
               "PC1" = "green")

gg_res_pc1 = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg), color = "Temperature")) + 
  geom_line(aes(y = tot_prcp/max(tot_prcp), color = "Precipitation" )) + 
  geom_line(aes(y = avg_vol/max(avg_vol), color="Volatility" ) ) + 
  geom_line(aes(y = pc1 , color="PC1")) + 
  scale_color_manual(name = "",
                     values = colors_pc1) + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years") 

gg_res_pc1


ggsave(gg_res_pc1, 
       filename = "C:\\...\\pc1_res_clim.pdf",
       width = 15 , height = 6)



colors_pc2 = c("Temperature" = "chartreuse4", 
                "Precipitation" = "dodgerblue",
                "Volatility" = "lightpink3", 
                "PC2" = "blue")


gg_res_pc2 = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg), color = "Temperature")) + 
  geom_line(aes(y = tot_prcp/max(tot_prcp), color = "Precipitation" )) + 
  geom_line(aes(y = avg_vol/max(avg_vol), color="Volatility" ) ) + 
  geom_line(aes(y = pc2 , color="PC2")) + 
  scale_color_manual(name = "",
                     values = colors_pc2) + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years") 

gg_res_pc2


ggsave(gg_res_pc2, 
       filename = "C:\\Users\\...\\pc2_res_clim.pdf",
       width = 15 , height = 6)



colors_pc3 = c("Temperature" = "chartreuse4", 
               "Precipitation" = "dodgerblue",
               "Volatility" = "lightpink3", 
               "PC3" = "purple")


gg_res_pc3 = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg), color = "Temperature")) + 
  geom_line(aes(y = tot_prcp/max(tot_prcp), color = "Precipitation" )) + 
  geom_line(aes(y = avg_vol/max(avg_vol), color="Volatility" ) ) + 
  geom_line(aes(y = pc3 , color="PC3")) + 
  scale_color_manual(name = "",
                     values = colors_pc3) + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years") 

gg_res_pc3

ggsave(gg_res_pc3, 
       filename = "C:\\Users\\...\\pc3_res_clim.pdf",
       width = 15 , height = 6)




#only the pcs

colors_pcs = c( "PC1" = "green",
                "PC2" = "blue",
                "PC3" = "violet")



gg_res_pcs = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_smooth(method = "loess", aes(y = pc1 , color="PC1" ) ) + 
  geom_smooth(method = "loess", aes(y = pc2 , color="PC2" ) ) + 
  geom_smooth(method = "loess", aes(y = pc3 , color="PC3" ) ) + 
  scale_color_manual(name = "",
                     values = colors_pcs) +  
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years")

gg_res_pcs


ggsave(gg_res_pcs, 
       filename = "C:\\...\\pcs_all_res_clim.pdf",
       width = 15 , height = 6)



sub_box_melted = melt(subset_df4_bis2[,c(12:16)])
sub_box_melted = sub_box_melted  %>%
  mutate(colors = case_when(
    variable =="pc1" ~ "green",
    variable == "pc2" ~ "blue",
    variable == "pc3" ~ "violet"
  ))


gg_res_pcs2 = sub_box_melted %>% ggplot(aes(x=variable, 
                                             y=value, 
                                             color = colors ))+
  scale_color_manual(values=c("blue", "green", "violet"))+
  geom_boxplot()  +    
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face="bold", size=8, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=1),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(face="bold", size = 15, hjust = 0.5)) + ylab("") +xlab("")


gg_res_pcs2

ggsave(gg_res_pcs2, 
       filename = "C:\\...\\pcs_all_boxplots_clim.pdf",
       width = 15 , height = 6)













