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

load("C:/...\\all_join_spline_sc_cali.RData")


#######
# PCA #
#######


pca_poll =  lapply(1:length(all_joined_spline_sc), function(i)
  PCA(all_joined_spline_sc[[i]][,c(11:14)], 
      graph = FALSE,
      scale.unit = F))

pcs = lapply(1:length(all_joined_spline_sc), function(i) pca_poll[[i]]$ind$coord ) 


names(pcs) = sapply(1:length(all_joined_spline_sc),
                    function(i) unique(all_joined_spline_sc[[i]]$County) )

save(pcs, file = "C:/...\\pcs_poll.RData")





range_col<- lapply(1:length(all_joined_spline_sc), function(i)
  colRanges(as.matrix(all_joined_spline_sc[[i]][,c(11:14)])))

features<- colnames(all_joined_spline_sc[[1]][,c(11:14)])

for(i in 1:length(all_joined_spline_sc)){ row.names(range_col[[i]])<- features }
for(i in 1:length(all_joined_spline_sc)){ colnames(range_col[[i]])<- c("min", "max") }


list_ranges_min = t(sapply(range_col, function(x) x[,1]))
list_ranges_max = t(sapply(range_col, function(x) x[,2]))


min_new_mesh = matrix(colMins(list_ranges_min), 1, 4)
max_new_mesh= matrix(colMaxs(list_ranges_max), 1, 4)

colnames(min_new_mesh)<- features
colnames(max_new_mesh)<- features


N <- 100

new_meshes<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))

final_meshes<- do.call(cbind, new_meshes)
colnames(final_meshes) <- features


new_pca = lapply(1:length(all_joined_spline_sc), function(i)
                         predict(pca_poll[[i]], newdata=final_meshes))


new_pcs = lapply(1:length(all_joined_spline_sc), function(i) new_pca[[i]]$coord )


names(new_pcs) = sapply(1:length(all_joined_spline_sc),
                    function(i) unique(all_joined_spline_sc[[i]]$County) )

save(new_pcs, file = "C:/...\\new_pcs_poll.RData")




######################
# DATA construction #
#####################



n_county = length(all_joined_spline_sc)

counties<- sapply(1:n_county, function(i) unique(all_joined_spline_sc[[i]]$County) )

subset_df <- lapply(1:n_county, function(i) all_joined_spline_sc[[i]][,c(1,2,7:10)]   )

subset_df = subset_df[-6] # 1 - remove madera  -- from below

n_county_new = length(subset_df)



subset_df_bis <-  lapply(1:n_county_new, function(i)  subset_df[[i]] %>% 
                           mutate_if(is.numeric, ~ifelse( .<0, 0, .)) )


subset_df_means<- lapply(1:n_county_new, function(i) subset_df_bis[[i]] %>% 
                           group_by(County) %>% 
                           dplyr::summarise(across(everything(), list(mean))) )

subset_df_means_df<- do.call(rbind,subset_df_means)


#one value per year which is the mean across all the values for that year
subset_df2<- subset_df_bis


subset_df2[[1]] = subset_df2[[1]] %>%
  mutate(pm_25_avg_new  = case_when(pm_25_avg_new > 150  ~ runif(1, 0.10, 140) ,
                                    TRUE ~ pm_25_avg_new))

subset_df2[[11]] = subset_df2[[11]] %>%
  mutate(pm_25_avg_new  = case_when(pm_25_avg_new > 150  ~ runif(1, 0.10, 140) ,
                                    TRUE ~ pm_25_avg_new))

for (i in 1:n_county_new) {
  
  subset_df2[[i]]$year = format(subset_df_bis[[i]]$Date, "%Y")
  
}

subset_df_means2<- lapply(1:n_county_new, function(i) subset_df2[[i]] %>% 
                            group_by(County,year) %>% 
                            dplyr::summarise(across(everything(), list(mean))) )


# they all do the same but the first one and the one I used keep the format
#while the other 2 change the format to character
#t(map(subset_df_means2[[1]], 1))
#mapply(`[[`, subset_df_means2[[1]], 1)
#sapply(subset_df_means2[[1]], `[[`, 1) --> this give atomoc vectors 
# lapply(subset_df_means2[[1]], `[[`, 1) --> this DOES NOT


subset_df_means2_df<- lapply(1:(dim(subset_df_means2[[1]])[1]), function(j)
  data.frame( do.call(rbind,
                      lapply(1:length(subset_df_means2), function(i)
                        t(sapply(subset_df_means2[[i]], `[[`, j)) ) ))  )


#################
# PLOTS as KPCA #
#################



subset_df3 <- lapply(1:length(subset_df2), function(i) subset_df2[[i]]%>%
                       mutate(date = yearmonth(Date), 
                              Quarter = yearquarter(Date)) %>%
                       group_by(Quarter) %>%
                       mutate_if(is.numeric, mean) %>%
                       mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                       distinct() %>%
                       filter(row_number()==1))


subset_df4<- do.call(rbind, subset_df3)



subset_df4_bis = subset_df4

subset_df4_bis$Quarter2 = sapply(str_split(subset_df4$Quarter, " "), "[[", 2)

unique(subset_df4$County)

length(unique(subset_df4$County))


sapply(1:length(all_joined_spline_sc), function(i) dim(all_joined_spline_sc[[i]])[1])
sapply(1:length(subset_df2), function(i) dim(subset_df2[[i]])[1])
sapply(1:length(pcs), function(i) length(pcs[[i]][,1]) )
length(pcs)
length(all_joined_spline_sc)
length(subset_df2)


#remove Madera from each list: entry 7

pcs_new =  pcs[-6] 


sapply(1:length(pcs_new), function(i) length(pcs_new[[i]][,1]))
sapply(1:length(subset_df2), function(i) dim(subset_df2[[i]])[1])



pc1_df = lapply(1:length(subset_df2), function(i)
  data.frame(pca1 = pcs_new[[i]][,1],
             County = subset_df2[[i]]$County, 
             year = subset_df2[[i]]$year, 
             Date = subset_df2[[i]]$Date) )



pc1_df2 <- lapply(1:length(pc1_df), function(i) pc1_df[[i]]%>%
                     mutate(date = yearmonth(Date), 
                            Quarter = yearquarter(Date)) %>%
                     group_by(Quarter) %>%
                     mutate_if(is.numeric, mean) %>%
                     mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                     distinct() %>%
                     filter(row_number()==1))




pc2_df = lapply(1:length(subset_df2), function(i)
  data.frame(pca2 = pcs_new[[i]][,2],
             County = subset_df2[[i]]$County, 
             year = subset_df2[[i]]$year, 
             Date = subset_df2[[i]]$Date) )



pc2_df2 <- lapply(1:length(pc2_df), function(i) pc2_df[[i]]%>%
                    mutate(date = yearmonth(Date), 
                           Quarter = yearquarter(Date)) %>%
                    group_by(Quarter) %>%
                    mutate_if(is.numeric, mean) %>%
                    mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                    distinct() %>%
                    filter(row_number()==1))


pc3_df = lapply(1:length(subset_df2), function(i)
  data.frame(pca3 = pcs_new[[i]][,3],
             County = subset_df2[[i]]$County, 
             year = subset_df2[[i]]$year, 
             Date = subset_df2[[i]]$Date) )



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


colors_pc1 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
                "PC1" = "green",
                "PM2.5" = "darkblue")


gg_res_pc1 = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new), color = "AQI")) + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new), color = "CO2" )) + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new), color="NO2" ) ) + 
  geom_line(aes(y = pc1 , color="PC1")) + 
  geom_line(aes(y = pm_25_avg_new/max(pm_25_avg_new), color="PM2.5") ) +
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
       filename = "C:\\...\\pc1_res.pdf",
       width = 15 , height = 6)


colors_pc2 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
                "PC2" = "blue",
                "PM2.5" = "darkblue")


gg_res_pc2 = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new), color = "AQI")) + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new), color = "CO2" )) + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new), color="NO2" ) ) + 
  geom_line(aes(y = pc2 , color="PC2")) + 
  geom_line(aes(y = pm_25_avg_new/max(pm_25_avg_new), color="PM2.5") ) +
  scale_color_manual(name = "",
                     values = colors_pc1) + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years") 

gg_res_pc2


ggsave(gg_res_pc2, 
       filename = "C:\\...\\pc2_res.pdf",
       width = 15 , height = 6)




colors_pc3 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
                "PC3" = "violet",
                "PM2.5" = "darkblue")


gg_res_pc3 = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new), color = "AQI")) + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new), color = "CO2" )) + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new), color="NO2" )) + 
  geom_line(aes(y = pc3 , color="PC3" )) + 
  geom_line(aes(y = pm_25_avg_new/max(pm_25_avg_new), color="PM2.5") )  +
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
       filename = "C:\\...\\pc3_res.pdf",
       width = 15 , height = 6)



#only the pcs

colors_pcs = c( "PC1" = "green",
                 "PC2" = "blue",
                 "PC3" = "violet")



gg_res_pcs = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
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
       filename = "C:\\Users\\...\\pcs_all_res_poll.pdf",
       width = 15 , height = 6)



sub_box_melted = melt(subset_df4_bis2[,c(1,10:13)])
sub_box_melted = sub_box_melted  %>%
  mutate(colors = case_when(
    variable =="pc1" ~ "green",
    variable == "pc2" ~ "blue",
    variable == "pc3" ~ "violet"
  ))

sub_box_melted %>% ggplot(aes(x=variable, 
                              y=value, 
                              color = colors ))+
  scale_color_manual(values=c("blue", "green", "violet"))+
  geom_boxplot()  +    
  facet_grid(vars(Quarter2), vars(County), scales = "free") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("")



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
       filename = "C:\\Users\\...\\pcs_all_boxplots_poll.pdf",
       width = 15 , height = 6)







