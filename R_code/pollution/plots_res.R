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
  `dark grey`  = "#8c8c8c")


drsimonj_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (drsimonj_colors)
  
  drsimonj_colors[cols]
}

drsimonj_cols()

drsimonj_palettes <- list(
  `main`  = drsimonj_cols("blue", "green", "yellow"),
  
  `cool`  = drsimonj_cols("blue", "green"),
  
  `hot`   = drsimonj_cols("yellow", "orange", "red"),
  
  `mixed` = drsimonj_cols("blue", "green", "yellow", "orange", "red"),
  
  `grey`  = drsimonj_cols("light grey", "dark grey")
)


drsimonj_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- drsimonj_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

drsimonj_pal("cool")(10)


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


######################
# DATA construction #
#####################

n_county = length(all_joined_spline_sc)

counties<- sapply(1:n_county, function(i) unique(all_joined_spline_sc[[i]]$County) )

subset_df <- lapply(1:n_county, function(i) all_joined_spline_sc[[i]][,c(1,2,7:10)]   )

subset_df = subset_df[-6] # 1 - remove madera  -- from below

n_county_new = length(subset_df)



#one value which is the mean across all the years __ before taking the mean
# replace all negative values with 0

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


#########
# PLOTs #
#########

#1) one value for all 10 years

county_gg<- map_data("county")
california <- subset(county_gg, region %in% c("california"))

california$County = sapply(1:dim(california)[1], function(i) simpleCap(california$subregion[i]))

#base plot 
ggplot(data = california) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3)


toplot<- merge(california,
               subset_df_means_df, 
               by="County", 
               all.x=T)


toplot <- toplot[order(toplot$order),]

gg0 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=aqi_avg_new_1))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged AQI")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8))  +
  ylab("Latitude") + xlab("Longitude") 

gg0


ggsave(gg0, 
       filename = "C:\\...\\avg_aqi.pdf",
       width = 5 , height = 5)

dev.off()


gg01 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=co2_avg_new_1 ))+
  geom_path()+ 
  scale_fill_gradientn(colours=drsimonj_pal("cool")(10),na.value="grey90",
                       name = "Averaged Co2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) +
  ylab("Latitude") + xlab("Longitude") 

gg01

ggsave(gg01, 
       filename = "C:\\...\\avg_co2.pdf",
        width = 5 , height = 5)

dev.off()


gg02 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=no2_avg_new_1 ))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged No2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) +
  ylab("Latitude") + xlab("Longitude") + scale_fill_distiller(palette = "RdPu",
                                                              na.value="grey90",
                                                              name = "Averaged No2")

gg02

ggsave(gg02, 
       filename = "C:\\...\\avg_no2.pdf",
       width = 5 , height = 5)

dev.off()


gg03 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=pm_25_avg_new_1 ))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
               name = "Averaged PM2.5")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) +
  ylab("Latitude") + xlab("Longitude") + scale_fill_distiller(na.value="grey90",
                                                              name = "Averaged PM2.5")
                             
gg03

ggsave(gg03, 
       filename = "C:\\...\\avg_pm25.pdf",
       width = 5 , height = 5)

dev.off()



#2) one value per year


toplot_by_year<- lapply(1:10, function(i)  
  merge(california,
        subset_df_means2_df[[i]],  # subset_df_means2
        by="County", 
        all.x=T) ) 


toplot_by_year <- lapply(1:10, function(i) 
                      toplot_by_year[[i]][order(toplot_by_year[[i]]$order),] )

year<- sapply(1:10, function(i) unique(toplot_by_year[[i]]$year) )
year<- year[1,]

for (i in 1:10) {
  
  toplot_by_year[[i]]$year2 <- rep(year[i], dim( toplot_by_year[[i]])[1] )
  
}


toplot_by_year_final = do.call(rbind, toplot_by_year)

 

gg1 = ggplot(toplot_by_year_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(aqi_avg_new_1)   ))+
  geom_path()+ facet_wrap(~ year2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged AQI")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) + xlab("Longitude") + ylab("Latitude")
gg1

ggsave(gg1, 
       filename = "C:\\...\\avg_aqi_year.pdf")


gg2 = ggplot(toplot_by_year_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(co2_avg_new_1)   ))+
  geom_path()+ facet_wrap(~ year2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged Co2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) 

ggsave(gg2, 
       filename = "C:\\...\\avg_co2_year.pdf")



gg3 = ggplot(toplot_by_year_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(no2_avg_new_1)   ))+
  geom_path()+ facet_wrap(~ year2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged No2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) 

ggsave(gg3, 
       filename = "C:\\...\\avg_no2_year.pdf")


gg4 = ggplot(toplot_by_year_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(pm_25_avg_new_1 )   ))+
  geom_path()+ facet_wrap(~ year2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged PM2.5")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) 

ggsave(gg4, 
       filename = "C:\\...\\avg_pm25_year.pdf")




prova0 = lapply(1:n_county_new, function(i) subset_df_means2[[i]][,-3] )
prova = melt(prova0)


ggbox = ggplot(prova , aes(x=County, y=value , color=County)) +
  geom_boxplot() + facet_wrap(~ variable, scales="free_y",
                              labeller = as_labeller(c("aqi_avg_new_1" = "AQI",
                                           "co2_avg_new_1" = "Co2",
                                           "no2_avg_new_1" = "No2",
                                           "pm_25_avg_new_1" = "PM2.5") ) ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=8, angle = 90 ),
        axis.text.y = element_text( size=9.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") +ylab("")

ggbox

ggsave(ggbox, 
       filename = "C:\\...\\box_poll.pdf")




###############
# By Quarters #
###############

#1 county
subset_df2[[1]]%>%
  mutate(date = yearmonth(Date), 
         Quarter = yearquarter(Date)) %>%
  group_by(Quarter) %>%
  mutate_if(is.numeric, mean) %>%
  mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
  distinct() %>%
  filter(row_number()==1)%>%
  as_tsibble(index = Quarter)%>%
  gg_subseries(aqi_avg_new)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged AQI")  + 
   xlab("Years")


#ALL COUNTIES

subset_df3 <- lapply(1:length(subset_df2), function(i) subset_df2[[i]]%>%
                       mutate(date = yearmonth(Date), 
                              Quarter = yearquarter(Date)) %>%
                       group_by(Quarter) %>%
                       mutate_if(is.numeric, mean) %>%
                       mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                       distinct() %>%
                       filter(row_number()==1))


subset_df4<- do.call(rbind, subset_df3)



subset_df4 %>%
  as_tsibble(key = County,index = Quarter) %>%
  gg_subseries(aqi_avg_new)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 8)) + ylab("Averaged AQI")  + 
  xlab("Years")


subset_df4 %>%
  as_tsibble(key = County,index = Quarter) %>%
  gg_subseries(co2_avg_new)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged Co2")  + 
  xlab("Years")



subset_df4 %>%
  as_tsibble(key = County,index = Quarter) %>%
  gg_subseries(no2_avg_new)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged No2")  + 
  xlab("Years")


subset_df4 %>%
  as_tsibble(key = County,index = Quarter) %>%
  gg_subseries(pm_25_avg_new)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged PM2.5")  + 
  xlab("Years")






subset_df4_bis = subset_df4

subset_df4_bis$Quarter2 = sapply(str_split(subset_df4$Quarter, " "), "[[", 2)

subset_df4_bis %>% ggplot(aes(x= Date , y=aqi_avg_new)) +
  geom_line() + facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged AQI")  + 
  xlab("Years")


subset_df4_bis %>% ggplot(aes(x= Date , y=co2_avg_new)) +
  geom_line() + facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged CO2")  + 
  xlab("Years")


subset_df4_bis %>% ggplot(aes(x= Date , y=no2_avg_new)) +
  geom_line() + facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged No2")  + 
  xlab("Years")


subset_df4_bis %>% ggplot(aes(x= Date , y=pm_25_avg_new)) +
  geom_line() + facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged PM2.5")  + 
  xlab("Years")


#####################
#boxplot by quarters#
#####################

prova_box_qrt = melt(subset_df4_bis[,-c(2,8,9)])

ggplot(prova_box_qrt , aes(x=County, y=value , color=County)) +
  geom_boxplot() + facet_grid(variable~ Quarter2, scales="free",
                              labeller = labeller(variable = c("aqi_avg_new" = "AQI",
                                                       "co2_avg_new" = "Co2",
                                                       "no2_avg_new" = "No2",
                                                       "pm_25_avg_new" = "PM2.5") ) ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=8, angle = 90 ),
        axis.text.y = element_text( size=9.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") +ylab("") 


calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}


ggbox_quart = ggplot(prova_box_qrt , aes(x=County, y=value , color=County)) +
  stat_summary(fun.data = calc_boxplot_stat, 
               geom="boxplot") + 
   facet_grid(variable~ Quarter2, scales="free",
               labeller = labeller(variable = c("aqi_avg_new" = "AQI",
                                                "co2_avg_new" = "Co2",
                                                "no2_avg_new" = "No2",
                                                "pm_25_avg_new" = "PM2.5") ) )+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=8, angle = 90 ),
        axis.text.y = element_text( size=9.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") +ylab("") 

ggbox_quart

ggsave(ggbox_quart, width = 10.78, height = 6.7,
       filename = "C:\\...\\box_4_poll.pdf")


####################
#trial all together#
####################

  

subset_df4_bis %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new)), color = "darkred") + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new) ), color = "darkgreen") + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new) ), color="gold2") + 
  #geom_line(aes(y = pm_25_avg_new), color="darkblue") + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years")


load("C:\\...\\param_df_final_cali.RData")
load("C:\\...\\kpca_opt_index_cali_poll.RData")

#remove Madera from each list: entry 7

param_df_final$County
unique(subset_df4$County)

kpca_index_new = lapply(1:length(kpca_index), function(i) kpca_index[[i]][-6] )

sapply(1:length(kpca_index), function(i) length(kpca_index_new[[i]]))


#other check
sapply(1:length(subset_df2), function(i) dim(subset_df2[[i]])[1])
sapply(1:length(subset_df), function(i) length(kpca_index_new[[1]][[i]]) )

names(kpca_index_new[[1]])
unique(subset_df4$County)


kpc1_df = lapply(1:length(subset_df2), function(i)
                          data.frame(kpca1 = as.numeric(unlist(kpca_index_new[[1]][i])),
                                     County = subset_df2[[i]]$County, 
                                     year = subset_df2[[i]]$year, 
                                     Date = subset_df2[[i]]$Date) )



kpc1_df2 <- lapply(1:length(kpc1_df), function(i) kpc1_df[[i]]%>%
                       mutate(date = yearmonth(Date), 
                              Quarter = yearquarter(Date)) %>%
                       group_by(Quarter) %>%
                       mutate_if(is.numeric, mean) %>%
                       mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                       distinct() %>%
                       filter(row_number()==1))



kpc2_df = lapply(1:length(subset_df2), function(i)
  data.frame(kpca2 = as.numeric(unlist(kpca_index_new[[2]][i])),
             County = subset_df2[[i]]$County, 
             year = subset_df2[[i]]$year, 
             Date = subset_df2[[i]]$Date) )



kpc2_df2 <- lapply(1:length(kpc2_df), function(i) kpc2_df[[i]]%>%
                     mutate(date = yearmonth(Date), 
                            Quarter = yearquarter(Date)) %>%
                     group_by(Quarter) %>%
                     mutate_if(is.numeric, mean) %>%
                     mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                     distinct() %>%
                     filter(row_number()==1))



kpc3_df = lapply(1:length(subset_df2), function(i)
  data.frame(kpca3 = as.numeric(unlist(kpca_index_new[[3]][i])),
             County = subset_df2[[i]]$County, 
             year = subset_df2[[i]]$year, 
             Date = subset_df2[[i]]$Date) )




kpc3_df2 <- lapply(1:length(kpc3_df), function(i) kpc3_df[[i]]%>%
                     mutate(date = yearmonth(Date), 
                            Quarter = yearquarter(Date)) %>%
                     group_by(Quarter) %>%
                     mutate_if(is.numeric, mean) %>%
                     mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                     distinct() %>%
                     filter(row_number()==1))



kpc1_df3<- do.call(rbind, kpc1_df2)
kpc2_df3<- do.call(rbind, kpc2_df2)
kpc3_df3<- do.call(rbind, kpc3_df2)


subset_df4_bis2 = subset_df4_bis

subset_df4_bis2$kpc1 = kpc1_df3$kpca1
subset_df4_bis2$kpc2 = kpc2_df3$kpca2
subset_df4_bis2$kpc3 = kpc3_df3$kpca3


colors_kpc1 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
           "KPC1" = "green",
           "PM2.5" = "darkblue")

gg_res_kpc1 = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new), color = "AQI")) + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new), color = "CO2" )) + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new), color="NO2" ) ) + 
  geom_line(aes(y = kpc1 , color="KPC1")) + 
  geom_line(aes(y = pm_25_avg_new/max(pm_25_avg_new), color="PM2.5") ) +
  scale_color_manual(name = "",
                     values = colors_kpc1) + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years") 

gg_res_kpc1

ggsave(gg_res_kpc1, 
       filename = "C:\\...\\kpc1_res.pdf",
       width = 15 , height = 6)


colors_kpc2 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
                "KPC2" = "blue",
                "PM2.5" = "darkblue")

gg_res_kpc2 = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new), color = "AQI")) + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new), color = "CO2" )) + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new), color="NO2" )) + 
  geom_line(aes(y = kpc2 , color="KPC2")) + 
  geom_line(aes(y = pm_25_avg_new/max(pm_25_avg_new), color="PM2.5"))  +
  scale_color_manual(name = "",
                     values = colors_kpc2) + 
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years")

gg_res_kpc2

ggsave(gg_res_kpc2, 
       filename = "C:\\...\\kpc2_res.pdf",
       width = 15 , height = 6)


colors_kpc3 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
                "KPC3" = "violet",
                "PM2.5" = "darkblue")

gg_res_kpc3 = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_line(aes(y = aqi_avg_new/max(aqi_avg_new), color = "AQI")) + 
  geom_line(aes(y = co2_avg_new/max(co2_avg_new), color = "CO2" )) + 
  geom_line(aes(y = no2_avg_new/max(no2_avg_new), color="NO2" )) + 
  geom_line(aes(y = kpc3 , color="KPC3" )) + 
  geom_line(aes(y = pm_25_avg_new/max(pm_25_avg_new), color="PM2.5") )  +
  scale_color_manual(name = "",
                     values = colors_kpc3) +  
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years")


gg_res_kpc3

ggsave(gg_res_kpc3, 
       filename = "C:\\...\\kpc3_res.pdf",
       width = 15 , height = 6)


colors_kpc3 = c("AQI" = "darkred", 
                "CO2" = "darkgreen",
                "NO2" = "gold2", 
                "KPC3" = "violet",
                "PM2.5" = "darkblue")

#only the kpcs

colors_kpcs = c( "KPC1" = "green",
                 "KPC2" = "blue",
                "KPC3" = "violet")



gg_res_kpcs = subset_df4_bis2 %>% ggplot(aes(x= Date)) +
  geom_smooth(method = "loess", aes(y = kpc1 , color="KPC1" ) ) + 
  geom_smooth(method = "loess", aes(y = kpc2 , color="KPC2" ) ) + 
  geom_smooth(method = "loess", aes(y = kpc3 , color="KPC3" ) ) +
  scale_color_manual(name = "",
                     values = colors_kpcs) +  
  facet_grid(vars(Quarter2), vars(County), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years")

gg_res_kpcs


ggsave(gg_res_kpcs, 
       filename = "C:\\...\\kpcs_all_res_poll.pdf",
       width = 15 , height = 6)




saveRDS(subset_df4_bis2,
        file = "C:\\...\\subset_df4_bis2_epa.rds")


sub_box_melted = melt(subset_df4_bis2[,c(1,10:13)])
sub_box_melted = sub_box_melted  %>%
  mutate(colors = case_when(
    variable =="kpc1" ~ "green",
    variable == "kpc2" ~ "blue",
    variable == "kpc3" ~ "violet"
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



gg_res_kpcs2 = sub_box_melted %>% ggplot(aes(x=variable, 
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

gg_res_kpcs2

ggsave(gg_res_kpcs2, 
       filename = "C:\\Users\\mcampi\\Desktop\\Env_Exp\\epa_web\\California\\figs\\kpcs_all_boxplots_poll.pdf",
       width = 15 , height = 6)



##################################
# Maps according to the quarters #
##################################

subset_df5 = subset_df3

for (i in 1:14) {subset_df5[[i]]$Quarter2 = quarter(subset_df5[[i]]$Quarter) } 


subset_df6 = lapply(1:14, function(i) subset_df5[[i]] %>% group_by(Quarter2)  %>%
                                      mutate_if(is.numeric, mean) %>%
                                      mutate_if(is.character,
                                                funs(paste(unique(.), collapse = "_"))) %>%
                                                  distinct() %>%
                                                    filter(row_number()==1)  %>%
                                                           select(-year))

subset_df7 = lapply(1:(dim(subset_df6[[1]])[1]), function(j)
                        data.frame( do.call(rbind,
                            lapply(1:length(subset_df_means2), function(i)
                                 t(sapply(subset_df6[[i]], `[[`, j)) ) ))  )


toplot_by_year2<- lapply(1:4, function(i)  
  merge(california,
        subset_df7[[i]],  
        by="County", 
        all.x=T) ) 


toplot_by_year2 <- lapply(1:4, function(i) 
  toplot_by_year2[[i]][order(toplot_by_year2[[i]]$order),] )

quart<- sapply(1:4, function(i) unique(toplot_by_year2[[i]]$Quarter2) )
quart<- quart[1,]

for (i in 1:4) {
  
  toplot_by_year2[[i]]$Quarter3 <- rep(quart[i], dim( toplot_by_year2[[i]])[1] )
  
}



toplot_by_quarter_final = do.call(rbind, toplot_by_year2)



gg11 = ggplot(toplot_by_quarter_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(aqi_avg_new  )   ))+
  geom_path()+ facet_wrap(~  Quarter3, ncol = 4,labeller= as_labeller(c("1" = "Q1", 
                                                                        "2" = "Q2",
                                                                        "3" = "Q3", 
                                                                        "4" = "Q4")) )+
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       name = "Averaged AQI")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) +
  ylab("Latitude") + xlab("Longitude")

gg11

ggsave(gg11, 
       filename = "C:\\...\\avg_aqi_quart.pdf",
       width = 10 , height = 5)


gg12 = ggplot(toplot_by_quarter_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(co2_avg_new   )   ))+
  geom_path()+ facet_wrap(~ Quarter3, ncol = 4, labeller= as_labeller(c("1" = "Q1", 
                                                                      "2" = "Q2",
                                                                      "3" = "Q3", 
                                                                      "4" = "Q4")) )+
  scale_fill_gradientn(colours=drsimonj_pal("cool")(10),na.value="grey90",
                       name = "Averaged Co2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) +
  ylab("Latitude") + xlab("Longitude")

gg12

ggsave(gg12, 
       filename = "C:\\...\\avg_co2_quart.pdf",
       width = 10 , height = 5)



gg13 = ggplot(toplot_by_quarter_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(no2_avg_new   )   ))+
  geom_path()+ facet_wrap(~  Quarter3, ncol = 4,labeller= as_labeller(c("1" = "Q1", 
                                                                        "2" = "Q2",
                                                                        "3" = "Q3", 
                                                                        "4" = "Q4")) )+
  scale_fill_gradientn(colours=drsimonj_pal("cool")(10),na.value="grey90",
                       name = "Averaged No2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8)) + scale_fill_distiller(palette = "RdPu",
                                                                     na.value="grey90",
                                                                     name = "Averaged No2")+
  ylab("Latitude") + xlab("Longitude")


gg13

ggsave(gg13, 
       filename = "C:\\...\\avg_no2_quart.pdf",
       width = 10 , height = 5)


gg14 = ggplot(toplot_by_quarter_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(pm_25_avg_new)   ))+
  geom_path()+ facet_wrap(~ Quarter3, ncol = 4, labeller= as_labeller(c("1" = "Q1", 
                                                                      "2" = "Q2",
                                                                      "3" = "Q3", 
                                                                      "4" = "Q4")) )+
  scale_fill_gradientn(colours=drsimonj_pal("cool")(10),na.value="grey90",
                       name = "Averaged No2")+
  coord_fixed(1.3) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=7 ),
        axis.text.y = element_text( size=7.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.spacing.x =unit(0.5, 'cm'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))  +
  guides(fill = guide_colorbar(label.position = "bottom",
                               barwidth = 8))  + scale_fill_distiller(na.value="grey90",
                                                                       name = "Averaged PM2.5")+
  ylab("Latitude") + xlab("Longitude")

gg14

ggsave(gg14, 
       filename = "C:\\...\\avg_pm25_quart.pdf",
       width = 10 , height = 5)




