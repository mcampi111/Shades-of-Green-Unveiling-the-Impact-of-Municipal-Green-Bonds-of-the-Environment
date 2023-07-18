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
  `dark grey`  = "#8c8c8c",
  `violet`= "#EE82EE")


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


######################
# DATA construction #
#####################

n_county = length(df_final_percounty)

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


#########
# PLOTs #
#########

#1) one value for all 10 years

county_gg<- map_data("county")
california <- subset(county_gg, region %in% c("california"))

california$COUNTY = sapply(1:dim(california)[1], function(i)california$subregion[i])

#base plot 
ggplot(data = california) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill= "yellow", color="red", alpha = 0.3)  + 
  coord_fixed(1.3)


toplot<- merge(california,
               df_means, 
               by="COUNTY", 
               all.x=T)


toplot <- toplot[order(toplot$order),]

gg0 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=tot_prcp_1 ))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(drsimonj_pal("cool")(10)),na.value="grey90",
                       name = "Averaged Total PRC")+
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
       filename = "C:\\...\\tot_prcp.pdf",
       width = 5 , height = 5)

dev.off()


gg1 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=temp_avg_1  ))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(drsimonj_pal("main")(10)),na.value="grey90",
                       name = "Averaged Temperature")+
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

gg1

ggsave(gg1, 
       filename = "C:\\...\\temp_avg.pdf",
       width = 5 , height = 5)

dev.off()


gg2 = ggplot(toplot, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=avg_vol_1  ))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(drsimonj_pal("mixed")(10)),na.value="grey90",
                       name = "Averaged Volatility")+
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

gg2

ggsave(gg2, 
       filename = "C:\\...\\vol_avg.pdf",
       width = 5 , height = 5)

dev.off()





prova0 = lapply(1:n_county, function(i) df_means_years[[i]][,-c(7,8,9,2,3)] )

for (i in 1:n_county) {
  
  prova0[[i]]$County = simpleCap(df_means_years[[i]]$COUNTY)
  prova0[[i]] =   prova0[[i]][,-1]
  
}


prova = melt(prova0)


ggbox = ggplot(prova , aes(x=County, y=value , color=County)) +
  geom_boxplot() + facet_wrap(~ variable, scales="free_y",
                              labeller = as_labeller(c("tot_prcp_1" = "Total Precipitation",
                                                       "temp_avg_1" = "Averaged Mean-Temperature",
                                                       "avg_vol_1" = "Averaged Volatility") ) ) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=9.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") +ylab("")  +xlab("County")

ggbox

ggsave(ggbox, 
       filename = "C:\\...\\box_clima.pdf")



#######################
#######################
##SEASONAL COMPONENT###
#######################
#######################


head(df_final_percounty2[[1]])

forecast::ggseasonplot(x = as.ts(df_final_percounty2[[1]]$tot_prcp))


df_final_percounty2[[5]] %>%
  mutate(date = yearmonth(YEARMODA)) %>%
  as_tsibble(index = YEARMODA, key = COUNTY) %>%
  gg_season(tot_prcp, alpha=0) + theme_bw() + geom_point(alpha=0.9) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ylab("Total Precipitation") + xlab("Years")  



df_final_percounty2[[5]] %>%
  mutate(date = yearmonth(YEARMODA)) %>%
  as_tsibble(index = YEARMODA, key = COUNTY) %>%
  gg_season(avg_vol, alpha=0) + theme_bw() + geom_point(alpha=0.9) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ylab("Averaged Volatility") + xlab("Years")  



df_final_percounty2[[5]] %>%
  mutate(date = yearmonth(YEARMODA)) %>%
  as_tsibble(index = YEARMODA, key = COUNTY) %>%
  gg_season(temp_avg, alpha=0) + theme_bw() + geom_point(alpha=0.9) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ylab("Averaged Temperature") + xlab("Years")  


###############
# By Quarters #
###############

#1 county
df_final_percounty2[[1]]%>%
  mutate(date = yearmonth(YEARMODA), 
         Quarter = yearquarter(YEARMODA)) %>%
  group_by(Quarter) %>%
  mutate_if(is.numeric, mean) %>%
  mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
  distinct() %>%
  filter(row_number()==1)%>%
  as_tsibble(index = Quarter)%>%
  gg_subseries(tot_prcp)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Total Precipitation")  + 
  xlab("Years")



#ALL COUNTIES

df3 <- lapply(1:length(df_final_percounty2), function(i) df_final_percounty2[[i]]%>%
                       mutate(date = yearmonth(YEARMODA), 
                              Quarter = yearquarter(YEARMODA)) %>%
                       group_by(Quarter) %>%
                       mutate_if(is.numeric, mean) %>%
                       mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                       distinct() %>%
                       filter(row_number()==1))


subset_df4<- do.call(rbind, df3)


subset_df4 %>%
  as_tsibble(key = COUNTY,index = Quarter) %>%
  fill_gaps(.full = TRUE) %>%
  gg_subseries(temp_avg)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged Temperature")  + 
  xlab("Years")


subset_df4 %>%
  as_tsibble(key = COUNTY,index = Quarter)%>%
  fill_gaps(.full = TRUE) %>%
  gg_subseries(tot_prcp)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Total Precipitation")  + 
  xlab("Years")



subset_df4 %>%
  as_tsibble(key = COUNTY,index = Quarter) %>%
  fill_gaps(.full = TRUE) %>%
  gg_subseries(avg_vol)  + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged Volatility")  + 
  xlab("Years")




subset_df4_bis = subset_df4

subset_df4_bis$Quarter2 = sapply(str_split(subset_df4$Quarter, " "), "[[", 2)

subset_df4_bis %>% ggplot(aes(x= date , y=temp_avg)) +
  geom_line() + facet_grid(vars(Quarter2), vars(COUNTY), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged Temperature")  + 
  xlab("Years")


subset_df4_bis %>% ggplot(aes(x= date , y=tot_prcp )) +
  geom_line() + facet_grid(vars(Quarter2), vars(COUNTY), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged Total Precipitation")  + 
  xlab("Years")


subset_df4_bis %>% ggplot(aes(x= date , y=avg_vol )) +
  geom_line() + facet_grid(vars(Quarter2), vars(COUNTY), scales = "free") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("Averaged Volatility")  + 
  xlab("Years")



#####################
#boxplot by quarters#
#####################

prova_box_qrt = melt(subset_df4_bis[,-c(2,6,7,8,9,10,11)])

prova_box_qrt$County = sapply(1:dim(subset_df4_bis)[1], function(i) simpleCap(prova_box_qrt$COUNTY[i]))

ggbox_quart = ggplot(prova_box_qrt , aes(x=County, y=value , color=County)) +
  geom_boxplot() + facet_grid(variable~ Quarter2, scales="free",
                              labeller = labeller(variable = c("tot_prcp" = "Total Precipitation",
                                                               "temp_avg" = "Avg. Mean Temp.",
                                                               "avg_vol" = "Avg. Volatility") ) ) +
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
       filename = "C:\\...\\box_4_clima.pdf")



####################
#trial all together#
####################


subset_df4_bis %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg)), color = "chartreuse4") + 
  geom_line(aes(y = tot_prcp/max(tot_prcp) ), color = "dodgerblue") + 
  geom_line(aes(y = avg_vol/max(avg_vol) ), color="lightpink3") + 
  facet_grid(vars(Quarter2), vars(COUNTY), scales = "free") + theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90)) + ylab("")  + 
  xlab("Years")


load("C:\\...\\param_df_final_cali.RData")
load("C:\\...\\kpca_opt_index_cali_weath.RData")

param_df_final$County
names(kpca_index[[1]])

unique(subset_df4$COUNTY)

kpca_index_new = kpca_index

sapply(1:length(df_final_percounty), function(i) dim(df_final_percounty[[i]])[1])
sapply(1:length(df_final_percounty), function(i) length(kpca_index_new[[1]][[i]]) )




kpc1_df = lapply(1:length(df_final_percounty), function(i)
  data.frame(kpca1 = as.numeric(unlist(kpca_index_new[[1]][i])),
             County = df_final_percounty[[i]]$COUNTY, 
             Date = df_final_percounty[[i]]$YEARMODA) )



kpc1_df2 <- lapply(1:length(kpc1_df), function(i) kpc1_df[[i]]%>%
                     mutate(date = yearmonth(Date), 
                            Quarter = yearquarter(Date)) %>%
                     group_by(Quarter) %>%
                     mutate_if(is.numeric, mean) %>%
                     mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                     distinct() %>%
                     filter(row_number()==1))



kpc2_df = lapply(1:length(df_final_percounty), function(i)
  data.frame(kpca2 = as.numeric(unlist(kpca_index_new[[2]][i])),
             County = df_final_percounty[[i]]$COUNTY, 
             Date = df_final_percounty[[i]]$YEARMODA) )



kpc2_df2 <- lapply(1:length(kpc2_df), function(i) kpc2_df[[i]]%>%
                     mutate(date = yearmonth(Date), 
                            Quarter = yearquarter(Date)) %>%
                     group_by(Quarter) %>%
                     mutate_if(is.numeric, mean) %>%
                     mutate_if(is.character, funs(paste(unique(.), collapse = "_"))) %>%
                     distinct() %>%
                     filter(row_number()==1))



kpc3_df = lapply(1:length(df_final_percounty), function(i)
  data.frame(kpca3 = as.numeric(unlist(kpca_index_new[[3]][i])),
             County = df_final_percounty[[i]]$COUNTY, 
             Date = df_final_percounty[[i]]$YEARMODA) )




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





subset_df4_bis2 = subset_df4_bis

subset_df4_bis2$kpc1 = kpc1_df3$kpca1
subset_df4_bis2$kpc2 = kpc2_df3$kpca2
subset_df4_bis2$kpc3 = kpc3_df3$kpca3



#delete for consistency with the pollution dataset

todelete = c("madera", "marin", 
             "yolo", "stanislaus")

subset_df4_bis2 = subset_df4_bis2[!subset_df4_bis2$COUNTY %in% todelete, ]

subset_df4_bis2$County = sapply(1:dim(subset_df4_bis2)[1], function(i) simpleCap(subset_df4_bis2$COUNTY[i]))


colors_kpc1 = c("Temperature" = "chartreuse4", 
                "Precipitation" = "dodgerblue",
                "Volatility" = "lightpink3", 
                "KPC1" = "green")

gg_res_kpc1 = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg), color = "Temperature")) + 
  geom_line(aes(y = tot_prcp/max(tot_prcp), color = "Precipitation" )) + 
  geom_line(aes(y = avg_vol/max(avg_vol), color="Volatility" ) ) + 
  geom_line(aes(y = kpc1 , color="KPC1")) + 
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
       filename = "C:\\...\\kpc1_res_clim.pdf",
       width = 15 , height = 6)


colors_kpc2 = c("Temperature" = "chartreuse4", 
                "Precipitation" = "dodgerblue",
                "Volatility" = "lightpink3", 
                "KPC2" = "blue")

gg_res_kpc2 = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg), color = "Temperature")) + 
  geom_line(aes(y = tot_prcp/max(tot_prcp), color = "Precipitation" )) + 
  geom_line(aes(y = avg_vol/max(avg_vol), color="Volatility" )) + 
  geom_line(aes(y = kpc2 , color="KPC2")) + 
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
       filename = "C:\\...\\kpc2_res_clim.pdf",
       width = 15 , height = 6)


colors_kpc3 = c("Temperature" = "chartreuse4", 
                "Precipitation" = "dodgerblue",
                "Volatility" = "lightpink3", 
                "KPC3" = "purple")

gg_res_kpc3 = subset_df4_bis2 %>% ggplot(aes(x= date)) +
  geom_line(aes(y = temp_avg/max(temp_avg), color = "Temperature")) + 
  geom_line(aes(y = tot_prcp/max(tot_prcp), color = "Precipitation" )) + 
  geom_line(aes(y = avg_vol/max(avg_vol), color="Volatility" )) + 
  geom_line(aes(y = kpc3 , color="KPC3" )) + 
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
       filename = "C:\\...\\kpc3_res_clim.pdf",
       width = 15 , height = 6)


#only the kpcs

colors_kpcs = c( "KPC1" = "green",
                 "KPC2" = "blue",
                 "KPC3" = "violet")

gg_res_kpcs = subset_df4_bis2 %>% ggplot(aes(x= date)) +
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
       filename = "C:\\...\\kpcs_all_res_clim.pdf",
       width = 15 , height = 6)


saveRDS(subset_df4_bis2,
        file = "C:\\...\\subset_df4_bis2_weather.rds")




sub_box_melted = melt(subset_df4_bis2[,c(12:16)])
sub_box_melted = sub_box_melted  %>%
  mutate(colors = case_when(
    variable =="kpc1" ~ "green",
    variable == "kpc2" ~ "blue",
    variable == "kpc3" ~ "violet"
  ))

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
       filename = "C:\\...\\kpcs_all_boxplots_clim.pdf",
       width = 15 , height = 6)



##################################
# Maps according to the quarters # take the average of the Quarters across the years
##################################


df5 = df3

for (i in 1:length(df5)) {df5[[i]]$Quarter2 = quarter(df5[[i]]$Quarter) } 


df6 = lapply(1:length(df5), function(i) df5[[i]] %>% group_by(Quarter2)  %>%
                      mutate_if(is.numeric, mean) %>%
                      mutate_if(is.character,
                                funs(paste(unique(.), collapse = "_"))) %>%
                      distinct() %>%
                      filter(row_number()==1)  %>%
                      select(-year))

df7 = lapply(1:(dim(df6[[1]])[1]), function(j)
  data.frame( do.call(rbind,
                      lapply(1:length(df_means), function(i)
                        t(sapply(df6[[i]], `[[`, j)) ) ))  )


toplot_by_year2<- lapply(1:4, function(i)  
  merge(california,
        df7[[i]],  
        by="COUNTY", 
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
  geom_polygon(aes(fill=as.numeric(temp_avg  )   ))+
  geom_path()+ facet_wrap(~  Quarter3, ncol = 4,labeller= as_labeller(c("1" = "Q1", 
                                                                        "2" = "Q2",
                                                                        "3" = "Q3", 
                                                                        "4" = "Q4")) )+
  scale_fill_gradientn(colours=rev(drsimonj_pal("main")(10)),na.value="grey90",
                       name = "Averaged Temperature")+
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
       filename = "C:\\...\\avg_temp_quart.pdf",
       width = 10 , height = 5)




gg12 = ggplot(toplot_by_quarter_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(tot_prcp )   ))+
  geom_path()+ facet_wrap(~  Quarter3, ncol = 4,labeller= as_labeller(c("1" = "Q1", 
                                                                        "2" = "Q2",
                                                                        "3" = "Q3", 
                                                                        "4" = "Q4")) )+
  scale_fill_gradientn(colours=rev(drsimonj_pal("cool")(20)),na.value="grey90",
                       name = "Averaged Total Precipitation")+
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
       filename = "C:\\...\\tot_precp_quart.pdf",
       width = 10 , height = 5)



gg13 = ggplot(toplot_by_quarter_final, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.numeric(avg_vol )   ))+
  geom_path()+ facet_wrap(~  Quarter3, ncol = 4,labeller= as_labeller(c("1" = "Q1", 
                                                                        "2" = "Q2",
                                                                        "3" = "Q3", 
                                                                        "4" = "Q4")) )+
  scale_fill_gradientn(colours=rev(drsimonj_pal("mixed")(10)),na.value="grey90",
                       name = "Averaged Volaitlity")+
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

gg13

ggsave(gg13, 
       filename = "C:\\...\\avg_vol_quart.pdf",
       width = 10 , height = 5)

