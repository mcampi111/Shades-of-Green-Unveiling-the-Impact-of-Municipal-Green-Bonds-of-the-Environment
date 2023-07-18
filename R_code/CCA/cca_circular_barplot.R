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

load("C:\\....\\cca_gbweath_gbpoll.RData")



#############
# FUNCTIONS #
#############

df_plot <- function(cca, kpc_comp, ncc, string_cca1, string_cca2, empty_bar){
  
  df = rbind(cca[[kpc_comp]]$xstructcorr, cca[[kpc_comp]]$ystructcorr)
  
  df1 = lapply(1:ncc, function(i) as.data.frame(rbind(as.matrix(df[1:9,i]),
                                                      as.matrix(df[18:10,i]))))
  
  for (i in 1: ncc) {
    colnames(df1[[i]]) = paste('CV', i , sep = "") 
    df1[[i]]$group = c(rep(string_cca1, nrow(df1[[i]])/2), 
                       rep(string_cca2, nrow(df1[[i]])/2))
    df1[[i]]$city =  c(row.names(df)[1:9], row.names(df)[18:10])
    df1[[i]]$city[18:10] = c( "Alameda" ,
                              "Los Angeles",
                              "Santa Cruz",
                              "San Diego",
                              "San Luis Obispo",
                              "San Joaquin",
                              "San Francisco",        
                              "Santa Clara",
                              "Napa")
    df1[[i]]$group2 = c(ifelse(df1[[i]][1:9,1] > 0 , "1", "2"),
                      ifelse(df1[[i]][10:18,1] > 0 , "3", "4"))
    
    df1[[i]] = rbind(df1[[i]][1:9,],arrange(df1[[i]][18:10,], desc(city)))
    
    
  }
  

  to_add <- lapply(1:ncc, function(i)
    data.frame( matrix(NA, 
                       empty_bar*nlevels(as.factor(df1[[i]]$group)), 
                       ncol(df1[[i]])) ))
  for (i in 1: ncc) {
  colnames(to_add[[i]]) <- colnames(df1[[i]])
  to_add[[i]]$group <- rep(levels(as.factor(df1[[i]]$group)), each=empty_bar)
  
  df1[[i]] <- rbind(df1[[i]], to_add[[i]])
  #df1[[i]] <- df1[[i]] %>% arrange(group)
  df1[[i]]$id <- seq(1, nrow(df1[[i]]))
  
  }
  
  
  label_df1<- lapply(1:ncc, function(i) df1[[i]])
  number_of_bar <- sapply(1:ncc, function(i) nrow(label_df1[[i]]))
  angle <-  lapply(1:ncc, function(i) 90 - 360 * (label_df1[[i]]$id-0.5) / (number_of_bar[i])  )
  
  for (i in 1: ncc) {
    label_df1[[i]]$hjust<-ifelse( angle[[i]] < -90, 1, 0)
    label_df1[[i]]$angle<-ifelse(angle[[i]] < -90, angle[[i]]+180, angle[[i]])
  }


  base_data <- lapply(1:ncc, function(i)
      df1[[i]] %>%
      group_by(group) %>%
      summarize(start=min(id), end=max(id) - empty_bar) %>%
      rowwise() %>%
      mutate(title=mean(c(start, end)))  )

  grid_data <- base_data

  for (i in 1:ncc) {
    grid_data[[i]]$end <- grid_data[[i]]$end[ c( nrow(grid_data[[i]]), 1:nrow(grid_data[[i]] )-1)] + 1
    grid_data[[i]]$start <- grid_data[[i]]$start - 1
    grid_data[[i]]  <- grid_data[[i]][-1,]
  }
  
  final_list = list(df1,label_df1,base_data,grid_data)
  
  return(final_list)
  
}


###############################
# GENERATING AND SAVING PLOTS #
###############################


################
# GB - WEATHER #
################

helio_gbweath = df_plot(cca_gb_weath, 1, 3, "GB", "Climate", 0 )

# saveRDS(helio_gbweath,
#         file = "C:\\Users\\mcampi\\Desktop\\CCA_Cali\\helio_gbweath_kpc2.rds")

gg =  vector(mode = "list", 3) 

#remember to change the kpca number in the loop

for (i in 1:3) {
  
gg1 =    ggplot(helio_gbweath[[1]][[i]], aes(x = as.factor(id),
                                             y = get(paste("CV", i, sep = "")), 
                                             fill = group2)) +  
         geom_bar(aes(x=as.factor(id), 
                      y = get(paste("CV", i, sep = "")),
                      fill=group2),
                  stat="identity",
                  alpha=0.7) +  
         geom_label(aes(x = id,
                        label = round(get(paste("CV", i, sep = "")), digits = 1),
                        y = ifelse(get(paste("CV", i, sep = "")) > 0, 
                                   get(paste("CV", i, sep = "")) + 0.1,
                                    get(paste("CV", i, sep = "")) + 0.7 )), 
                    position = "stack",
                    color = "white", 
                    angle= helio_gbweath[[2]][[i]]$angle,
                    show.legend = FALSE) + 
         scale_fill_manual(values=c("1" = "green",
                                    "3" = "skyblue", 
                                    "2" ="darkgreen",
                                     "4" ="blue")) +
  
         ylim(-2.4,1.7) +
         theme_minimal() +
         theme( legend.position = "none", axis.text = element_blank(),
                axis.title = element_blank(),panel.grid = element_blank(),
                plot.margin = unit(rep(-1,4), "cm") )  +
        coord_polar(start = 0) + 
        geom_text(data=helio_gbweath[[2]][[i]], 
                  aes(x = id, 
                  y = ifelse(get(paste("CV", i, sep = "")) > 0, 
                             get(paste("CV", i, sep = "")) + 0.3, 
                             get(paste("CV", i, sep = "")) + 1 ),
                  label = city,
                  hjust = hjust), 
                  color="black", 
                  fontface="bold",
                  alpha=0.6, 
                  size=3.5, 
                  angle= helio_gbweath[[2]][[i]]$angle,
                  inherit.aes = FALSE ) +
       geom_segment(data=helio_gbweath[[3]][[i]], 
                    aes(x = start, y = -1, xend = end, yend = -1), 
                     colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
       geom_text(data=helio_gbweath[[3]][[i]],
                 aes(x = title, y = -1.4, label=group), 
                 hjust=c(0,2), colour = "black",
                 alpha=0.8, size=4, 
                 fontface="bold", inherit.aes = FALSE)

print(i)
print(gg1)

ggsave(gg1,
       filename=paste("C:\\Users\\mcampi\\Desktop\\CCA_Cali\\figs\\cca_gb_weath_kpc1_cv",
                      i,".pdf",sep=""), width = 7, height = 7)

gg[[i]] = gg1

rm(gg1)

}

#aes is a function that depends on i and therefore when printing afterwards it messes
#up - therefore I directly save them inside the loop 

i = 1
gg[[1]]
i = 2
gg[[2]] 
i = 3
gg[[3]] 


# to do the following you need to reproduce them one by one (I removed the code
# since taking a lot of space)




# gg_combined = ggarrange(gg1, gg2,gg3 ,
#                         labels = c("CV1", "CV2", "CV3"),
#                         ncol = 2, nrow = 2,  align = "v") 
# 

# ggsave(gg_combined,
#        filename="C:\\Users\\mcampi\\Desktop\\CCA_Cali\\figs\\gb_weath_kpc1_allcv.pdf",
#        width = 12,
#        height = 12)

ggsave(gg_combined,
       filename="C:\\Users\\mcampi\\Desktop\\CCA_Cali\\figs\\gb_weath_kpc2_allcv.pdf",
       width = 12,
       height = 12)




##############
# GB - POLL #
#############

helio_gbpoll = df_plot(cca_gb_poll, 2, 3, "GB", "Pollution", 0 )

# saveRDS(helio_gbpoll,
#         file = "C:\\Users\\mcampi\\Desktop\\CCA_Cali\\helio_gbpoll_kpc2.rds")



gg =  vector(mode = "list", 3) 

#remember to change the kpca number in the loop

for (i in 1:3) {
  
  gg1 =    ggplot(helio_gbpoll[[2]][[i]], aes(x = as.factor(id),
                                               y = get(paste("CV", i, sep = "")), 
                                               fill = group2)) +  
    geom_bar(aes(x=as.factor(id), 
                 y = get(paste("CV", i, sep = "")),
                 fill=group2),
             stat="identity",
             alpha=0.7) +  
    geom_label(aes(x = id,
                   label = round(get(paste("CV", i, sep = "")), digits = 1),
                   y = ifelse(get(paste("CV", i, sep = "")) > 0, 
                              get(paste("CV", i, sep = "")) + 0.1,
                              get(paste("CV", i, sep = "")) + 0.7 )), 
               position = "stack",
               color = "black", 
               angle= helio_gbpoll[[2]][[i]]$angle,
               show.legend = FALSE) + 
    scale_fill_manual(values=c("1" = "green",
                               "3" = "yellow", 
                               "2" ="darkgreen",
                               "4" ="goldenrod")) +
    
    ylim(-2.4,1.7) +
    theme_minimal() +
    theme( legend.position = "none", axis.text = element_blank(),
           axis.title = element_blank(),panel.grid = element_blank(),
           plot.margin = unit(rep(-1,4), "cm") )  +
    coord_polar(start = 0) + 
    geom_text(data=helio_gbpoll[[2]][[i]], 
              aes(x = id, 
                  y = ifelse(get(paste("CV", i, sep = "")) > 0, 
                             get(paste("CV", i, sep = "")) + 0.3, 
                             get(paste("CV", i, sep = "")) + 1 ),
                  label = city,
                  hjust = hjust), 
              color="black", 
              fontface="bold",
              alpha=0.6, 
              size=3.5, 
              angle= helio_gbpoll[[2]][[i]]$angle,
              inherit.aes = FALSE ) +
    geom_segment(data=helio_gbpoll[[3]][[i]], 
                 aes(x = start, y = -1, xend = end, yend = -1), 
                 colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=helio_gbpoll[[3]][[i]],
              aes(x = title, y = -1.4, label=group), 
              hjust=c(2,0), colour = "black",
              alpha=0.8, size=4, 
              fontface="bold", inherit.aes = FALSE)
  
  print(i)
  print(gg1)
  
  ggsave(gg1,
         filename=paste("C:\\...\\cca_gb_poll_kpc2_cv",
                        i,".pdf",sep=""),  width = 7, height = 7)
  
  gg[[i]] = gg1
  
  rm(gg1)
  
}

#aes is a function that depends on i and therefore when printing afterwards it messes
#up - therefore I directly save them inside the loop 

i = 1
gg[[1]]
i = 2
gg[[2]] 
i = 3
gg[[3]] 


# to do the following you need to reproduce them one by one (I removed the code
# since taking a lot of space)




gg_combined = ggarrange(gg1, gg2,gg3 ,
                        labels = c("CV1", "CV2", "CV3"),
                        ncol = 2, nrow = 2,  align = "v") 


# ggsave(gg_combined,
#        filename="C:\\...\\gb_poll_kpc1_allcv.pdf",
#        width = 12,
#        height = 12)

ggsave(gg_combined,
       filename="C:\\...\\gb_poll_kpc2_allcv.pdf",
       width = 12,
       height = 12)




