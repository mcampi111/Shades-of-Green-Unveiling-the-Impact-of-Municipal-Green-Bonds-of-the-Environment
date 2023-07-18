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


load("C:/...\\all_join_spline_sc_cali.RData")
load("C:/...\\California\\pcs_poll.RData")
load("C:\\...\\param_df_final_cali.RData")
load("C:\\...\\kpca_opt_index_cali_poll.RData")


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



#remove Madera
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



#remove Madera from each list
param_df_final$County
names(kpca_index[[1]])
kpca_index_new = lapply(1:length(kpca_index), function(i) kpca_index[[i]][-6] )
sapply(1:length(kpca_index), function(i) length(kpca_index_new[[i]]))


#other check
sapply(1:length(subset_df2), function(i) dim(subset_df2[[i]])[1])
sapply(1:length(subset_df), function(i) length(kpca_index_new[[1]][[i]]) )

names(kpca_index_new[[1]])


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



subset_df4_bis2 = subset_df4_bis

subset_df4_bis2$pc1 = pc1_df3$pca1
subset_df4_bis2$pc2 = pc2_df3$pca2
subset_df4_bis2$pc3 = pc3_df3$pca3
subset_df4_bis2$kpc1 = kpc1_df3$kpca1
subset_df4_bis2$kpc2 = kpc2_df3$kpca2
subset_df4_bis2$kpc3 = kpc3_df3$kpca3


#PLOTS MATRICES - only for the quarters means 

unique(subset_df4_bis2$County)

cov_raw = t(subset_df4_bis2 %>% 
          filter(County == "Alameda") %>% 
          ungroup() %>%
          select(c(3:6)) )

cov_raw2 = t(cov_raw)  %*% cov_raw


heatmap.2(cov_raw2, 
          dendrogram = "none",
          col =  rev(hcl.colors(20*10, "plasma", rev = TRUE)), 
          #Rowv=FALSE, Colv = FALSE, 
          labRow = FALSE, labCol = FALSE,
          margins=c(9,3), # ("margin.Y", "margin.X")
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
          cexRow = 1.7, cexCol = 1.7)


cov_raw3 = cov_raw2 %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = 1:40),
    Var2 = factor(gsub("V", "", Var2), levels = 1:40))

ggplot(cov_raw3, aes(Var1, Var2)) +
  geom_tile(aes(fill = value))  +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")



cov_pcs = t(subset_df4_bis2 %>% 
              filter(County == "Alameda") %>% 
              ungroup() %>%
              select(c(11:13)) )

cov_pcs2 = t(cov_pcs)  %*% cov_pcs
heatmap.2(cov_pcs2, 
          dendrogram = "none",
          col =  rev(hcl.colors(20*10, "plasma", rev = TRUE)), 
          #Rowv=FALSE, Colv = FALSE, 
          labRow = FALSE, labCol = FALSE,
          margins=c(9,3), # ("margin.Y", "margin.X")
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
          cexRow = 1.7, cexCol = 1.7)



cov_pcs3 = cov_pcs2 %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = 1:40),
    Var2 = factor(gsub("V", "", Var2), levels = 1:40))

ggplot(cov_pcs3, aes(Var1, Var2)) +
  geom_tile(aes(fill = value))  +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")



cov_kpcs = t(subset_df4_bis2 %>% 
              filter(County == "San Diego") %>% 
              ungroup() %>%
              select(c(14:16)) )

cov_kpcs2 = t(cov_kpcs)  %*% cov_kpcs
heatmap.2(cov_kpcs2, 
          dendrogram = "none",
          col =  rev(hcl.colors(20*10, "plasma", rev = TRUE)), 
          #Rowv=FALSE, Colv = FALSE, 
          labRow = FALSE, labCol = FALSE,
          margins=c(9,3), # ("margin.Y", "margin.X")
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
          cexRow = 1.7, cexCol = 1.7)



cov_kpcs3 = cov_kpcs2 %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = 1:40),
    Var2 = factor(gsub("V", "", Var2), levels = 1:40))

ggplot(cov_kpcs3, aes(Var1, Var2)) +
  geom_tile(aes(fill = value))  +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")


#all counties

mat_raw_all = lapply(1:length(subset_df3), function(i)
                                     t(subset_df3[[i]]  %>%
                                         ungroup() %>%  select(c(3:6)) ))


mat_raw_all2 = lapply(1:length(subset_df3), function(i) 
                                    t(mat_raw_all[[i]]) %*% mat_raw_all[[i]] )


mat_raw_all3 = lapply(1:length(subset_df3), function(i) 
                                  mat_raw_all2[[i]] %>%
                                      as_tibble() %>%
                                        rownames_to_column("Var1") %>%
                                           pivot_longer(-Var1, 
                                                        names_to = "Var2",
                                                        values_to = "value") %>%
                                        mutate(Var1 = factor(Var1, levels = 1:40),
                                               Var2 = factor(gsub("V", "", Var2), levels = 1:40)) )

for (i in 1:length(subset_df3)) {
  
  mat_raw_all3[[i]]$County = unique(subset_df3[[i]]$County)
  
}


for (i in 1:length(subset_df3)) {
  
  mat_raw_all3[[i]]$value_scaled = scale(mat_raw_all3[[i]]$value)
  
}



mat_raw_all4 = do.call(rbind, mat_raw_all3)
mat_raw_all4bis = mat_raw_all4 %>% 
  filter(!str_detect(County, 'Solano'))  #fix this
  
ggplot(mat_raw_all4bis, aes(Var1, Var2)) +
  geom_tile(aes(fill = value_scaled))  + facet_wrap(~County, scales = "free") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom")



#do the same for the others i.e. pcs and kpcs by concatenating the kpc1_df2, .. and pc1_df2 ..

################
# COMPUTE CKTA #
################ 

head(subset_df2[[1]])
head(pc1_df[[1]])
head(kpc1_df[[1]])
#joint subsetdf2 with pcs and kpcs as for the quartes


subset_df2_bis = subset_df2

for (i in 1:length(subset_df2_bis)) {
  
  subset_df2_bis[[i]]$pc1 = pc1_df[[i]]$pca1
  subset_df2_bis[[i]]$pc2 = pc2_df[[i]]$pca2
  subset_df2_bis[[i]]$pc3 = pc3_df[[i]]$pca3
  subset_df2_bis[[i]]$kpc1 = kpc1_df[[i]]$kpca1
  subset_df2_bis[[i]]$kpc2 = kpc2_df[[i]]$kpca2
  subset_df2_bis[[i]]$kpc3 = kpc3_df[[i]]$kpca3
  
}

K_Kraw = lapply(1:length(subset_df2_bis), function(i)
           as.matrix(subset_df2_bis[[i]][,c(3:6)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(3:6)])))

K_Pc1 =  lapply(1:length(subset_df2_bis), function(i)
              as.matrix(subset_df2_bis[[i]][,c(8)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(8)])))

K_Pc2 = lapply(1:length(subset_df2_bis), function(i)
             as.matrix(subset_df2_bis[[i]][,c(8,9)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(8,9)])))
  
K_Pc3 =  lapply(1:length(subset_df2_bis), function(i)
             as.matrix(subset_df2_bis[[i]][,c(8,9,10)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(8,9,10)])))


K_kPc1 =  lapply(1:length(subset_df2_bis), function(i)
  as.matrix(subset_df2_bis[[i]][,c(11)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(11)])))

K_kPc2 = lapply(1:length(subset_df2_bis), function(i)
  as.matrix(subset_df2_bis[[i]][,c(11,12)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(11,12)])))

K_kPc3 =  lapply(1:length(subset_df2_bis), function(i)
  as.matrix(subset_df2_bis[[i]][,c(11,12,13)]) %*% as.matrix(t(subset_df2_bis[[i]][,c(11,12,13)])))


ckta_pc1 = lapply(1:length(subset_df2_bis), function(i)
                                              new_ckta(K_Kraw[[i]],
                                                       K_Pc1[[i]]))


save(ckta_pc1, file = "C:\\...\\ckta_pc1.RData")

ckta_pc2 = lapply(1:length(subset_df2_bis), function(i)
                                           new_ckta(K_Kraw[[i]],
                                                    K_Pc2[[i]]))

save(ckta_pc2, file = "C:\\...\\ckta_pc2.RData")


ckta_pc3 = lapply(1:length(subset_df2_bis), function(i)
                                                new_ckta(K_Kraw[[i]],
                                                         K_Pc3[[i]]))

save(ckta_pc3, file = "C:\\...\\ckta_pc3.RData")


ckta_kpc1 = lapply(1:length(subset_df2_bis), function(i)
                                                 new_ckta(K_Kraw[[i]],
                                                          K_kPc1[[i]]))
save(ckta_kpc1, file = "C:\\...\\ckta_kpc1.RData")


ckta_kpc2 = lapply(1:length(subset_df2_bis), function(i)
                                               new_ckta(K_Kraw[[i]],
                                                        K_kPc2[[i]]))
save(ckta_kpc2, file = "C:\\...\\ckta_kpc2.RData")


ckta_kpc3 = lapply(1:length(subset_df2_bis), function(i)
                                               new_ckta(K_Kraw[[i]],
                                                        K_kPc3[[i]]))
save(ckta_kpc3, file = "C:\\...\\ckta_kpc3.RData")




res = data.frame( "pc1" = round(do.call(rbind,ckta_pc1), digits = 3),
            "pc1-pc2" =round(do.call(rbind,ckta_pc2), digits = 3),
            "pc1-pc2-pc3" =round(do.call(rbind,ckta_pc3), digits = 3),
            "kpc1" =round(do.call(rbind,ckta_kpc1), digits = 3),
            "kpc1-kpc2" =round(do.call(rbind,ckta_kpc2), digits = 3),
            "kpc1-kpc2-kpc3" =round(do.call(rbind,ckta_kpc3), digits = 3) )



rownames(res) = unique(subset_df4$County)

kable(res, format = "latex")

