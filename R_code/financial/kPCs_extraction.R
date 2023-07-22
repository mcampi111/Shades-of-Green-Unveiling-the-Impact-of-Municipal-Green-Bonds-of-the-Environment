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
library(useful)
library(matrixStats)
library(readxl)
library(gplots) 
library(ggpubr)


# gb_data<- read_excel("C:\\;;;\\Muni_green.xlsx", 
#                      sheet = "HardCode")   not available!!!

##########################################
#  Split the original by state for PLOTS #
##########################################

gb_data_state_v0 <- gb_data %>% group_split(State) 

######################################
#     Number of Issuers per State    #
######################################


n_issuer<- sapply(1:length(gb_data_state_v0), function(i) length(unique(gb_data_state_v0[[i]]$`Issuer Name`) ) )
state_gb_v0<- sapply(1:length(gb_data_state_v0), function(i) unique(gb_data_state_v0[[i]]$State ))

df_issuer<- data.frame(N_issuer = n_issuer,  States = state_gb_v0 )


gg0 = ggplot(data = df_issuer, aes(x=States, y=N_issuer, fill=States)) +
  geom_bar(stat="identity")+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, size = 40),
        axis.title.x = element_text(size = 55),
        axis.title.y = element_text(size = 55),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Number of Issuers")+
  xlab("US States") +
  geom_text(aes(label=N_issuer), 
            position=position_dodge(width=0.7),
            vjust=-0.25,  color="blue", size=16)
gg0

ggsave("C:\\...\\figs\\green_hist_stations_us.pdf",
       gg0, width = 23 , height = 15)
dev.off()


n_issuer_cali = gb_data_state_v0[[4]] %>% group_by(COUNTY_LOCATION_ISSUER)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')

n_issuer_cali = n_issuer_cali[-1,]
n_issuer_cali = n_issuer_cali %>%  mutate(
  County = str_split(COUNTY_LOCATION_ISSUER, "\\,", simplify=T)[,1])

n_issuer_cali = n_issuer_cali[,-1]
n_issuer_cali

county_cali<- unique(gb_data_state_v0[[4]]$COUNTY_LOCATION_ISSUER )
county_cali<- county_cali[-7]
county_cali = str_split(county_cali, "\\,", simplify=T)[,1]
county_cali



gg = ggplot(data = n_issuer_cali, aes(x=County, y=total_count, fill=County)) +
  geom_bar(stat="identity")+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, size = 50),
        axis.title.x = element_text(size = 65),
        axis.title.y = element_text(size = 65),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Number of Issued Bonds")+
  xlab("California Counties") +
  geom_text(aes(label=total_count), 
            position=position_dodge(width=0.7),
            vjust=-0.25,  color="blue", size=16)

gg

ggsave("C:\\...\\green_hist_stations.pdf",
       gg, width = 20 , height = 18)
dev.off()


#####################
# Descriptive Plots #
#####################


toplot_boxplots = gb_data_state_v0[[4]]
toplot_boxplots$County = str_split(toplot_boxplots$COUNTY_LOCATION_ISSUER, "\\,", simplify=T)[,1]

toplot_boxplots2 = toplot_boxplots %>%
  filter(str_detect(County, 'Santa Clara|San Diego|Alameda|Los Angeles|San Joaquin|Santa Cruz|Napa|San Luis Obispo|San Francisco'))

toplot_boxplots3 = toplot_boxplots2[,c(4,7,8,13:16,21,28)]

melted_boxbplot = melt(toplot_boxplots3, id = "County")


gg_box = ggplot(melted_boxbplot , aes(x=County, y=as.numeric(value) , color=County)) +
  geom_boxplot() + facet_wrap(~variable, scales = "free_y",
                              labeller = as_labeller(c("Cpn" = "Coupon",
                                                       "Mty Size" = "Maturity Size",
                                                       "Amt Out" = "Amount Out",
                                                       "OAS_SPREAD_BID" = "Bid OAS",
                                                       "DUR_MID" = "Mid Mac Duration",
                                                       "ISSUE_PX" = "Issue Price",
                                                       "YIELD_ON_ISSUE_DATE" = "Yield Issue Date",
                                                       "SPREAD_AT_ISSUANCE_TO_WORST" = "Spread at Issuance to Worst"))) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=9.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") +ylab("")


gg_box

ggsave(gg_box,
       filename="C:\\...\\ggbox_finance.pdf",
       width = 7,
       height = 7)



toplotbar1 = gb_data_state_v0[[4]] %>% group_by(MUNI_ISSUE_TYP)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')



toplotbar2 =  gb_data_state_v0[[4]] %>% group_by(ISSUER_INDUSTRY)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')


toplotbar3 = gb_data_state_v0[[4]] %>% group_by(MUNI_SOURCE)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')

toplotbar3 = toplotbar3[-1,]

toplotbar4 = gb_data_state_v0[[4]] %>% group_by(RSK_BB_ISS_5Y_DEFAULT_RISK)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')

toplotbar4 = toplotbar4[-1,]



###########################
# California all together #
###########################

bar1 = ggplot(data = toplotbar1, aes(x=MUNI_ISSUE_TYP, y=total_count, fill="steelblue")) +
  geom_bar(stat="identity",fill="steelblue")+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, 
                                                             size = 8,
                                                             vjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Muni Issue Type") +
  geom_text(aes(label=total_count), 
            vjust=0,  color="blue", size=7)
bar1

ggsave(bar1,
       filename="C:\\...\\figs\\bar1.pdf",
       width = 7,
       height = 7)




bar2 = ggplot(data = toplotbar2, aes(x=ISSUER_INDUSTRY , y=total_count, fill="darkgreen" )) +
  geom_bar(stat="identity",fill="darkgreen" )+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, 
                                                             size = 8,
                                                             vjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Issuer Industry") +
  geom_text(aes(label=total_count),
            vjust=0,  color="blue", size=7)
bar2


ggsave(bar2,
       filename="C:\\...\\bar2.pdf",
       width = 7,
       height = 7)


bar3 = ggplot(data = toplotbar3, aes(x=MUNI_SOURCE , y=total_count, fill="gold" )) +
  geom_bar(stat="identity",fill="gold")+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, 
                                                             size = 8,
                                                             vjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Muni Source") +
  geom_text(aes(label=total_count),
            vjust=0,  color="blue", size=7)

bar3


ggsave(bar3,
       filename="C:\\...\\bar3.pdf",
       width = 7,
       height = 7)


bar4 = ggplot(data = toplotbar4, aes(x=RSK_BB_ISS_5Y_DEFAULT_RISK  , y=total_count, fill="violet"  )) +
  geom_bar(stat="identity", fill = "violet")+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, 
                                                             size = 8,
                                                             vjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Risk BB Issues 5year") +
  geom_text(aes(label=total_count), 
            vjust=0,  color="blue", size=7)
bar4

ggsave(bar4,
       filename="C:\\...\\bar4.pdf",
       width = 7,
       height = 7)




#############
# by county #
#############

sapply(1:length(gb_data_state_v0), function(i) unique(gb_data_state_v0[[i]]$COUNTY_LOCATION_ISSUER))

my_gb_cali = gb_data_state_v0[[4]]

saveRDS(my_gb_cali,
        file = "C:\\...\\my_gb_cali.rds")

toplotbar11 = my_gb_cali %>% group_by(MUNI_ISSUE_TYP,COUNTY_LOCATION_ISSUER)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')
toplotbar11 = toplotbar11[-c(8,25),]
toplotbar11$County = sapply(str_split(toplotbar11$COUNTY_LOCATION_ISSUER, ","), "[[", 1)

toplotbar11 = toplotbar11[-c(9, 10, 12, 13, 15, 20, 22),]


bar11 = ggplot(data = toplotbar11, aes(x=MUNI_ISSUE_TYP, y=total_count, fill="steelblue")) +
  geom_bar(stat="identity",fill="steelblue")+ facet_wrap(~County) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 65, 
                                                             size = 7,
                                                             hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Muni Issue Type") 

bar11

ggsave(bar11,
       filename="C:\\..\\bar1_county.pdf",
       width = 7,
       height = 7)




toplotbar22 = my_gb_cali %>% group_by(ISSUER_INDUSTRY,COUNTY_LOCATION_ISSUER)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')
toplotbar22 = toplotbar22[-c(1,24),]
toplotbar22$County = sapply(str_split(toplotbar22$COUNTY_LOCATION_ISSUER, ","), "[[", 1)

toplotbar22 = toplotbar22[-c(3,4,5,14,16,17,23,25),]


bar22 = ggplot(data = toplotbar22, aes(x=ISSUER_INDUSTRY, y=total_count, fill="darkgreen")) +
  geom_bar(stat="identity",fill="darkgreen")+ facet_wrap(~County) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 65, 
                                                             size = 7,
                                                             hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Issuer Industry") #+ geom_text(aes(label=total_count), vjust=0,  color="blue", size=5)


bar22

ggsave(bar22,
       filename="C:\\..\\bar2_county.pdf",
       width = 7,
       height = 7)




toplotbar33 = my_gb_cali %>% group_by(MUNI_SOURCE,COUNTY_LOCATION_ISSUER)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')
toplotbar33 = toplotbar33[-c(1,7),]
toplotbar33$County = sapply(str_split(toplotbar33$COUNTY_LOCATION_ISSUER, ","), "[[", 1)

toplotbar33 = toplotbar33[-c(5,7,14,17,20,24,25,26,27,28),]


bar33 = ggplot(data = toplotbar33, aes(x=MUNI_SOURCE, y=total_count, fill="gold")) +
  geom_bar(stat="identity",fill="gold")+ facet_wrap(~County) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 65, 
                                                             size = 7,
                                                             hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Muni Source") #+ geom_text(aes(label=total_count), vjust=0,  color="blue", size=5)


bar33

ggsave(bar33,
       filename="C:\\..\\bar3_county.pdf",
       width = 7,
       height = 7)



toplotbar44 = my_gb_cali %>% group_by(RSK_BB_ISS_5Y_DEFAULT_RISK,COUNTY_LOCATION_ISSUER)  %>% 
  summarise(total_count=n(),
            .groups = 'drop')
toplotbar44 = toplotbar44[-c(1,12),]
toplotbar44$County = sapply(str_split(toplotbar44$COUNTY_LOCATION_ISSUER, ","), "[[", 1)

toplotbar44 = toplotbar44[-c(1,2,5,7,13,16,18,23,24,25),]


bar44 = ggplot(data = toplotbar44, aes(x=RSK_BB_ISS_5Y_DEFAULT_RISK, y=total_count, fill="violet")) +
  geom_bar(stat="identity",fill="violet")+ facet_wrap(~County) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 65, 
                                                             size = 7,
                                                             hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Issued Bonds")+
  xlab("Risk BB Issues 5year") #+ geom_text(aes(label=total_count), vjust=0,  color="blue", size=5)


bar44

ggsave(bar44,
       filename="C:\\..\\bar4_county.pdf",
       width = 7,
       height = 7)



####################################
#  Select California, Texas and NY #
####################################


my_gb_data_state<- list(gb_data_state_v0[[4]], gb_data_state_v0[[26]], gb_data_state_v0[[32]])

n_state<- length(my_gb_data_state)


################
#Missing values#  
################

vis_miss(my_gb_data_state[[1]]) 

vis_miss(my_gb_data_state[[2]]) 

vis_miss(my_gb_data_state[[3]]) 


###################################################
# Put them back into a list to apply the encoding #
###################################################

my_gb_data<- do.call(rbind, my_gb_data_state)


###################################################
#            RECHECK MISSING VALUES               #
###################################################

my_gb_data_miss<- my_gb_data %>% replace_with_na_all(condition = ~.x == "#N/A N/A")

vis_miss(my_gb_data_miss)


names_to_remove<- c('RTG_SP_LONG',
                    'RTG_FITCH_LONG',
                    'BB_COMPOSITE',
                    'RTG_FITCH',
                    'RTG_FITCH_INITIAL',
                    'RTG_SP',
                    "RTG_SP_INITIAL")


my_gb_data_miss_reduced<- my_gb_data_miss[ , -which(names(my_gb_data_miss) %in% names_to_remove)]

vis_miss(my_gb_data_miss_reduced)

my_gb_data_miss_reduced_2 = my_gb_data_miss_reduced %>% drop_na()

vis_miss(my_gb_data_miss_reduced_2)


names(my_gb_data_miss_reduced_2)

###########################################
#              Encoding                   # Note that day are encoded in euclidean
###########################################

my_gb_data_new <- my_gb_data_miss_reduced_2 %>% mutate( Cpn2= as.numeric(Cpn),
                                              MUNI_ISSUE_TYP_2 = as.numeric(as.factor(MUNI_ISSUE_TYP)),
                                              ISSUER_INDUSTRY_2 = as.numeric(as.factor(ISSUER_INDUSTRY)),
                                              MUNI_SOURCE_2 = as.numeric(as.factor(MUNI_SOURCE)),
                                              MUNI_OFFERING_TYP_2 = as.numeric(as.factor(MUNI_OFFERING_TYP)),
                                              RSK_BB_ISS_5Y_DEFAULT_RISK_2 = as.numeric(as.factor(RSK_BB_ISS_5Y_DEFAULT_RISK)),
                                              Maturity_Enc = as.numeric(- (min(as.Date(  Maturity, format =  "%d/%m/%Y")) -(as.Date(  Maturity, format =  "%d/%m/%Y")))),
                                              Dated_Date_Enc = as.numeric(- (min(as.Date(  `Dated Date`, format =  "%d/%m/%Y")) -(as.Date(  `Dated Date`,format =  "%d/%m/%Y")))),
                                              OAS_SPREAD_BID_2 = as.numeric(OAS_SPREAD_BID),
                                              DUR_MID_2 = as.numeric(DUR_MID),
                                              ISSUE_PX_2 = as.numeric(ISSUE_PX),
                                              YIELD_ON_ISSUE_DATE_2 = as.numeric(YIELD_ON_ISSUE_DATE),
                                              SPREAD_AT_ISSUANCE_TO_WORST_2 = as.numeric(SPREAD_AT_ISSUANCE_TO_WORST))


lapply(my_gb_data_new, class)



############################
#       Re-order columns   #   
############################    

names(my_gb_data_new)

col_order<- c( "State", "COUNTY_LOCATION_ISSUER", "CUSIP","Issuer Name", "ISSUER",
              "Mty Size", "Amt Out", "Cpn2", "Maturity_Enc", "Dated_Date_Enc" , "OAS_SPREAD_BID_2","DUR_MID_2",
               "ISSUE_PX_2", "YIELD_ON_ISSUE_DATE_2", "SPREAD_AT_ISSUANCE_TO_WORST_2",
              "MUNI_ISSUE_TYP_2", "ISSUER_INDUSTRY_2","MUNI_SOURCE_2","MUNI_OFFERING_TYP_2", 
              "RSK_BB_ISS_5Y_DEFAULT_RISK_2" )

my_gb_data_new_fin<- my_gb_data_new[,col_order]

#drop CUSIP and ISSUER (redundant --> in issuer name)

my_gb_data_new_fin<- my_gb_data_new_fin[,-c(3,5)]


####################################
#          Hot - Encoding          # --> the format required for the Jaccard distance is binary data
####################################     see this useful link:https://www.r-bloggers.com/2020/02/a-guide-to-encoding-categorical-features-using-r/


hot_df <- my_gb_data_new_fin
hot_df$MUNI_ISSUE_TYP_2<- factor(hot_df$MUNI_ISSUE_TYP_2, exclude = NULL)
hot_df$ISSUER_INDUSTRY_2<- factor(hot_df$ISSUER_INDUSTRY_2, exclude = NULL)
hot_df$MUNI_SOURCE_2<- factor(hot_df$MUNI_SOURCE_2, exclude = NULL)
hot_df$MUNI_OFFERING_TYP_2<- factor(hot_df$MUNI_OFFERING_TYP_2, exclude = NULL)
hot_df$RSK_BB_ISS_5Y_DEFAULT_RISK_2<- factor(hot_df$RSK_BB_ISS_5Y_DEFAULT_RISK_2, exclude = NULL)

hot_df <- model.matrix(~.-1, data = hot_df[, c( "MUNI_ISSUE_TYP_2",
                                                "ISSUER_INDUSTRY_2" ,
                                                "MUNI_SOURCE_2",
                                                "MUNI_OFFERING_TYP_2",
                                                "RSK_BB_ISS_5Y_DEFAULT_RISK_2")],
                       contrasts.arg = list(
                         MUNI_ISSUE_TYP_2 = contrasts(hot_df$MUNI_ISSUE_TYP_2, contrasts = FALSE),
                         ISSUER_INDUSTRY_2 = contrasts(hot_df$ISSUER_INDUSTRY_2, contrasts = FALSE),
                         MUNI_SOURCE_2 = contrasts(hot_df$MUNI_SOURCE_2, contrasts = FALSE),
                         MUNI_OFFERING_TYP_2 = contrasts(hot_df$MUNI_OFFERING_TYP_2, contrasts = FALSE),
                         RSK_BB_ISS_5Y_DEFAULT_RISK_2 = contrasts(hot_df$RSK_BB_ISS_5Y_DEFAULT_RISK_2, contrasts = FALSE)
                       ))




lapply(hot_df,class)


my_gb_data_new_fin_hot<- cbind(my_gb_data_new_fin, hot_df)



####################################################################
# Split the selected States for the KPCA by COUNTY_LOCATION_ISSUER #
####################################################################

#I construct them both - the second is much bigger and I might not use it at some point

mygb_prova<- my_gb_data_new_fin %>% group_split(State) 

my_gb_data_county<- my_gb_data_new_fin %>% group_split(COUNTY_LOCATION_ISSUER) 

my_gb_data_county_hot<- my_gb_data_new_fin_hot %>% group_split(COUNTY_LOCATION_ISSUER) 


issuer_names_my_county_bystate<-  sapply(1:length(my_gb_data_county), function(j) 
                                       unique(my_gb_data_county[[j]]$COUNTY_LOCATION_ISSUER ))


issuer_names_my_county_bystate 

#the first one is "#N/A Field Not Applicable" hence I remove it:
my_gb_data_county<- my_gb_data_county[-1]
my_gb_data_county_hot<- my_gb_data_county_hot[-1]
issuer_names_my_county_bystate<- issuer_names_my_county_bystate[-1]


n_county<- length(my_gb_data_county)

issuer_names_my_county<- sapply(1:n_county, function(i) unique(my_gb_data_county[[i]]$COUNTY_LOCATION_ISSUER)) 

#check
sapply(1:n_county, function(i) unique(my_gb_data_county_hot[[i]]$COUNTY_LOCATION_ISSUER)) 

saveRDS(my_gb_data_county_hot,
        file = "C:\\...\\my_gb_data_county_hot.rds")

#####################################################
#                   2 KERNELS                       #
#####################################################
#                                                   #
#k1: Euclidean distance between numerical variables # 
#                                                   #
#####################################################
#                                                   #
#k2: Jaccard distance between categorical variables #
#                                                   #
#####################################################




######################################
#    Euclidean Distance Similarity   # my_gb_data_county_hot and my_gb_data_county are the same but
###################################### hot has 63 variables for the jaccard distance

euc_mat = lapply(1:n_county, function(i) distance(my_gb_data_county[[i]][, 4:13], method = "euclidean")) 

######################################
#    Jaccard Distance Similarity     #
######################################

jacc_mat<- lapply(1:n_county, function(i) (1 - distance(my_gb_data_county_hot[[i]][, c(19:63)], method = "jaccard")))


############################################
#   FINAL KERNEL :   SUM    THEM TOGETHER  #   
############################################

final_mat = lapply(1:n_county, function(i) jacc_mat[[i]] + euc_mat[[i]])

heatmap.2(as.matrix(final_mat[[11]]), dendrogram = "none", Rowv=FALSE, Colv = FALSE,  trace="none",symm=T)


dim_mat <- sapply(1:n_county, function(i) dim(final_mat[[i]])[1] )
dim_mat

#some are null
is_null_dim_jacc<- sapply(1:n_county, function(i) is.null(dim_mat[[i]]) )
to_remove_1<- which(is_null_dim_jacc == "TRUE")

new_mat_final<- final_mat[-to_remove_1]

#not enough issuers (less than 2)
dim_mat_2 <- sapply(1:length(new_mat_final), function(i) dim(new_mat_final[[i]])[1] )
dim_mat_2

                                      
to_remove_2<- which(dim_mat_2 < 3 )   
                                      
new_mat_final_2<- new_mat_final[-to_remove_2]

sapply(1:length(new_mat_final_2), function(i) dim(new_mat_final_2[[i]])[1] )


issuer_names_my_county_final2<- sapply(1:n_county, function(i) unique(my_gb_data_county[[i]]$COUNTY_LOCATION_ISSUER))[-to_remove_1]
issuer_names_my_county_final2

issuer_names_my_county_final<- issuer_names_my_county_final2[-to_remove_2]
issuer_names_my_county_final

length(issuer_names_my_county_final)


length(new_mat_final_2)


my_gb_data_county_final_2 = my_gb_data_county[-to_remove_1]
my_gb_data_county_final_3 = my_gb_data_county_final_2[-to_remove_2]

my_gb_data_county_final_hot_2 = my_gb_data_county_hot[-to_remove_1]
my_gb_data_county_final_hot_3 = my_gb_data_county_final_hot_2[-to_remove_2]


length(my_gb_data_county_final_3)
length(my_gb_data_county_final_hot_3)



save(my_gb_data_county_final_3, issuer_names_my_county_final, my_gb_data_county_final_hot_3,
     file = "C:\\...\\gb_data_final_CA_TX_NY.RData")


###################################
#           Kpca                  #
###################################


##########################
#     import modules     #
##########################

pd <- import("pandas")
np <- import("numpy")
sk_dec<-  import("sklearn.decomposition")


#################
#  grid values  #
#################

gamma_vec = c(0.00001, 0.0001, 0.001, 0.01)
alfa_vec = c(0.01, 0.1, 1, 10, 100)


my_rbf_c_k<- function(gamma_hyper, Kx){
     
     rbf_k = exp(- gamma_hyper * Kx)  # or exp(-gamma_hyper* Kx *t(Kx) * 2*tcrossprod(x)) ?
     Kxc = t(t(rbf_k)-colMeans(rbf_k))
     Kxc = Kxc-rowMeans(Kxc)
     
     return(Kxc)
}


ker_matrices<- lapply(1:length(gamma_vec), function(i) 
                  lapply(1:length(new_mat_final_2), function(j)
                      my_rbf_c_k(gamma_vec[i], as.matrix(new_mat_final_2[[j]]))))

heatmap.2(as.matrix(ker_matrices[[1]][[1]]), dendrogram = "none", Rowv=FALSE, Colv = FALSE,  trace="none",symm=T)



####################
#   KPCA functions # 
####################

#8 values for gamma
#5 values for alfa
#35 kpca one for each state


kpca_function<- function(df, kernel, hyper2, ncomp){ 
  
  df_mat<- as.matrix(df)
  kpca_machine<- sk_dec$KernelPCA( kernel= kernel,
                                   n_components = int(ncomp),
                                   alpha = hyper2)
  
  
  X_kpca<- kpca_machine$fit_transform( df_mat )
  X_list<- list(as.matrix(X_kpca))
  names(X_list)<- c("X_kpca")
  
  return(X_list)
  
}




X_kpca =  lapply(1:length(alfa_vec), function (h) 
                lapply(1:length(gamma_vec), function(i) 
                  lapply(1:length(new_mat_final_2), function(j)
                                     kpca_function(df = ker_matrices[[i]][[j]], 
                                                   kernel = "precomputed", 
                                                   hyper2 = alfa_vec[h],
                                                   ncomp = 3) ))) 



############################################
#  FUNCTION FOR LEARNING THE PRE-IMAGE MAP #
############################################

#kernel for the inversion
inv_kern = lapply(1:length(alfa_vec), function (h) 
              lapply(1:length(gamma_vec), function(i) 
                 lapply(1:length(new_mat_final_2), function(j)
                          X_kpca[[h]][[i]][[j]]$X_kpca  %*%  t(X_kpca[[h]][[i]][[j]]$X_kpca)    ))) 

#denominator to the power of -1
#################################
#  NOTE: HERE I LEARN ALFA!!!!  #  
#################################


# NOTE: TO LEARN THE PRE-IMAGE WE ONLY USE THE EUCLIDEAN PART 


inv_xkpca_den =  lapply(1:length(alfa_vec), function (h) 
                    lapply(1:length(gamma_vec), function(i) 
                       lapply(1:length(new_mat_final_2), function(j) 
                         solve(  (t( inv_kern[[h]][[i]][[j]] ) %*% inv_kern[[h]][[i]][[j]] ) + alfa_vec[h]*diag( dim( inv_kern[[h]][[i]][[j]] )[1]) )    )))


X_back = lapply(1:length(alfa_vec), function (h) 
            lapply(1:length(gamma_vec), function(i) 
                lapply(1:length(new_mat_final_2), function(j) 
                  t(t(as.matrix(my_gb_data_county_final_3[[j]][,4:13]) )  %*%  inv_kern[[h]][[i]][[j]]   %*%  inv_xkpca_den[[h]][[i]][[j]]  %*% t(inv_kern[[h]][[i]][[j]])) )))

#######################################
# MSE distances - Euclidean distances #   
#######################################


dist_kpca<- lapply(1:length(alfa_vec), function(h)
              lapply(1:length(gamma_vec), function(i) 
                lapply(1:length(new_mat_final_2),  function(j)
                  sapply(1:nrow(X_back[[h]][[i]][[j]]), function(k) 
                     dist( rbind(  X_back[[h]][[i]][[j]][k,],
                            as.numeric( my_gb_data_county_final_3[[j]][k,4:13])  ) ) ))))




save(X_kpca, X_back, dist_kpca, file = "C:\\...\\kpca_files_CA_TX_NY_gb.RData" )

load("C:\\Users\\...\\kpca_files_CA_TX_NY_gb.RData")


#################
#final distances#
#################

d_final =lapply(1:length(alfa_vec), function(h)
          lapply(1:length(gamma_vec), function(i) 
           sapply(1:length(new_mat_final_2), function(j)
             sum(dist_kpca[[h]][[i]][[j]])/nrow(X_back[[h]][[i]][[j]])   )))

names(d_final)<- alfa_vec


l_d_final<- lapply(1:length(alfa_vec), function(i) data.frame(do.call(rbind, d_final[[i]])) )


for(i in 1:length(l_d_final)){ colnames(l_d_final[[i]])<- issuer_names_my_county_final }
for(i in 1:length(l_d_final)){ rownames(l_d_final[[i]])<- gamma_vec }
for(i in 1:length(l_d_final)){ l_d_final[[i]]<- tibble::rownames_to_column(l_d_final[[i]], "gamma") }



param_df = lapply(1:length(alfa_vec), function(j)
  data.frame( County = colnames(l_d_final[[j]])[-1],
              gamma = gamma_vec[sapply(2:ncol(l_d_final[[j]]), function(i) which.min(l_d_final[[j]][,i]))],
              dist = sapply(2:ncol(l_d_final[[j]]), function(i) min(l_d_final[[j]][,i])),
              alfa = alfa_vec[j]) )


param_df_2<- do.call(rbind, param_df)

param_df_county = param_df_2 %>% group_split(County)

param_df_county_min =  lapply(1:length(param_df_county), function(i) subset(param_df_county[[i]], 
                                                                          dist==min(dist)) )
param_df_final = as.data.frame(do.call(rbind, param_df_county_min))

param_df_final = format( param_df_final[-3], scientific = F)

save(param_df_final, file = "C:\\..\\param_df_final_CA_TX_NY_gb.RData")


param_df_final_for_kpca<- param_df_final


param_df_final_for_kpca$gamma_pos = sapply(1:dim(param_df_final_for_kpca)[1], function(i) which(gamma_vec == as.numeric( param_df_final_for_kpca$gamma[i])))

param_df_final_for_kpca$alfa_pos = sapply(1:dim(param_df_final_for_kpca)[1], function(i) which(alfa_vec == as.numeric( param_df_final_for_kpca$alfa[i])))


###################################
# Select the optimal kpca indices #
###################################

kpca_index<- lapply(1:3, function(j) lapply(1:dim(param_df_final_for_kpca)[1], function(i)
  X_kpca[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]][[i]]$X_kpca[,j]  ))

for(i in 1:3){
  
  names(kpca_index[[i]]) <-param_df_final_for_kpca$County
  
}



save(kpca_index, file = "C:\\...\\kpca_opt_index_CA_TX_NY_gb.RData")

load("C:\\Users\\mcampi\\Desktop\\Green_data\\Muni_Green_Bond_Data\\kpca_opt_index_CA_TX_NY_gb.RData")

###############################
# Select the obest pre-images #
###############################


best_preimages = lapply(1:dim(param_df_final_for_kpca)[1], function(i)
  X_back[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]][[i]]  )


rec_euc = lapply(1:length(best_preimages), function(j)
            sapply(1:ncol(best_preimages[[j]]), function(i) {
                    sqrt(sum((best_preimages[[j]][, i] - my_gb_data_county_final_hot_3[[j]][,4:13][, i])^2))
                          }))

rec_euc = do.call(rbind, rec_euc)
rec_euc = as.data.frame(rec_euc)
colnames(rec_euc) = c( "Mty Size" ,"Amt Out", "Cpn","Maturity", "Dated_Date", "Oas_Spread_bid",             
   "Dur_Mid",  "Issue_Px", "Yield_Issue_Date","Spread_Issuance_Worst")
rec_euc = scale(rec_euc)
rec_euc = as.data.frame(rec_euc)
rec_euc$County = do.call(rbind,lapply(sapply(1:24, function(i) str_split(param_df_final_for_kpca$County[i], ",")), "[[",1))

rec_euc = rec_euc[c(1,12,14,17:22),]

melted_rec_euc = melt(rec_euc)

saveRDS(melted_rec_euc,
        file = "C:\\...\\California\\melted_rec_euc.rds")

library(ggstance)


ggplot(melted_rec_euc, aes(x = variable, y =  abs(value))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  facet_grid(~County, scales = "free") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 65, 
                                                             size = 7,
                                                             hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Standardised Reconstrunction Error")+
  xlab("County") + coord_flip()


ggplot(melted_rec_euc, aes(x =  abs(value), y =variable )) +
  geom_barh(stat = "identity", fill = "steelblue") + 
  facet_grid(County~ .,  scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(legend.position = "none",  
        axis.text.x = element_text(size = 12, hjust = 1),
        axis.text.y = element_text(size = 13, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, vjust = 2.5), 
        strip.text = element_text(face="bold", size=14, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("Standardised Reconstrunction Error")+
  ylab("Numerical Variables") 


ggplot(melted_rec_euc, aes(x =  abs(value), y =County )) +
  geom_barh(stat = "identity", fill = "steelblue") + 
  facet_grid(variable~ .,  scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 65, 
                                                             size = 7,
                                                             hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Standardised Reconstrunction Error")+
  xlab("County") 



#this tells us which variable are best reproduced with all the kPCs - now I take one by one


inv_kern1 = lapply(1:length(alfa_vec), function (h) 
  lapply(1:length(gamma_vec), function(i) 
    lapply(1:length(new_mat_final_2), function(j)
      X_kpca[[h]][[i]][[j]]$X_kpca[,1]  %*%  t(X_kpca[[h]][[i]][[j]]$X_kpca[,1])    ))) 

inv_xkpca_den1 =  lapply(1:length(alfa_vec), function (h) 
  lapply(1:length(gamma_vec), function(i) 
    lapply(1:length(new_mat_final_2), function(j) 
      solve(  (t( inv_kern1[[h]][[i]][[j]] ) %*% inv_kern1[[h]][[i]][[j]] ) + alfa_vec[h]*diag( dim( inv_kern1[[h]][[i]][[j]] )[1]) )    )))


X_back1 = lapply(1:length(alfa_vec), function (h) 
  lapply(1:length(gamma_vec), function(i) 
    lapply(1:length(new_mat_final_2), function(j) 
      t(t(as.matrix(my_gb_data_county_final_3[[j]][,4:13]) )  %*%  inv_kern1[[h]][[i]][[j]]   %*%  inv_xkpca_den1[[h]][[i]][[j]]  %*% t(inv_kern1[[h]][[i]][[j]])) )))





inv_kern2 = lapply(1:length(alfa_vec), function (h) 
  lapply(1:length(gamma_vec), function(i) 
    lapply(1:length(new_mat_final_2), function(j)
      X_kpca[[h]][[i]][[j]]$X_kpca[,c(1:2)]  %*%  t(X_kpca[[h]][[i]][[j]]$X_kpca[,c(1:2)])    ))) 

inv_xkpca_den2 =  lapply(1:length(alfa_vec), function (h) 
  lapply(1:length(gamma_vec), function(i) 
    lapply(1:length(new_mat_final_2), function(j) 
      solve(  (t( inv_kern1[[h]][[i]][[j]] ) %*% inv_kern2[[h]][[i]][[j]] ) + alfa_vec[h]*diag( dim( inv_kern2[[h]][[i]][[j]] )[1]) )    )))


X_back2 = lapply(1:length(alfa_vec), function (h) 
  lapply(1:length(gamma_vec), function(i) 
    lapply(1:length(new_mat_final_2), function(j) 
      t(t(as.matrix(my_gb_data_county_final_3[[j]][,4:13]) )  %*%  inv_kern2[[h]][[i]][[j]]   %*%  inv_xkpca_den2[[h]][[i]][[j]]  %*% t(inv_kern2[[h]][[i]][[j]])) )))



best_preimages1 = lapply(1:dim(param_df_final_for_kpca)[1], function(i)
  X_back1[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]][[i]]  )

best_preimages2 = lapply(1:dim(param_df_final_for_kpca)[1], function(i)
  X_back2[[ param_df_final_for_kpca$alfa_pos[i] ]][[ param_df_final_for_kpca$gamma_pos[i] ]][[i]]  )



rec_euc1 = lapply(1:length(best_preimages1), function(j)
  sapply(1:ncol(best_preimages1[[j]]), function(i) {
    1/10 *sqrt(sum(( my_gb_data_county_final_hot_3[[j]][,4:13][, i] - best_preimages1[[j]][, i])^2))
  }))

rec_euc2 = lapply(1:length(best_preimages2), function(j)
  sapply(1:ncol(best_preimages2[[j]]), function(i) {
    1/10 *sqrt(sum(( my_gb_data_county_final_hot_3[[j]][,4:13][, i] - best_preimages2[[j]][, i] )^2))
  }))



rec_euc1 = do.call(rbind, rec_euc1)
rec_euc1 = as.data.frame(rec_euc1)
colnames(rec_euc1) = c( "Mty Size" ,"Amt Out", "Cpn","Maturity", "Dated_Date", "Oas_Spread_bid",             
                       "Dur_Mid",  "Issue_Px", "Yield_Issue_Date","Spread_Issuance_Worst")
rec_euc1 = scale(rec_euc1, center = F)
rec_euc1 = as.data.frame(rec_euc1)
rec_euc1$County = do.call(rbind,lapply(sapply(1:24, function(i) str_split(param_df_final_for_kpca$County[i], ",")), "[[",1))
rec_euc1$col = "kPC1"

rec_euc1 = rec_euc1[c(1,12,14,17:22),]

melted_rec_euc1 = melt(rec_euc1)


rec_euc2 = do.call(rbind, rec_euc2)
rec_euc2 = as.data.frame(rec_euc2)
colnames(rec_euc2) = c( "Mty Size" ,"Amt Out", "Cpn","Maturity", "Dated_Date", "Oas_Spread_bid",             
                        "Dur_Mid",  "Issue_Px", "Yield_Issue_Date","Spread_Issuance_Worst")
rec_euc2 = scale(rec_euc2,center = F)
rec_euc2 = as.data.frame(rec_euc2)
rec_euc2$County = do.call(rbind,lapply(sapply(1:24, function(i) str_split(param_df_final_for_kpca$County[i], ",")), "[[",1))
rec_euc2$col = "kPC2"


rec_euc2 = rec_euc2[c(1,12,14,17:22),]

melted_rec_euc2 = melt(rec_euc2)


all_melted_rec_euc = rbind(melted_rec_euc1, melted_rec_euc2)

saveRDS(all_melted_rec_euc,
        file = "C:\\...\\all_melted_rec_euc_kpcs.rds")



library(ggstance)


ggplot(all_melted_rec_euc, aes(x =  value, y =variable, fill = col )) +
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
  xlab("MSE")+
  ylab("Numerical Variables") 






#####################################
# Select the optimal GAMMA hyper.   # 
#####################################

gamma_optimal =   as.numeric(param_df_final_for_kpca$gamma)
alfa_optimal =   as.numeric(param_df_final_for_kpca$alfa)

save(gamma_optimal, alfa_optimal, file = "C:\\...\\opthyper_CA_TX_NY_gb.RData")



########################################
# We know evaluate a new set of points #  
########################################                        


#I TAKE MAXIMUM AND MINIMA OF only EUCLIDEAN VARIABELS

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


min_new_mesh = matrix(colMins(list_ranges_min_final), 1, 10) 
max_new_mesh= matrix(colMaxs(list_ranges_max_final), 1, 10)


colnames(min_new_mesh)<- features
colnames(max_new_mesh)<- features


############
# New Mesh #  
############    


#choose one or another

N <- 100

N<- 1000


new_meshes_euc<- lapply(1:length(min_new_mesh), function(i) seq(min_new_mesh[i], max_new_mesh[i], length.out = N))



#We now stratify the categories with all the possible permutations that we have
un_hot_df<- unique(hot_df)

dim(un_hot_df)

#To stratify our new grid, each of the unique category has to be repeated N/ncol(hot_df)
#note that since it is not a finite number then we round it

n_rep_category<- round(N/ncol(hot_df))

#Then, to construct our new mesh, we repeat the unique rows of hot_df a  number of times equal to n_rep_category  
# and obtain the new mesh. Note that if there are some N missing then we repeat again the firt categories



new_meshes_jacc<- un_hot_df[rep(seq_len(nrow(un_hot_df)), each = n_rep_category), ]

dim(new_meshes_jacc)


##########################
#THIS IS FOR N = 100 only#
##########################
final_part<- rbind(un_hot_df[1,], un_hot_df[1,])
new_meshes_jacc<- rbind(new_meshes_jacc, final_part )
dim(new_meshes_jacc)


###########################
#THIS IS FOR N = 1000 only#
###########################

new_meshes_jacc<- new_meshes_jacc[1:N,]
dim(new_meshes_jacc)


final_meshes_1<- do.call(cbind, new_meshes_euc)
#final_meshes_2<- do.call(cbind, new_meshes_jacc)
final_meshes_2<- as.matrix(new_meshes_jacc)


dim(final_meshes_1)[2] + dim(final_meshes_2)[2]
length(features) + length(colnames(all_perm_df)) #correct 63 - 55 = 8 which are the first 3 and the 5 not hot

colnames(final_meshes_1) <- features
colnames(final_meshes_2) 

head(final_meshes_1)
head(final_meshes_2)

##########################
#  Evaluate optimal kpca #
##########################

source_python('C:\\...\\stepwise_kpca_2.py')


#I extract the eigenvalues and eigenvectors of kernel_matrices giving the optimal kpca
#To achieve such a task, I firstly select the optimal kernel matrices


opt_ker_mat<-  lapply(1:dim(param_df_final_for_kpca)[1], function(i)
                           ker_matrices[[param_df_final_for_kpca$gamma_pos[i]]][[i]] )


eigen_dec <- lapply(1:length(gamma_optimal), function(i)
  stepwise_kpca_2(K_norm = as.matrix(opt_ker_mat[[i]]),
                  n_components = int(3))  )

saveRDS(eigen_dec, file = "C:\\...\\eigen_dec_kpca_opt_CA_TX_NY.rds")



################################################################
# We now construct a projection of the new mesh by using these #
#sets of eigenvalues and eigenfunctions to check results above #
################################################################


source_python('C:\\...\\project_x_2.py')

lambdas = lapply(1:length(eigen_dec), function(i) do.call(cbind, eigen_dec[[i]][[2]]))


new_X_kpca =   lapply(1:length(my_gb_data_county_final_3), function(i) sapply(1:N, function(j)   
  project_x_2(x_new_1 = final_meshes_1[j,],
              x_new_2 = final_meshes_2[j,],
              X1 = as.matrix(scale(my_gb_data_county_final_3[[i]][, c(4:13)], center = FALSE)),
              X2 = as.matrix(my_gb_data_county_final_hot_3[[i]][, c(19:63)]),
              gamma = gamma_optimal[i],
              alphas = eigen_dec[[i]][[1]],
              lambdas = lambdas[[i]]) ))





