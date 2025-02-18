library(rjags)
library(tidyverse)
library(caTools)
library(ggplot2)





#calculate area under the curve

SRS_combined <- read_csv("C:/Users/feder/Dropbox/Documents/consulting/trials/N0f1/Talisman/Data/SRS_combined.csv")
x <- c(30, 60, 120, 180, 240, 300, 1440) #from 30 min until 24 hours later

time_l <- x[7]-x[1] # overall length follow-up

x1 <- seq(1:7)

#check where all NAs are
NA_index <- which(is.na(SRS_combined), arr.ind=TRUE, useNames = TRUE)

# select only those NAs important for the follow-up of 2 years and outcomes we consider

NA_index_relevant <- NA_index[names(SRS_combined)[NA_index[,2]] %in% c("post_SRS_sympt_30", "post_SRS_sympt_60",
                                    "post_SRS_sympt_120", "post_SRS_sympt_180",
                                         "post_SRS_sympt_240", "post_SRS_sympt_300",
                                    "post_SRS_sympt_FU1",
                                         "post_SRS_medurge_30", "post_SRS_medurge",
                     "post_SRS_medurge_120", "post_SRS_medurge_180",
                                         "post_SRS_medurge_240", "post_SRS_medurge_300",
                     "post_SRS_medurge_FU1","post_SRS_global_30", "post_SRS_global_60",
                                         "post_SRS_global_120", "post_SRS_global_180",
                    "post_SRS_global_240", "post_SRS_global_300",
                                         "post_SRS_global_FU1"),]
# what is the ID of individuals with NAs?

id_NA <- unique(NA_index_relevant[,1]) #5, 153, 150

# what id in castor do they correspond to?

Castor_NA <- SRS_combined$Castor.Participant.ID[id_NA]

# rename variables with minutes post intervention, easier to reshape data

SRS_combined %>% rename(post_SRS_global_1440=post_SRS_global_FU1,
                        post_SRS_sympt_1440=post_SRS_sympt_FU1,
                        post_SRS_medurge_1440=post_SRS_medurge_FU1)->SRS_combined


# consider every outcome as a difference with baseline

SRS_combined %>% mutate(post_SRS_sympt_30 = post_SRS_sympt_30- pre_SRS_sympt, 
                        post_SRS_sympt_60 = post_SRS_sympt_60- pre_SRS_sympt,
                        post_SRS_sympt_120 = post_SRS_sympt_120- pre_SRS_sympt,
                        post_SRS_sympt_180 =  post_SRS_sympt_180- pre_SRS_sympt,
                        post_SRS_sympt_240 = post_SRS_sympt_240- pre_SRS_sympt,
                        post_SRS_sympt_300 = post_SRS_sympt_300- pre_SRS_sympt,
                        post_SRS_sympt_1440 = post_SRS_sympt_1440- pre_SRS_sympt,
                        post_SRS_medurge_30= post_SRS_medurge_30- pre_SRS_medurge,
                        post_SRS_medurge_60 = post_SRS_medurge_60- pre_SRS_medurge,
                        post_SRS_medurge_120 = post_SRS_medurge_120- pre_SRS_medurge,
                        post_SRS_medurge_180 = post_SRS_medurge_180- pre_SRS_medurge,
                        post_SRS_medurge_240 = post_SRS_medurge_240- pre_SRS_medurge,
                        post_SRS_medurge_300 = post_SRS_medurge_300 - pre_SRS_medurge,
                        post_SRS_medurge_1440 = post_SRS_medurge_1440 - pre_SRS_medurge,
                        post_SRS_global_30 = post_SRS_global_30 - pre_SRS_global, 
                        post_SRS_global_60 = post_SRS_global_60 - pre_SRS_global,
                        post_SRS_global_120 = post_SRS_global_120 - pre_SRS_global,
                        post_SRS_global_180 = post_SRS_global_180 - pre_SRS_global,
                        post_SRS_global_240 = post_SRS_global_240 - pre_SRS_global,
                        post_SRS_global_300 = post_SRS_global_300 - pre_SRS_global,
                        post_SRS_global_1440 = post_SRS_global_1440 - pre_SRS_global) -> SRS_combined

# reshape so that data are in long format, time over 2 days 

SRS_combined %>% pivot_longer(cols=c(post_SRS_sympt_30, post_SRS_sympt_60,
                                     post_SRS_sympt_120, post_SRS_sympt_180,
                                     post_SRS_sympt_240, post_SRS_sympt_300,
                                     post_SRS_sympt_1440,post_SRS_medurge_30, post_SRS_medurge_60,
                                     post_SRS_medurge_120, post_SRS_medurge_180,
                                     post_SRS_medurge_240, post_SRS_medurge_300,
                                     post_SRS_medurge_1440,post_SRS_global_30, post_SRS_global_60,
                                     post_SRS_global_120, post_SRS_global_180,
                                     post_SRS_global_240, post_SRS_global_300,
                                     post_SRS_global_1440),names_pattern = "post_SRS_(.*)_(.*)", 
                              names_to = c("type","time")) %>% 
  mutate (ID = as.factor(Castor.Participant.ID),Treatment = 
            as.factor(Treatment), Set = as.factor(Set), 
          time = as.numeric(time)/1440)-> SRS_combined_long 


# select only variables we need for the analysis

SRS_combined_long %>% select(ID, Treatment, time, type,Set, value) -> SRS_combined_long

#Check size should be #5=treatments*20=individuals *7=time point *3=outcomes *2 sets = 4200
 


