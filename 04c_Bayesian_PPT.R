library(rjags)
library(tidyverse)
library(readxl)
library(caTools)
library(ggplot2)


Combined_PPT_tmnt_set <- read_excel("Combined_PPT_tmnt_set.xlsx")

#aggregated

Combined_PPT_tmnt_set %>% group_by(Castor.Participant.ID) %>% mutate(id = cur_group_id()) -> 
  PPT_combined

PPT_combined %>% arrange(id)  %>% select (id, Treatment, Set, 
                                          pre_PPT_both, post_PPT_both) -> PPT_combined

data <- list(n = 20, y_p = t(structure(c(PPT_combined %>% filter(Treatment==5)%>% mutate(delta=post_PPT_both - pre_PPT_both)%>%pull(delta)),
                                       dim=c(2,20))), 
             y_t1 = t(structure(c(PPT_combined %>%
                                    filter(Treatment==1)%>% mutate(delta=post_PPT_both - pre_PPT_both)%>%pull(delta)),
                                dim=c(2,20))),
             y_t2 = t(structure(c(PPT_combined %>%
                                    filter(Treatment==2)%>% mutate(delta=post_PPT_both - pre_PPT_both)%>%pull(delta)),
                                dim=c(2,20))),
             y_t3 = t(structure(c(PPT_combined %>%
                                    filter(Treatment==3)%>% mutate(delta=post_PPT_both - pre_PPT_both)%>%pull(delta)),
                                dim=c(2,20))),
             y_t4 = t(structure(c(PPT_combined %>%
                                    filter(Treatment==4)%>% mutate(delta=post_PPT_both - pre_PPT_both)%>%pull(delta)),
                                dim=c(2,20))),
             
             np = 2,
             #type = SRS_combined %>%filter(id==ID)%>% pull(Treatment),
             nt = 2,K=4,mcid=0)

model_aggregated <- 'model {

for( i in 1 : n ) {
  mu_t1[i] ~ dnorm(mu.t1, tau.t1)
  mu_t2[i] ~ dnorm(mu.t2, tau.t2)
  mu_t3[i] ~ dnorm(mu.t3, tau.t3)
  mu_t4[i] ~ dnorm(mu.t4, tau.t4)
  
  mu_p[i] ~ dnorm(mu.p, tau.p)
  
  diff_patient[i,1] <-  mu_t1[i]-mu_p[i]
  diff_patient[i,2] <-  mu_t2[i]-mu_p[i]
  diff_patient[i,3] <-  mu_t3[i]-mu_p[i]
  diff_patient[i,4] <-  mu_t4[i]-mu_p[i]
 
for(j in 1:K){
p_patient[i,j] <- 1-step(diff_patient[i,j]-mcid)

}
  
  for ( t in 1 : np ) {
    y_p[i, t] ~ dnorm(mu_p[i], tau.within) } 
    
  for ( t in 1 : nt ) {
      y_t1[i, t] ~ dnorm(mu_t1[i], tau.within)  
      y_t2[i, t] ~ dnorm(mu_t2[i], tau.within)  
      y_t3[i, t] ~ dnorm(mu_t3[i], tau.within)  
      y_t4[i, t] ~ dnorm(mu_t4[i], tau.within)  
      }
}      
      
  diff[1] <-  mu.t1-mu.p
  diff[2] <-  mu.t2-mu.p
  diff[3] <-  mu.t3-mu.p
  diff[4] <-  mu.t4-mu.p
      
for(i in 1:K){
p[i] <- 1-step(diff[i]-mcid)

}



tau.within<-1/(sigma.within*sigma.within)
sigma.within~dunif(0.01, 10)

mu.p~dnorm(0, 0.001) 
mu.t1~dnorm(0,0.001) 
mu.t2~dnorm(0,0.001) 
mu.t3~dnorm(0,0.001) 
mu.t4~dnorm(0,0.001) 
  

tau.p<-1/(sigma.p*sigma.p)
sigma.p~dunif(0.01, 10) 
tau.t1<-1/(sigma.t1*sigma.t1)
sigma.t1~dunif(0.01, 10)

tau.t2<-1/(sigma.t2*sigma.t2)
sigma.t2~dunif(0.01, 10)

tau.t3<-1/(sigma.t3*sigma.t3)
sigma.t3~dunif(0.01, 10)

tau.t4<-1/(sigma.t4*sigma.t4)
sigma.t4~dunif(0.01, 10)

}'


parameters <- c("diff","p", "p_patient")


model.fit <- jags.model(file= textConnection(model_aggregated),
                        data=data, n.chains = 3)

model.samples <- coda.samples(model.fit, parameters, n.iter=100000)


#plot(model.samples)
Bayes_PPT <- summary(model.samples)



# Extract components from the summary object
stats_df <- as.data.frame(Bayes_PPT$statistics)
quant_df <- as.data.frame(Bayes_PPT$quantiles)

# Add a column with the parameter names if they are stored as row names
stats_df$Parameter <- rownames(stats_df)
quant_df$Parameter <- rownames(quant_df)

# Merge the two data frames by the Parameter column
combined_df <- merge(stats_df, quant_df, by = "Parameter")

# reorder the columns to have Parameter first
combined_df <- combined_df[, c("Parameter", setdiff(names(combined_df), "Parameter"))]

# Write the combined summary to a CSV file
write.csv(combined_df, file = "Bayes_PPT_summary.csv", row.names = FALSE)

library(writexl)

# Round all numeric columns to 3 decimals
combined_df_rounded <- combined_df
numeric_cols <- sapply(combined_df_rounded, is.numeric)
combined_df_rounded[numeric_cols] <- lapply(combined_df_rounded[numeric_cols], round, 3)


write_xlsx(combined_df_rounded, "Bayes_PPT_summary.xlsx")

