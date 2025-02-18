# Try if splines can capture the behaviour better ##

library(splines)
lmm_hypoxia_sympt_splines <- lmer(value~relevel(factor(Treatment), ref=5)*ns(time, df=4)+(1|ID),#+Delta_Serumconcentration, 
                               data = SRS_combined_long %>% filter(type == "sympt", time!=1) )

summary(lmm_hypoxia_sympt_splines)

####### How well do the models fit? #####
time_seq <- seq(min(SRS_combined_long$time), max(SRS_combined_long$time), length.out = 100)  # Generate a sequence of time points
treatments <- unique(SRS_combined_long$Treatment)  # Extract unique treatments

# Create a grid of all combinations of time and treatment
new_data <- expand.grid(time = time_seq, Treatment = treatments)

# Ensure variables in the new dataset match the model's structure
new_data$Treatment <- factor(new_data$Treatment)

# Add predicted values
new_data$Predicted <- predict(lmm_hypoxia_sympt_splines, newdata = new_data, 
                              re.form = NA)  # re.form = NA excludes random effects

predictions <- predict(lmm_hypoxia_sympt_splines, newdata = new_data, 
                       re.form = NA,se.fit=TRUE)  # re.form = NA excludes random effects


# Add predicted values
new_data$Predicted <- predict(lmm_hypoxia_sympt_splines, newdata = new_data, 
                              re.form = NA)  # re.form = NA excludes random effects

predictions <- predict(lmm_hypoxia_sympt_splines, newdata = new_data, 
                       re.form = NA,se.fit=TRUE)  # re.form = NA excludes random effects

# Add standard errors to the new dataset
new_data$SE <- predictions$se.fit
new_data$Lower <- new_data$Predicted - 1.96 * new_data$SE
new_data$Upper <- new_data$Predicted + 1.96 * new_data$SE
library(ggplot2)

# Plot the predicted trajectories
ggplot(new_data, aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Add lines for each treatment
  labs(title = "Fitted Trajectories by Treatment",
       x = "Time",
       y = "Predicted Outcome - sympt") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Choose a color palette


# Plot with confidence intervals
ggplot(new_data, aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Predicted trajectories
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Treatment), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(title = "Fitted Trajectories with Confidence Intervals",
       x = "Time",
       y = "Predicted Outcome - sympt") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")  # Match fill 


### there is too much variability, let us visualize until 1 day afyter
ggplot(new_data |> filter(time<=0.5), aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Predicted trajectories
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Treatment), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(title = "Fitted Trajectories with Confidence Intervals",
       x = "Time",
       y = "Predicted Outcome - sympt") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")  # Match fill 


# My conclusion is that We do not gain much by using splines

# Try if splines can capture the behaviour better medurge ##

library(splines)
lmm_hypoxia_medurge_splines <- lmer(value~relevel(factor(Treatment), ref=5)*ns(time, df=4)+(1|ID),#+Delta_Serumconcentration, 
                                  data = SRS_combined_long %>% filter(type == "medurge", time!=1) )

summary(lmm_hypoxia_medurge_splines)

####### How well do the models fit? #####
time_seq <- seq(min(SRS_combined_long$time), max(SRS_combined_long$time), length.out = 100)  # Generate a sequence of time points
treatments <- unique(SRS_combined_long$Treatment)  # Extract unique treatments

# Create a grid of all combinations of time and treatment
new_data <- expand.grid(time = time_seq, Treatment = treatments)

# Ensure variables in the new dataset match the model's structure
new_data$Treatment <- factor(new_data$Treatment)

# Add predicted values
new_data$Predicted <- predict(lmm_hypoxia_medurge_splines, newdata = new_data, 
                              re.form = NA)  # re.form = NA excludes random effects

predictions <- predict(lmm_hypoxia_medurge_splines, newdata = new_data, 
                       re.form = NA,se.fit=TRUE)  # re.form = NA excludes random effects


# Add predicted values
new_data$Predicted <- predict(lmm_hypoxia_medurge_splines, newdata = new_data, 
                              re.form = NA)  # re.form = NA excludes random effects

predictions <- predict(lmm_hypoxia_medurge_splines, newdata = new_data, 
                       re.form = NA,se.fit=TRUE)  # re.form = NA excludes random effects

# Add standard errors to the new dataset
new_data$SE <- predictions$se.fit
new_data$Lower <- new_data$Predicted - 1.96 * new_data$SE
new_data$Upper <- new_data$Predicted + 1.96 * new_data$SE
library(ggplot2)

# Plot the predicted trajectories
ggplot(new_data, aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Add lines for each treatment
  labs(title = "Fitted Trajectories by Treatment",
       x = "Time",
       y = "Predicted Outcome - medurge") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Choose a color palette


# Plot with confidence intervals
ggplot(new_data, aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Predicted trajectories
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Treatment), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(title = "Fitted Trajectories with Confidence Intervals",
       x = "Time",
       y = "Predicted Outcome - medurge") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")  # Match fill 


### there is too much variability, let us visualize until 1 day afyter
ggplot(new_data |> filter(time<=0.5), aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Predicted trajectories
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Treatment), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(title = "Fitted Trajectories with Confidence Intervals",
       x = "Time",
       y = "Predicted Outcome - medurge") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")  # Match fill 

#####################################

# Try if splines can capture the behaviour better - global##

library(splines)
lmm_hypoxia_global_splines <- lmer(value~relevel(factor(Treatment), ref=5)*ns(time, df=4)+(1|ID),#+Delta_Serumconcentration, 
                                  data = SRS_combined_long %>% filter(type == "global", time!=1) )

summary(lmm_hypoxia_global_splines)

####### How well do the models fit? #####
time_seq <- seq(min(SRS_combined_long$time), max(SRS_combined_long$time), length.out = 100)  # Generate a sequence of time points
treatments <- unique(SRS_combined_long$Treatment)  # Extract unique treatments

# Create a grid of all combinations of time and treatment
new_data <- expand.grid(time = time_seq, Treatment = treatments)

# Ensure variables in the new dataset match the model's structure
new_data$Treatment <- factor(new_data$Treatment)

# Add predicted values
new_data$Predicted <- predict(lmm_hypoxia_global_splines, newdata = new_data, 
                              re.form = NA)  # re.form = NA excludes random effects

predictions <- predict(lmm_hypoxia_global_splines, newdata = new_data, 
                       re.form = NA,se.fit=TRUE)  # re.form = NA excludes random effects



# Add standard errors to the new dataset
new_data$SE <- predictions$se.fit
new_data$Lower <- new_data$Predicted - 1.96 * new_data$SE
new_data$Upper <- new_data$Predicted + 1.96 * new_data$SE
library(ggplot2)

# Plot the predicted trajectories
ggplot(new_data, aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Add lines for each treatment
  labs(title = "Fitted Trajectories by Treatment",
       x = "Time",
       y = "Predicted Outcome - global") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Choose a color palette


# Plot with confidence intervals
ggplot(new_data, aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Predicted trajectories
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Treatment), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(title = "Fitted Trajectories with Confidence Intervals",
       x = "Time",
       y = "Predicted Outcome - global") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")  # Match fill 


### there is too much variability, let us visualize until 1 day afyter
ggplot(new_data |> filter(time<=0.5), aes(x = time, y = Predicted, color = Treatment)) +
  geom_line(size = 1.2) +  # Predicted trajectories
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Treatment), alpha = 0.2, color = NA) +  # Confidence intervals
  labs(title = "Fitted Trajectories with Confidence Intervals",
       x = "Time",
       y = "Predicted Outcome - global") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")  # Match fill 

##########################################