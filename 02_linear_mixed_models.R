###Linear mixed model ####
library(dplyr)
library(lme4)
library(flextable)
library(broom.mixed)
library(officer)


# Fit linear mixed models of the type y_{ij} = b0 + b1*treatment + b2 

lmm_hypoxia_sympt <- lmer(value~relevel(Treatment, ref=5)*time +(0+time|ID), 
                          data = SRS_combined_long %>% filter(type == "sympt") )

lmm_hypoxia_medurge <- lmer(value~relevel(Treatment, ref=5)*time +(0+time|ID), 
                            data = SRS_combined_long %>% filter(type == "medurge") )


lmm_hypoxia_global <- lmer(value~relevel(Treatment, ref=5)*time +(0+time|ID),
                           data = SRS_combined_long %>% filter(type == "global") )



extract_results <- function(model) {
  # Extract fixed-effect estimates
  fixed_effects <- coef(summary(model))
  
  # Compute confidence intervals
  conf_intervals <- confint(model, parm = "beta_", method = "Wald")
  
  # Combine into a data frame
  results <- data.frame(
    Term = rownames(fixed_effects),
    Estimate = round(fixed_effects[, "Estimate"], 3),
    CI_Lower = round(conf_intervals[, 1], 3),
    CI_Upper = round(conf_intervals[, 2], 3)
  )
  
  return(results)
}


results_model_sympt <- extract_results(lmm_hypoxia_sympt)
results_model_medurge <- extract_results(lmm_hypoxia_medurge)
results_model_global <- extract_results(lmm_hypoxia_global)



library(flextable)
library(officer)

# Create flextables for each model
ft1 <- flextable(results_model_sympt) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()

ft2 <- flextable(results_model_medurge) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()


ft3 <- flextable(results_model_global) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()




# Create a Word document and add content
doc <- read_docx() %>%
  body_add_par("Results for Model sympt", style = "heading 1") %>%
  body_add_flextable(ft1) %>%
  body_add_par("") %>%
  body_add_par("Results for Model medurge", style = "heading 1") %>%
  body_add_flextable(ft2) %>%
  body_add_par("") %>%
  body_add_par("Results for Model global", style = "heading 1") %>%
  body_add_flextable(ft3) %>%
  body_add_par("")  # Add space between sections
# Add space between sections

# Save the document
print(doc, target = "multiple_models_results.docx")

####### How well do the models fit? #####
time_seq <- seq(min(SRS_combined_long$time), max(SRS_combined_long$time), length.out = 100)  # Generate a sequence of time points
treatments <- unique(SRS_combined_long$Treatment)  # Extract unique treatments

# Create a grid of all combinations of time and treatment
new_data <- expand.grid(time = time_seq, Treatment = treatments)

# Ensure variables in the new dataset match the model's structure
new_data$Treatment <- factor(new_data$Treatment)

# Add predicted values
new_data$Predicted <- predict(lmm_hypoxia_sympt, newdata = new_data, 
                              re.form = NA)  # re.form = NA excludes random effects

predictions <- predict(lmm_hypoxia_sympt, newdata = new_data, 
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


### A linear model does not really capture the behaviour of 
# increasing values until t* and then decreasing


#############Adding bdnf to the LMM model to check if the effects change ######

library(readxl)

bdnf <- read_excel("consulting/trials/N0f1/Talisman/bdnf.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric","text"))
bdnf |> rename(ID = Participant, Set = set ) -> bdnf

SRS_combined_long |> left_join(bdnf) -> SRS_combined_long_bdnf

####################################################

lmm_hypoxia_sympt_bdnf <- lmer(value~relevel(factor(Treatment), ref=5)*+(0+time|ID)+Delta_Serumconcentration, 
                               data = SRS_combined_long_bdnf %>% filter(type == "sympt") )

summary(lmm_hypoxia_sympt_bdnf)



lmm_hypoxia_medurge_bdnf <- lmer(value~relevel(factor(Treatment), ref=5)*time +(0+time|ID)+Delta_Serumconcentration, 
                                 data = test_bdnf %>% filter(type == "medurge") )


lmm_hypoxia_global_bdnf <- lmer(value~relevel(factor(Treatment), ref=5)*time +(0+time|ID)+Delta_Serumconcentration,
                                data = test_bdnf %>% filter(type == "global") )



results_model_sympt <- extract_results(lmm_hypoxia_sympt_bdnf)
results_model_medurge <- extract_results(lmm_hypoxia_medurge_bdnf)
results_model_global <- extract_results(lmm_hypoxia_global_bdnf)



library(flextable)
library(officer)

# Create flextables for each model
ft1 <- flextable(results_model_sympt) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()

ft2 <- flextable(results_model_medurge) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()


ft3 <- flextable(results_model_global) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()




# Create a Word document and add content
doc <- read_docx() %>%
  body_add_par("Results for Model sympt", style = "heading 1") %>%
  body_add_flextable(ft1) %>%
  body_add_par("") %>%
  body_add_par("Results for Model medurge", style = "heading 1") %>%
  body_add_flextable(ft2) %>%
  body_add_par("") %>%
  body_add_par("Results for Model global", style = "heading 1") %>%
  body_add_flextable(ft3) %>%
  body_add_par("")  # Add space between sections
# Add space between sections

# Save the document
print(doc, target = "multiple_models_results_bdnf.docx")

######################## add gender and disease stage ####

library(readxl)

extra_covariate_analysis_FG <- read_excel("consulting/trials/N0f1/Talisman/extra covariate analysis FG.xlsx", 
                                             col_types = c("text", "text", "numeric", 
                                                                    "numeric"))

extra_covariate_analysis_FG |> rename(ID = PD, disease = `disease group` ) -> extra_covariate

SRS_combined_long |> left_join(extra_covariate) -> SRS_combined_long_extra

####################################################

lmm_hypoxia_sympt_extra <- lmer(value~relevel(factor(Treatment), ref=5)*time+(0+time|ID)+factor(gender)+
                                  factor(disease), 
                               data = SRS_combined_long_extra %>% filter(type == "sympt") )


summary(lmm_hypoxia_sympt_extra)



lmm_hypoxia_medurge_extra <- lmer(value~relevel(factor(Treatment), ref=5)*time +(0+time|ID)+factor(gender)+
                                    factor(disease), 
                                 data = SRS_combined_long_extra %>% filter(type == "medurge") )

summary(lmm_hypoxia_medurge_extra)


lmm_hypoxia_global_extra <- lmer(value~relevel(factor(Treatment), ref=5)*time +(0+time|ID)+factor(gender)+
                                  factor(disease), 
                                data = SRS_combined_long_extra %>% filter(type == "global") )



summary(lmm_hypoxia_global_extra)

results_model_sympt <- extract_results(lmm_hypoxia_sympt_extra)
results_model_medurge <- extract_results(lmm_hypoxia_medurge_extra)
results_model_global <- extract_results(lmm_hypoxia_global_extra)



library(flextable)
library(officer)

# Create flextables for each model
ft1 <- flextable(results_model_sympt) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()

ft2 <- flextable(results_model_medurge) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()


ft3 <- flextable(results_model_global) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()




# Create a Word document and add content
doc <- read_docx() %>%
  body_add_par("Results for Model sympt", style = "heading 1") %>%
  body_add_flextable(ft1) %>%
  body_add_par("") %>%
  body_add_par("Results for Model medurge", style = "heading 1") %>%
  body_add_flextable(ft2) %>%
  body_add_par("") %>%
  body_add_par("Results for Model global", style = "heading 1") %>%
  body_add_flextable(ft3) %>%
  body_add_par("")  # Add space between sections
# Add space between sections

# Save the document
print(doc, target = "multiple_models_results_extra.docx")


################## all together ########

library(readxl)

extra_covariate_analysis_FG <- read_excel("consulting/trials/N0f1/Talisman/extra covariate analysis FG.xlsx", 
                                          col_types = c("text", "text", "numeric", 
                                                        "numeric"))

extra_covariate_analysis_FG |> rename(ID = PD, disease = `disease group` ) -> extra_covariate

SRS_combined_long_bdnf |> left_join(extra_covariate) -> SRS_combined_long_extra

####################################################

lmm_hypoxia_sympt_extra <- lmer(value~relevel(factor(Treatment), ref=5)*time+(0+time|ID)+factor(gender)+
                                  factor(disease)+Delta_Serumconcentration, 
                                data = SRS_combined_long_extra %>% filter(type == "sympt") )


summary(lmm_hypoxia_sympt_extra)



lmm_hypoxia_medurge_extra <- lmer(value~relevel(factor(Treatment), ref=5)*time +(0+time|ID)+factor(gender)+
                                    factor(disease)+Delta_Serumconcentration, 
                                  data = SRS_combined_long_extra %>% filter(type == "medurge") )

summary(lmm_hypoxia_medurge_extra)


lmm_hypoxia_global_extra <- lmer(value~relevel(factor(Treatment), ref=5)*time +(0+time|ID)+factor(gender)+
                                   factor(disease)+Delta_Serumconcentration, 
                                 data = SRS_combined_long_extra %>% filter(type == "global") )



summary(lmm_hypoxia_global_extra)

results_model_sympt <- extract_results(lmm_hypoxia_sympt_extra)
results_model_medurge <- extract_results(lmm_hypoxia_medurge_extra)
results_model_global <- extract_results(lmm_hypoxia_global_extra)



library(flextable)
library(officer)

# Create flextables for each model
ft1 <- flextable(results_model_sympt) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()

ft2 <- flextable(results_model_medurge) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()


ft3 <- flextable(results_model_global) %>%
  set_header_labels(
    Term = "Term",
    Estimate = "Estimate",
    CI_Lower = "Lower CI",
    CI_Upper = "Upper CI"
  ) %>%
  theme_vanilla() %>%
  autofit()




# Create a Word document and add content
doc <- read_docx() %>%
  body_add_par("Results for Model sympt", style = "heading 1") %>%
  body_add_flextable(ft1) %>%
  body_add_par("") %>%
  body_add_par("Results for Model medurge", style = "heading 1") %>%
  body_add_flextable(ft2) %>%
  body_add_par("") %>%
  body_add_par("Results for Model global", style = "heading 1") %>%
  body_add_flextable(ft3) %>%
  body_add_par("")  # Add space between sections
# Add space between sections

# Save the document
print(doc, target = "multiple_models_results_adjusted.docx")












