dat6$VehPower <- as.factor(dat6$VehPower)
### GLM with Capped Covariates
GLM1 = glm(ClaimAmount ~VehBrand+VehGas+Gender+Transmission+Employment+RegArea+ClaimNb
           +VehAge_Capped+DrivAge_Capped+Fines_Capped+BonusMalus_Capped+Density_Capped+VehPower
           +offset(log(Exposure)),family = Gamma(link = "log"), data=dat6)
summary(GLM1)# AIC = 416,175
GLM1_back = step(GLM1, criteria = "AIC",
                 direction = "both",
                 trace = TRUE)
GLM1_back$anova
# removed
# AIC = 443409

summary(GLM1_back)

### GLM with grouped and Capped Covariates and Grouped Factors ###
GLM2 =  glm(ClaimAmount ~ ClaimNb + DrivAge_Capped_Grouped + VehBrand  
            + VehGas + Gender + Transmission + Density_Capped
            + Employment + BonusMalus_Capped_Grouped + Fines_Capped_Grouped
            + RegArea_grouped + VehAge_Capped_Grouped+VehPower_capped
            + offset(log(Exposure)), family = Gamma(link = "log"), data=dat6)
summary(GLM2)
#AIC 442312
GLM2_back = step(GLM2, criteria = "AIC",
                 direction = "both",
                 trace = TRUE)
GLM2_back$anova
summary(GLM2_back)
final_model= GLM2_back
final_glm = GLM2_back
###################################### CA vs OBs and Lift Chart ###############################################

# Predictions using the gamma GLM
predictions = predict(final_model, type = "response")

# Adding predictions to the dataframe
dat6$predicted_claim_amount = predictions

names(dat6)
#VehPower_chart
# Calculate the mean predicted ClaimAmount for each level of cappedVehPower
mean_predicted_claim = aggregate(predicted_claim_amount ~ VehPower_capped, data = dat6, FUN = mean)

library(ggplot2)
# Generate the graph with mean ClaimAmount from each level of VehPower
ggplot(dat6, aes(x = VehPower_capped, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  # Plot mean points with color mapped to "Mean ClaimAmount"
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + # Add line through mean points with color mapped to "Mean ClaimAmount"
  geom_line(data = mean_predicted_claim, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  # Add line representing mean predicted ClaimAmount with color mapped to "Mean Predicted ClaimAmount"
  geom_point(data = mean_predicted_claim, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + # Add points on the GLM line with color mapped to "Mean Predicted ClaimAmount"
  labs(title = "Mean ClaimAmount vs. VehPower_chart",       # Add labels
       x = "VehPower_chart",
       y = "Mean ClaimAmount",
       color = "Legend") +  # Add legend title
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), # Define colors for legend
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) +  # Define labels for legend
  theme_minimal()



#DrivAge
# Calculate the mean predicted ClaimAmount for each level of cappedDrivAge
mean_predicted_claim_drivage = aggregate(predicted_claim_amount ~   DrivAge_Capped_Grouped, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of DrivAge
ggplot(dat6, aes(x = DrivAge_Capped_Grouped, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + 
  geom_line(data = mean_predicted_claim_drivage, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  
  geom_point(data = mean_predicted_claim_drivage, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. DrivAge_chart",       
       x = "DrivAge_chart",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  theme_minimal()






#VehAge
# Calculate the mean predicted ClaimAmount for each level of VehAge
mean_predicted_claim_vehage = aggregate(predicted_claim_amount ~ VehAge_Capped_Grouped, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of VehAge_chart
ggplot(dat6, aes(x = VehAge_Capped_Grouped, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + 
  geom_line(data = mean_predicted_claim_vehage, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  
  geom_point(data = mean_predicted_claim_vehage, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. VehAge_chart",       
       x = "VehAge_chart",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  ylim(0,4000) +theme_minimal()




#BonusMalus
# Calculate the mean predicted ClaimAmount for each level of BonusMalus_chart
mean_predicted_claim_bonusmalus = aggregate(predicted_claim_amount ~ BonusMalus_Capped_Grouped, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of BonusMalus_chart
ggplot(dat6, aes(x = BonusMalus_Capped_Grouped, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group=1,color = "Mean ClaimAmount")) + 
  geom_line(data = mean_predicted_claim_bonusmalus, aes(group=1,y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), linetype = "solid") +  
  geom_point(data = mean_predicted_claim_bonusmalus, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. BonusMalus_chart",       
       x = "BonusMalus_chart",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  ylim(0,4000) +theme_minimal()


#VehBrand
# Calculate the mean predicted ClaimAmount for each level of VehBrand_chart
mean_predicted_claim_vehbrand = aggregate(predicted_claim_amount ~ VehBrand, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of VehBrand_chart
ggplot(dat6, aes(x = VehBrand, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + 
  geom_line(data = mean_predicted_claim_vehbrand, aes(group=1,y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  
  geom_point(data = mean_predicted_claim_vehbrand, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. VehBrand_chart",       
       x = "VehBrand_chart",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  ylim(0,4000) + theme_minimal()


#VehGas
# Calculate the mean predicted ClaimAmount for each level of VehGas_chart
mean_predicted_claim_vehgas = aggregate(predicted_claim_amount ~ VehGas, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of VehGas_chart
ggplot(dat6, aes(x = VehGas, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + 
  geom_line(data = mean_predicted_claim_vehgas, aes(group=1,y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  
  geom_point(data = mean_predicted_claim_vehgas, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. VehGas_chart",       
       x = "VehGas_chart",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  ylim(0,4000) +theme_minimal()


#RegArea
# Calculate the mean predicted ClaimAmount for each level of Region_chart
mean_preds_region_area = aggregate(predicted_claim_amount ~ RegArea_grouped, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of Region_chart
ggplot(dat6, aes(x = RegArea_grouped, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + 
  geom_line(data = mean_preds_region_area, aes(group=1,y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  
  geom_point(data = mean_preds_region_area, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. Region_chart",       
       x = "Reg_Area",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  ylim(0,4000) +theme_minimal()

#Density
# Calculate the mean predicted ClaimAmount for each level of Density_chart
mean_predicted_claim_density = aggregate(predicted_claim_amount ~ Density_Capped_Grouped, data = dat6, FUN = mean)

# Generate the graph with mean ClaimAmount from each level of Density_chart
ggplot(dat6, aes(x = Density_Capped_Grouped, y = ClaimAmount)) + 
  stat_summary(fun = mean, geom = "point", aes(color = "Mean ClaimAmount")) +  
  stat_summary(fun = mean, geom = "line", aes(group = 1, color = "Mean ClaimAmount")) + 
  geom_line(data = mean_predicted_claim_density, aes(group=1,y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount")) +  
  geom_point(data = mean_predicted_claim_density, aes(y = predicted_claim_amount, color = "Mean GLM Predicted ClaimAmount"), size = 2) + 
  labs(title = "Mean ClaimAmount vs. Density_chart",       
       x = "Density_chart",
       y = "Mean ClaimAmount",
       color = "Legend") + 
  scale_color_manual(values = c("Mean ClaimAmount" = "blue", "Mean GLM Predicted ClaimAmount" = "red"), 
                     labels = c("Mean ClaimAmount", "Mean GLM Predicted ClaimAmount")) + 
  theme_minimal()


# Fit your GLM model


summary_result <- summary(final_model)
coefficients_table <- data.frame(
  Variable = rownames(summary_result$coefficients),
  Estimate = round(exp(summary_result$coefficients[, "Estimate"]),4),
  LowerValue = round(exp( summary_result$coefficients[, "Estimate"] - 2 *summary_result$coefficients[, "Std. Error"]),4),
  UpperValue = round(exp( summary_result$coefficients[, "Estimate"] + 2 *summary_result$coefficients[, "Std. Error"]),4),
  p_values = round(summary_result$coefficients[, "Pr(>|t|)"],4))
print(coefficients_table)





#.............Lift Chart
# Function to create lift chart for all factors combined
create_combined_lift_chart <- function() {
  # Create an empty dataframe to store combined predicted probabilities and actual outcomes
  combined_predicted_actual <- data.frame(predicted_probs = numeric(0), actual_outcome = numeric(0))
  
  # Fit GLM models for each factor and combine predicted probabilities and actual outcomes
  for (factor_name in factors) {
    # Fit the GLM model with only the current factor
    formula <- as.formula(paste("ClaimAmount ~", factor_name))
    glm_model <- glm(formula, data = dat6, family = Gamma(link = "log"))
    
    # Predict probabilities for the outcome variable
    predicted_probs <- predict(glm_model, type = "response")
    
    # Combine predicted probabilities with actual outcomes
    temp_df <- data.frame(predicted_probs, actual_outcome = dat6$ClaimAmount)
    combined_predicted_actual <- rbind(combined_predicted_actual, temp_df)
  }
  
  # Order predictions and divide them into deciles
  combined_predicted_actual <- combined_predicted_actual[order(combined_predicted_actual$predicted_probs), ]
  deciles <- ceiling((1:nrow(combined_predicted_actual)) / (nrow(combined_predicted_actual)/10))
  combined_predicted_actual$decile <- deciles
  
  # Calculate average response rate in each decile
  lift_chart_data <- aggregate(actual_outcome ~ decile, data = combined_predicted_actual, FUN = mean)
  
  # Plot the lift chart
  plot(lift_chart_data$decile, lift_chart_data$actual_outcome, type = "b", xlab = "Decile", ylab = "Actual Outcome Rate", main = "Combined Lift Chart", col = "blue")
  abline(h = mean(combined_predicted_actual$actual_outcome), lty = 2, col = "red")
}

# List of factors in your model
factors <- c("VehGas","Gender","Employment","Density_Capped_Grouped","VehBrand_grouped","Transmission","Employment","Fines_Capped_Grouped","BonusMalus_Capped_Grouped",
             "RegArea_grouped","VehPower_capped","VehAge_Capped_Grouped","DrivAge_Capped_Grouped","Density_Capped_Grouped")

# Call the function to create the combined lift chart
create_combined_lift_chart()
