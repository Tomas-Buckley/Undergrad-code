### Contingency Tables

categorical_vars <- c("VehBrand", "VehGas", "Transmission", "RegArea", "Gender", "Employment")
#we use a fisher "exact" test because some frequencies are so small that Chi square wont work
for (i in 1:(length(categorical_vars)-1)) {
  #iterate over the remaining variables, the lower have already been done
  for (j in (i+1):length(categorical_vars)) {
    cat("\nFisher test for:", categorical_vars[i], "and", categorical_vars[j], "\n")
    fisher_test<-fisher.test(table(dat6[, categorical_vars[i]], dat6[, categorical_vars[j]]), simulate.p.value=TRUE)
    print(fisher_test$p.value)
  }
}
# Combined Area and region is leading to lower simulated p values than there should be
# none of the others seem to show meaningful interactions, RegArea and Density still worth investigating


contingency_table <- table(dat6$VehBrand, dat6$VehGas)
print(contingency_table)

chi_squared_test <- chisq.test(contingency_table)

# View the test result
print(chi_squared_test)

# Vehicle Gas and Vehicle Brand highly associated

## Correlation between Covariates
numerical_vars <- dat6[c("VehPower", "DrivAge", "Fines", "BonusMalus",  "Density", "ClaimNb")]

correlation_matrix <- cor(numerical_vars)
print(correlation_matrix)

# a correlation greater than 0.515 warrants investigation, none meet this threshold
# bonusmalus and driver age correlated at -0.469



### Univariate Analysis of Continuous Variables

dat6_numeric = data.frame(
  dat6$VehPower,
  dat6$VehAge ,
  dat6$DrivAge ,
  dat6$BonusMalus ,
  dat6$Density ,
  dat6$Fines 
)
correlation_matrix = cor(dat6_numeric)

## visusalising this in a heatmap
correlations <- cor(dat6[, c("ClaimNb", "DrivAge", "VehPower", "Density","BonusMalus","Fines", "VehAge")])
heatmap.2(correlations, trace = "none", margins = c(7,7), keysize = 1.5)

## Create scatter plots with the pairs() function to look for any obivous non-linear relationships
pairs(dat6_numerical, pch = 16, col = "orange")

# Add correlation values to the plot
text(
  x = 1:7,
  y = 1:7,
  labels = round(correlation_matrix, 2),
  pos = 4, 
  cex = 0.7
)


### Univariate Gamma models for Claim Amount
                                            #AIC, reduction in deviance on 1 df unless stated otherwise
summary(glm(ClaimAmount~1, family=Gamma(link="log"),data=dat6))#416464 0 (null model)
summary(glm(ClaimAmount~VehPower_capped_Factor, family=Gamma(link="log"),data=dat6))#416460, 15, 9 df
summary(glm(ClaimAmount~VehAge, family=Gamma(link="log"),data=dat6))#416397, 46
summary(glm(ClaimAmount~DrivAge,family=Gamma(link="log"),data=dat6))#416466, 0
summary(glm(ClaimAmount~BonusMalus,family=Gamma(link="log"),data=dat6))#416440, 17
summary(glm(ClaimAmount~VehBrand,family=Gamma(link="log"),data=dat6))#416412, 148, 10 df
summary(glm(ClaimAmount~VehGas,family=Gamma(link="log"),data=dat6))#416465, 1
summary(glm(ClaimAmount~RegArea,family=Gamma(link="log"),data=dat6))#416407, 172, 103 df
summary(glm(ClaimAmount~Density,family=Gamma(link="log"),data=dat6))#416466, 0
summary(glm(ClaimAmount~Gender,family=Gamma(link="log"),data=dat6))#416466, 0
summary(glm(ClaimAmount~Transmission,family=Gamma(link="log"),dat6))#416465, 0
summary(glm(ClaimAmount~Employment,family=Gamma(link="log"),data=dat6))#416460, 5, 2 df
summary(glm(ClaimAmount~Fines,family=Gamma(link="log"),data=dat6))#416438, 18

### Relevel reference level by frequency


str(dat6)
levels(dat6$VehPower_capped_Factor)
levels(dat6$RegArea)
levels(dat6$VehBrand)
levels(dat6$VehGas)
levels(dat6$Gender)
levels(dat6$Transmission)
levels(dat6$Employment)
