### Group Factors by levels

names(dat6)

## Data partitioning
set.seed(123)

indices = sample(nrow(dat6))
# Number of rows in each partition
partition_size = ceiling(nrow(dat6) / 5)

# Creating partitions
partitions = list()
for (i in 1:5) {
  start_index <- (i - 1) * partition_size + 1
  end_index <- min(i * partition_size, nrow(dat6))
  partitions[[i]] = dat6[indices[start_index:end_index], ]
}


## VehBrand

#Fit gamma GLM and test significance for each partition
results = lapply(partitions, function(partition) {
  glm_model = glm(ClaimAmount ~ VehBrand, data = partition, family = Gamma(link="log"))
  summary(glm_model)$coefficients
})


#  print out the coefficients of each partition
for (i in seq_along(results)) {
  cat("Partition", i, ":\n")
  print(results[[i]])
}
#Decided to group, B12,B10,B13;  B3,B11,B2; B1,B4; B5,B6,B14

# Create a new factor groupedVehBrand based on your grouping
dat6$VehBrand_grouped = ifelse(dat6$VehBrand %in% c("B5","B6","B14"), "Lowest",
                               ifelse(dat6$VehBrand %in% c("B1","B4"), "Low",
                                      ifelse(dat6$VehBrand %in% c("B12","B10","B13"), "Highest",
                                             "High")))

# Convert grouped VehBrand to a factor
dat6$VehBrand_grouped = factor(dat6$VehBrand_grouped)

# Check the levels of the new factor
levels(dat6$VehBrand_grouped)


## RegArea

# Fit gamma GLM and test significance for each partition
results_regarea = lapply(partitions, function(partition) {
  glm_model = glm(ClaimAmount ~ RegArea, data = partition, family = Gamma(link="log"))
  summary(glm_model)$coefficients
})

#print out the coefficients of each partition
for (i in seq_along(results_regarea)) {
  cat("Partition", i, ":\n")
  print(results_regarea[[i]])
}


low_vec = c("Alsace_A","Champagne-Ardenne_C","Champagne-Ardenne_D","Haute-Normandie_C","Franche-Comte_E","Nord-Pas-de-Calais_A","Auvergne_C","Auvergne_E")
high_vec = c("Champagne-Ardenne_B","Champagne-Ardenne_E","Franche-Comte_A","Bourgogne_B","Auvergne_D","Auvergne_B","Corse_B","Corse_C")
# Create a new column 'groupedRegion' based on specified grouping
dat6$RegArea_grouped = ifelse(dat6$RegArea %in% high_vec,"High",
                              ifelse(dat6$RegArea %in% low_vec,"Low",
                                     "Middle"))

# Convert 'groupedRegion' to a factor
dat6$RegArea_grouped = factor(dat6$RegArea_grouped, levels = c("Low", "Middle","High"))

# Check the levels of the new factor
levels(dat6$RegArea_grouped)

## VehPower

# Fit gamma GLM and test significance for each partition
results_VP = lapply(partitions, function(partition) {
  glm_model = glm(ClaimAmount ~ VehPower_capped_Factor, data = partition, family = Gamma(link="log"))
  summary(glm_model)$coefficients
})

#print out the coefficients of each partition
for (i in seq_along(results_VP)) {
  cat("Partition", i, ":\n")
  print(results_VP[[i]])
}

# Create a new column 'groupedvehiclepower' based on specified grouping
dat6$VehPower_capped = ifelse(dat6$VehPower %in% c(12, 10,11), "High",
                              ifelse(dat6$VehPower %in% c(9,8,5), "Middle",
                                     "Low"))

# Convert vehpower grouped to a factor
dat6$VehPower_capped = factor(dat6$VehPower_capped, levels = c("Low","Middle","High"))

# Check the levels of the new factor
levels(dat6$VehPower_capped)



## Changing base factor to be the most common one
#not necessary for transmission, gender, employment or VehGas since their factors are pretty similar
most_common_reg_area <- names(sort(table(dat6$RegArea_grouped), decreasing = TRUE))[1]
most_common_vehp <- names(sort(table(dat6$VehPower_capped), decreasing = TRUE))[1]
most_common_vehbrand <- names(sort(table(dat6$VehBrand_grouped), decreasing = TRUE))[1]

dat6$RegArea_grouped = relevel(dat6$RegArea_grouped, ref = most_common_reg_area)#Middle
dat6$VehPower_capped = relevel(dat6$VehPower_capped, ref = most_common_vehp)#Low
dat6$VehBrand_grouped = relevel(dat6$VehBrand_grouped, ref = most_common_vehbrand)#High



