### Cap Numerical Variables

# Based on quantiles studied previously 
dat6$VehAge_Capped = pmin(dat6$VehAge,20)
dat6$DrivAge_Capped = pmin(dat6$DrivAge-18, 81-18)#reduce driverage min to 0
dat6$Fines_Capped = pmin(dat6$Fines, 885)
dat6$BonusMalus_Capped = pmin(dat6$BonusMalus, 125)
dat6$Density_Capped = pmin(dat6$Density, 27000)



### Categorization of Covariates
#Based on quantiles and frequency Tables


## VehAge
hist(dat6$VehAge_Capped)
quantile(dat6$VehAge_Capped)

dat6$VehAge_Capped_Grouped = cut(dat6$VehAge_Capped,c(0,1,3,7,11,20),include.lowest=TRUE)
hist(dat6$VehAge_Capped_Grouped)


## DrivAge
hist(dat6$DrivAge_Capped)
quantile(dat6$DrivAge_Capped)

dat6$DrivAge_Capped_Grouped = cut(dat6$DrivAge_Capped,c(0,4,8,16,25,29,34,38,48,Inf),include.lowest=TRUE)
barplot(table(dat6$DrivAge_Capped_Grouped))


## Fines
hist(dat6$Fines_Capped)
quantile(dat6$Fines_Capped)

dat6$Fines_Capped_Grouped = cut(dat6$Fines_Capped,c(0,89,136,375,Inf),include.lowest=TRUE)
barplot(table(dat6$Fines_Capped_Grouped))


## Bonus Malus
hist(dat6$BonusMalus_Capped)
quantile(dat6$BonusMalus_Capped)

dat6$BonusMalus_Capped_Grouped = cut(dat6$BonusMalus_Capped,c(49,50,61,71,91,Inf),include.lowest=TRUE)
barplot(table(dat6$BonusMalus_Capped_Grouped))


## Density
hist(dat6$Density_Capped)
quantile(dat6$Density_Capped)

dat6$Density_Capped_Grouped = cut(dat6$Density_Capped,c(0,49,115,250,500,1000,2000,4000,Inf),include.lowest=TRUE)
barplot(table(dat6$Density_Capped_Grouped))

#cleaning up the environment a little
rm(dat1,dat2,dat3,dat4,dat5,dat6_numerical)

dat6$VehAge <- NULL
dat6$DrivAge <- NULL
dat6$Density <- NULL
dat6$Fines <- NULL
dat6$BonusMalus <- NULL
