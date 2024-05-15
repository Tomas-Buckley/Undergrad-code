library(MASS)
getwd()
setwd("C:/Users/Lenovo/Documents/Stats consulting sem II")

CONTRACTS<-read.csv("Motor Liability Claims Data - Frequency.csv",header=TRUE,encoding="latin1")
CLAIMS<-read.csv("Motor Liability Claims Data - Severity.csv",header=TRUE,encoding="latin1")

class(CLAIMS)
################################## Data Cleaning and Preparation ##############################
#We only want policies that are in both datasets
CONTRACTs.new <- subset(CONTRACTS,CONTRACTS$ClaimNb>0)#34060 here because there are duplicates


claim_to_con <- setdiff(unique(CONTRACTs.new$IDpol),unique(CLAIMS$IDpol))#9116
con_to_claim <- setdiff(unique(CLAIMS$IDpol),unique(CONTRACTs.new$IDpol))#6


merge_dat = merge(CONTRACTs.new,CLAIMS)
merge_dat
nrow(merge_dat)

# Check for missing values
sum(is.na(merge_dat))


# Add in additional columns
#1. Random number between 1 and 4
#2. column of 1's so we can add the number of claims later
#3. A unique number for each claim (multiple instances of some IDpols)
#4. Random number between 1 and 10 for train test split
dat1 = data.frame(ClaimID = 1:26444, ClaimCountRow = 1, merge_dat)
dat1
nrow(dat1)
# 26,444 policies in raw data

# Remove Claims with an Exposure > 1
plot(dat1$Exposure, ylab="Exposure", main="Plot of Exposure Levels",col='orange')
dat2 = dat1[-which(dat1$Exposure > 1),]
dat2
nrow(dat1) - nrow(dat2)
nrow(dat2)
# 54 Rows Removed with Exposure > 1

# Remove Policies with more than five claims
plot(dat2$ClaimNb, ylab = "Claim Number")
dat3 = dat2[-which(dat2$ClaimNb > 5),]
dat3
nrow(dat2) - nrow(dat3)
nrow(dat3)
# Additional 61 Rows Removed with ClaimNb > 5


# Remove Claims greater than 10,000
hist(log10(dat3$ClaimAmount), xlab = "log(Claim Amount)",col='blue', main='Histogram of log(Claim Amount)')
dat4 = dat3[-which(dat3$ClaimAmount > 10000),]
dat4
nrow(dat3) - nrow(dat4)
nrow(dat4)
# 478 claims removed that were greater than 10,000

#remove claim amounts less than 50
dat5 = dat4[-which(dat4$ClaimAmount < 50),]
dat5
nrow(dat4) - nrow(dat5)
nrow(dat5)
# 327 claims removed that were less than 50

# Check the type of each variable
str((merge_dat))

# Need to change all "chr" types to factors ( NOTE had to change region manually)
dat5$Region = factor(dat5$Region)
dat5$VehBrand = factor(dat5$VehBrand)
dat5$VehGas = factor(dat5$VehGas)
dat5$Area = factor(dat5$Area)
dat5$Gender = factor(dat5$Gender)
dat5$Transmission = factor(dat5$Transmission)
dat5$Employment = factor(dat5$Employment)

#combine Region and Area into one variable
dat6 = dat5
dat6$RegArea <- paste(dat6$Region, dat6$Area, sep="_")
dat6 <- subset(dat6, select = -c(Region, Area))
dat6$RegArea <- factor(dat6$RegArea, levels = unique(dat6$RegArea))

class(dat6)
