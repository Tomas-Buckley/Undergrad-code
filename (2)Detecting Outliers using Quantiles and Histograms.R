###Detecting Outliers using Quantiles + Histograms
names(dat6)

## VehPower
write.csv(table(dat6$VehPower)), file='vehp.csv',row.names = FALSE)

# 4    5    6    7    8    9    10    11   12   13    14   15 
# 3806 4975 6089 5408 1593 1178 1175  695  281  134   97   93

hist(dat6$VehPower, main = "Histogram of Vehicle Power",col="red")
# Vehicle power has a clear right skew
quantile(dat6$VehPower, prob = 0.99)
# 1% of Vehicles have Power greater than 13


## VehAge
table(dat6$VehAge)
write.csv(table(dat6$VehAge), file='vehage.csv')
# 0    1    2    3    4    5    6    7    8    9    10   11   12   13    14   15   16   17   18   19 
# 1134 2253 2183 1819 1729 1631 1624 1595 1451 1495 1590 1336 1301 1031  941  801  525  405  235  135 

# 20    21   22   23   24   25   26   27   28   29   30   31   32   33   35   36   39   69   84   99 
# 109   53   36   24   19   16   13    3    7    2    7    2    3    2    4    5    2    1    1    1 

plot(dat6$VehAge)
#clear separation at regular intervals, worth investigating
hist(dat6$VehAge)
#heavy right skew
quantile(dat6$VehAge, probs = .99)
# Only 1% of insured cars are on the road more than 20 years


## VehBrand
table(dat6$VehBrand)

# B1    B10  B11 B12   B13  B14 B2   B3   B4   B5   B6 
# 6620  735  642 4099  511  126 6568 2347 1068 1596 1212

#hist doesnt work unless variable is numeric, 
#quantile makes no sense for categorical also
barplot(table(dat6$VehBrand))
#far more of B2 and B1 than any others, B12 also has many

## VehGas
table(dat6$VehGas)

# Diesel  Regular 
# 13031   12493

barplot(table(dat6$VehGas))
#pretty even across the two

## Transmission
table(dat6$Transmission)

# Automatic    Manual 
# 12724        12800 

barplot(table(dat6$Transmission))
#again, very even

## Area
table(dat6$RegArea)

barplot(table(dat6$RegArea))
# Large numbers correspond with densest populations in France, Centre_C most common

## Density
table(dat6$Density)

# Too many to put in table,
quantile(dat6$Density, prob = 0.99)
#1% above 27000
hist(dat6$Density)
# Heavily skewed to the right

## Gender
table(dat6$Gender)

# Female Male 
# 12786  12738 

barplot(table(dat6$Gender))
# even split again, as expected

## DrivAge
table(dat6$DrivAge)

# 18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
# 58 229 302 298 330 359 353 386 443 398 460 493 509 507 489 561 551 565 575 604 576 577 578

# 41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63 
# 608 603 630 676 688 598 705 626 651 674 720 663 626 584 517 490 433 371 320 293 318 298 229 

# 64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86 
# 229 238 206 191 197 197 188 153 180 137 125 137 135 108 107 103  91  51  57  43  27  12  24 

# 87  88  89  90  91  93  94  95  99 
# 13  10  20   8   7   1   1   2   4 

# Ages range from 18 to 99
quantile(dat6$DrivAge, probs = 0.99)
# 1% of drivers older than 81, very few 18 year old drivers

hist(dat6$DrivAge)
#Almost Normal, slight right skew


## Employment
table(dat6$Employment)

# Private    Public     Unemployed 
# 8511       8433       8580

barplot(table(dat6$Employment))
# fairly equal split

## Fines
table(dat6$Fines)

# 0      90    135    180   225   270   315   360   375   405   450   465   495   510   540 
# 13387  2607  2681   314   663   401    92    83  2620    30    22   686    16   719     2 

# 555  585   600   630   645   690   735   750   780   810   825   840   870   885   915 
# 93     2   186     4    97    30    36   373    14     1     3    95     3   117     2 

# 930   975  1005  1020  1065  1110  1125  1155  1200  1215  1260  1350  1395  1500  1590 
# 19    27     1    20     5     7    33     3     1     6    15     1     2     2     1 

# 1725  1875 
# 1     1

#every variable has 45 between them until 450, due to the speeding fine being 90 with an extra penalty of 45 for being late
quantile(dat6$Fines, probs = 0.99)
#less than 1% after 885

hist(dat6$Fines)
# Heavy right skew


## BonusMalus
table(dat6$BonusMalus)

# 50      51    52    53    54    55    56    57    58    59    60     61    62    63    64 
# 11144   399   252   157   435   328   209   471   543   136   526    88  1035   247   520 

# 65     66    67    68    69    70    71    72    73    74    75    76    77    78    79 
# 114    72   224   574    83    74   199   624    99    49   134   648   182    76    13

# 80     81    82    83    84    85    86    87    88    89    90    91    92    93    94 
# 687    37    33    55    11   738    29     9    52     6   696    25    10    22     2 

# 95     96    97    98    99   100   101   102   103   104   105   106   107   108   109 
# 908    32    17     3     4  1272    22    15     6     3     8   194     7     2     1 

# 110   112   113   114   115   116   117   118   119   120   121   122   125   126   130 
# 5     254     3     2     9     3     1   242     1     1     3     1   283     2     1 

# 132   133   138   139   140   147   148   156   158   165   173   176   187   190   196 
# 17     7     1     8    26    27    16    30     2     5     4     1     2     2     2 

# 208   228 
# 1     1

hist(dat6$BonusMalus)
# Heavy right skew
quantile(dat6$BonusMalus, probs = 0.99)
#1% above 125



## ClaimNb
table(dat6$ClaimNb)

# 1      2      3      4      5 
# 22815  2500   180    20     9 

quantile(dat6$ClaimNb, probs = 0.99)
#only 1% have 2 or more
hist(dat6$ClaimNb)
# as expected most of these people only made one claim, note all zero claims are omitted from claim amount


## ClaimAmount
min(dat6$ClaimAmount) # 50.07
max(dat6$ClaimAmount) # 10,000
quantile(dat6$ClaimAmount)

# 0%        25%       50%      75%      100% 
# 50.0700   707.6575  1,172  1,204  10,000

hist(dat6$ClaimAmount,breaks=500,xlim=c(0,10000))
# Right Skewed
# Largest Amount of claims at around 1200
hist(dat6$ClaimAmount,breaks=500,xlim=c(1000,1400))
#5000 at 1200, and 4000 at 1120, could be a repeated cost like an evaluation cost


