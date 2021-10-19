#YPLLGgeneva-SummarizeResults

# LIBRARIES ---------------------------------------------------------------
#Basic
library(tidyverse)
#Spatial
library(sf)


# IMPORT DATA -------------------------------------------------------------

#extraction from spatial_autocorrelation_results_YPLLG_long.txt

#Function to return proportion
proportion <- function(vec_val, vec_n){
  return(round(vec_val*100/vec_n,2))
}

#Function to return the mean and standard deviation of a vector
mean_sd<-function(df){
  if(all(rowSums(df)==100)==F){warning('At least one subset has a total percentage that is not equal to 100%')}
  print('Mean values of the 10 subsets')
  print(round(colMeans(df),2))
  print('Standard deviations of the 10 subsets')
  print(round(apply(df, 2, sd),2))
}



# RESULTS FROM THE 10 SUBSETS ---------------------------------------------


#RAW - Number of individuals per LISA cluster classes
# NS.n<-c(6648,6377,6446,7018,6678,6358,6530,6359,5981,6584)
# HH.n<-c(1384,1476,1448,1161,1503,1469,1334,1542,1715,1422)
# LL.n<-c(708,843,807,717,627,860,861,783,883,696)
# LH.n<-c(569,517,460,385,591,497,464,540,615,611)
# HL.n<-c(690,785,835,717,599,814,811,774,803,683)
# Neighborless.n<-c(1,2,4,2,2,2,0,2,3,4)


#ADJUSTED - Number of individuals per LISA cluster classes
NS.n<-c(8031,7980,7929,8866,7900,8371,7988,7549,7271,8186)
HH.n<-c(679,753,760,466,774,632,651,946,1097,765)
LL.n<-c(382,396,428,179,388,320,470,448,500,242)
LH.n<-c(446,408,343,224,443,331,358,497,550,458)
HL.n<-c(461,461,536,263,493,344,533,558,579,345)
Neighborless.n<-c(1,2,4,2,2,2,0,2,3,4)

n<-tibble(NS.n,HH.n,LL.n,LH.n,HL.n,Neighborless.n)
# Minimum value
apply(n, 2, min)
# Maximum value
apply(n, 2, max)
# Mean value
round(colMeans(n),2)
# Standard deviation
round(apply(n, 2, sd),2)



# FEMALES -----------------------------------------------------------------


#RAW - Number of females per LISA cluster classes
# NS.f<-c(3483,3373,3314,3664,3524,3246,3377,3298,3084,3377)
# HH.f<-c(827,915,907,722,897,900,822,973,1059,871)
# LL.f<-c(329,352,372,343,295,401,382,351,418,308)
# LH.f<-c(304,284,263,224,355,286,287,302,340,344)
# HL.f<-c(370,403,466,380,304,436,434,421,413,353)
# Neighborless.f<-c(0,0,1,1,2,1,0,2,1,3)


#ADJUSTED - Number of females per LISA cluster classes
NS.f<-c(4275,4274,4167,4692,4244,4391,4217,3956,3824,4229)
HH.f<-c(374,438,470,279,436,355,368,589,664,460)
LL.f<-c(189,160,193,88,176,146,222,204,237,120)
LH.f<-c(230,219,201,134,244,195,219,295,301,255)
HL.f<-c(245,236,291,140,275,182,276,301,288,189)
Neighborless.f<-c(0,0,1,1,2,1,0,2,1,3)

f<-tibble(NS.f,HH.f,LL.f,LH.f,HL.f,Neighborless.f)
# Minimum value
apply(f, 2, min)
# Maximum value
apply(f, 2, max)
# Mean value
round(colMeans(f),2)
# Standard deviation
round(apply(f, 2, sd),2)
#Percentage
proportion(f,n) %>% colMeans() %>% round(.,1)

# Significavity between LISA classes
#Convert to long dataframe
f <- f %>% mutate(replication = row_number()) #Add row index as column
f <- f %>% pivot_longer(-c(replication), names_to="cluster_type")
#Compute Tukey HSD test
model=lm(f$value ~ f$cluster_type )
ANOVA=aov(model)
TukeyHSD(x=ANOVA, 'f$cluster_type', conf.level=0.95)



# MALES -------------------------------------------------------------------

#RAW - Number of males per LISA cluster classes
# NS.m<-c(3165,3004,3132,3354,3154,3112,3153,3061,2897,3207)
# HH.m<-c(557,561,541,439,606,569,512,569,656,551)
# LL.m<-c(379,491,435,374,332,459,479,432,465,388)
# LH.m<-c(265,233,197,161,236,211,177,238,275,267)
# HL.m<-c(320,382,369,337,295,378,377,353,390,330)
# Neighborless.m<-c(1,2,3,1,2,1,0,0,2,1)

#ADJUSTED - Number of males per LISA cluster classes
NS.m<-c(3756,3706,3762,4174,3656,3980,3771,3593,3447,3957)
HH.m<-c(305,315,290,187,338,277,283,357,433,305)
LL.m<-c(193,236,235,91,212,174,248,244,263,122)
LH.m<-c(216,189,142,90,199,136,139,202,249,203)
HL.m<-c(216,225,245,123,218,162,257,257,291,156)
Neighborless.m<-c(1,2,3,1,0,1,0,0,2,1)

m<-tibble(NS.m,HH.m,LL.m,LH.m,HL.m,Neighborless.m)
# Minimum value
apply(m, 2, min)
# Maximum value
apply(m, 2, max)
# Mean value
round(colMeans(m),2)
# Standard deviation
round(apply(m, 2, sd),2)
#Percentage
proportion(m,n) %>% colMeans() %>% round(.,1)



# NON SWISS ---------------------------------------------------------------


#RAW - Number of non swiss per LISA cluster classes
# NS.nonswiss<-c(1373,1330,1353,1399,1422,1321,1313,1303,1260,1361)
# HH.nonswiss<-c(190,209,200,174,216,210,185,211,239,188)
# LL.nonswiss<-c(204,253,252,214,184,269,258,228,251,192)
# LH.nonswiss<-c(145,119,108,94,136,113,125,137,149,135)
# HL.nonswiss<-c(138,144,133,130,124,156,150,141,146,130)
# Neighborless.nonswiss<-c(0,0,0,0,0,0,0,0,0,0)

#ADJUSTED - Number of non swiss per LISA cluster classes
NS.nonswiss<-c(1671,1634,1621,1788,1652,1734,1598,1547,1528,1682)
HH.nonswiss<-c(111,137,122,80,124,114,110,143,169,110)
LL.nonswiss<-c(80,99,121,41,99,84,110,108,111,50)
LH.nonswiss<-c(88,73,59,42,87,52,70,92,98,75)
HL.nonswiss<-c(100,112,123,60,120,85,143,130,139,89)
Neighborless.nonswiss<-c(0,0,0,0,0,0,0,0,0,0)

nonswiss<-tibble(NS.nonswiss,HH.nonswiss,LL.nonswiss,LH.nonswiss,HL.nonswiss,Neighborless.nonswiss)
# Minimum value
apply(nonswiss, 2, min)
# Maximum value
apply(nonswiss, 2, max)
# Mean value
round(colMeans(nonswiss),2)
# Standard deviation
round(apply(nonswiss, 2, sd),2)
#Percentage
proportion(nonswiss,n) %>% colMeans() %>% round(.,1)



# SWISS -------------------------------------------------------------------


#RAW - Number of swiss per LISA cluster classes
# NS.swiss<-c(5275,5047,5093,5619,5256,5037,5217,5056,4721,5223)
# HH.swiss<-c(1194,1267,1248,987,1287,1259,1149,1331,1476,1234)
# LL.swiss<-c(504,590,555,503,443,591,603,555,632,504)
# LH.swiss<-c(424,398,352,291,455,384,339,403,466,476)
# HL.swiss<-c(552,641,702,587,475,658,661,633,657,553)
# Neighborless.swiss<-c(1,2,4,2,2,2,0,2,3,4)

#ADJUSTED - Number of swiss per LISA cluster classes
NS.swiss<-c(6360,6346,6308,7078,6248,6637,6390,6002,5743,6504)
HH.swiss<-c(568,616,638,386,650,518,541,803,928,655)
LL.swiss<-c(302,297,307,138,289,236,360,340,389,192)
LH.swiss<-c(358,335,284,182,356,279,288,405,452,383)
HL.swiss<-c(361,349,413,203,373,259,390,428,440,256)
Neighborless.swiss<-c(1,2,4,2,2,2,0,2,3,4)

swiss<-tibble(NS.swiss,HH.swiss,LL.swiss,LH.swiss,HL.swiss,Neighborless.swiss)
# Minimum value
apply(swiss, 2, min)
# Maximum value
apply(swiss, 2, max)
# Mean value
round(colMeans(swiss),2)
# Standard deviation
round(apply(swiss, 2, sd),2)
# Percentage
proportion(swiss,n) %>% colMeans() %>% round(.,1)

# Significavity between LISA classes
#Convert to long dataframe
swiss <- swiss %>% mutate(replication = row_number()) #Add row index as column
swiss <- swiss %>% pivot_longer(-c(replication), names_to="cluster_type")
#Compute Tukey HSD test
model=lm(swiss$value ~ swiss$cluster_type )
ANOVA=aov(model)
TukeyHSD(x=ANOVA, 'swiss$cluster_type', conf.level=0.95)



# YPLLG ---------------------------------------------------------------------


#RAW - Mean of LE per LISA classes
# NS.LE<-c(4.86,4.45,3.94,4.30,4.70,4.39,4.51,4.48,3.97,4.38)
# HH.LE<-c(18.76,19.35,19.50,19.41,19.41,19.51,19.28,19.02,19.15,18.74)
# LL.LE<-c(-14.98,-14.99,-15.66,-14.22,-15.09,-14.68,-15.03,-15.91,-15.47,-14.54)
# LH.LE<-c(-11.77,-11.47,-11.19,-9.90,-10.13,-10.21,-9.97,-10.42,-10.20,-10.38)
# HL.LE<-c(17.00,16.54,17.19,17.17,17.15,16.95,17.30,16.68,16.89,16.65)
# Neighborless.LE<-c(10.60,6.90,-3.26,4.73,16.65,-2.17,NaN,-20.41,15.48,-5.94)

#ADJUSTED - Mean of LE per LISA classes
NS.LE<-c(-3.53,-3.66,-4.10,-3.91,-3.73,-3.57,-3.47,-3.97,-3.97,-3.66)
HH.LE<-c(8.46,9.52,9.07,9.28,8.82,9.74,9.13,8.61,9.11,9.17)
LL.LE<-c(-24.22,-23.67,-24.16,-22.56,-24.05,-22.92,-21.70,-24.32,-23.72,-22.51)
LH.LE<-c(-20.84,-20.74,-20.31,-18.79,-20.02,-19.97,-18.71,-19.17,-19.28,-19.63)
HL.LE<-c(8.60,8.58,9.30,9.55,8.77,9.02,9.05,8.70,9.28,9.45)
Neighborless.LE<-c(-2.30,-4.87,-14.03,-5.40,6.77,-12.99,NaN,-29.98,4.37,-14.34)

LE<-tibble(NS.LE,HH.LE,LL.LE,LH.LE,HL.LE,Neighborless.LE)
# Minimum value
apply(LE, 2, min)
# Maximum value
apply(LE, 2, max)
# Mean value
round(colMeans(LE),2)
# Standard deviation
round(apply(LE, 2, sd),2)



# YPLL --------------------------------------------------------------------


#RAW - Mean of YPLL per LISA classes
# NS.YPLL<-c(4.27,4.36,4.59,4.50,4.29,4.41,4.30,4.44,4.64,4.50)
# HH.YPLL<-c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)
# LL.YPLL<-c(11.61,11.83,12.09,11.01,11.71,11.37,11.69,12.32,11.92,11.40)
# LH.YPLL<-c(9.22,8.98,8.62,7.69,7.85,7.94,7.60,8.05,7.90,7.97)
# HL.YPLL<-c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)
# Neighborless.YPLL<-c(0.00,2.50,7.75,2.50,0.00,4.00,NaN,12.50,0.00,6.50)

#ADJUSTED - Mean of YPLL per LISA classes
NS.YPLL<-c(4.07,4.18,4.37,4.34,4.02,4.16,4.17,4.27,4.41,4.34)
HH.YPLL<-c(0.01,0.01,0.01,0.00,0.01,0.01,0.02,0.01,0.01,0.01)
LL.YPLL<-c(12.67,13.09,13.40,12.03,12.86,12.58,11.56,13.17,12.79,11.79)
LH.YPLL<-c(9.72,9.51,8.60,7.83,8.86,8.72,7.97,8.09,8.39,8.80)
HL.YPLL<-c(0.05,0.05,0.03,0.04,0.02,0.04,0.04,0.03,0.03,0.02)
Neighborless.YPLL<-c(0.00,2.50,7.75,2.50,0.00,4.00,NaN,12.50,0.00,6.50)

YPLL<-tibble(NS.YPLL,HH.YPLL,LL.YPLL,LH.YPLL,HL.YPLL)
# Minimum value
apply(YPLL, 2, min)
# Maximum value
apply(YPLL, 2, max)
# Mean value
round(colMeans(YPLL),2)
# Standard deviation
round(apply(YPLL, 2, sd),2)


# INCOME ------------------------------------------------------------------

#RAW - Mean INCOME per LISA classes
# NS.Inc<-c(131745,131050,131849,132005,131741,131986,133217,133202,133722,132155)
# HH.Inc<-c(122458,124662,125166,124841,119719,126142,125346,121270,120105,118996)
# LL.Inc<-c(112679,112988,110094,108454,111660,110792,107923,111290,113317,112259)
# LH.Inc<-c(133531,135101,137893,137518,131334,135743,131456,132503,128348,130611)
# HL.Inc<-c(117151,119427,116325,112682,119526,116978,113341,116390,117384,119932)
# Neighborless.Inc<-c(189714,181910,123659,161831,155578,169635,NaN,158274,172219,134302)

#ADJUSTED - Mean INCOME per LISA classes
NS.Inc<-c(129136,127443,128550,127539,129976,127977,129443,130671,130421,129436)
HH.Inc<-c(131636,138489,134327,148294,123057,140793,132664,122881,122576,119384)
LL.Inc<-c(111703,113073,108454,113418,109982,110903,110382,111184,112598,116478)
LH.Inc<-c(130314,136738,141971,138389,124790,136628,133315,127003,127125,123051)
HL.Inc<-c(118549,123014,119539,120471,119499,120487,118187,120427,120988,126765)
Neighborless.Inc<-c(189714,181910,123659,161831,155578,169635,NaN,158274,172219,134302)

Inc<-tibble(NS.Inc,HH.Inc,LL.Inc,LH.Inc,HL.Inc)
# Minimum value
apply(Inc, 2, min)
# Maximum value
apply(Inc, 2, max)
# Mean value
round(colMeans(Inc),2)
# Standard deviation
round(apply(Inc, 2, sd),2)

# Significavity between LISA classes
#Convert to long dataframe
Inc <- Inc %>% mutate(replication = row_number()) #Add row index as column
Inc <- Inc %>% pivot_longer(-c(replication), names_to="cluster_type")
#Compute Tukey HSD test
model=lm(Inc$value ~ Inc$cluster_type )
ANOVA=aov(model)
TukeyHSD(x=ANOVA, 'Inc$cluster_type', conf.level=0.95)

# Neighborhood median age ------------------------------------------------------------------

#RAW - Mean Neighborhood median age per LISA classes
# NS.Medage<-c(41.42,41.32,41.17,41.37,40.93,41.25,41.38,41.10,40.81,40.93)
# HH.Medage<-c(54.29,52.97,53.36,55.70,54.62,54.11,53.40,53.87,53.46,54.17)
# LL.Medage<-c(38.92,38.99,38.98,38.74,38.86,38.70,38.74,38.84,38.87,39.01)
# LH.Medage<-c(46.40,46.51,46.31,48.06,45.90,46.77,47.59,45.72,45.87,46.44)
# HL.Medage<-c(39.15,39.43,39.15,39.05,39.03,39.18,39.07,39.08,39.18,39.29)
# Neighborless.Medage<-c(48.25,43.61,45.54,39.86,37.67,41.38,NaN,37.50,42.89,37.31)

#ADJUSTED - Mean Neighborhood median age per LISA classes
NS.Medage<-c(43.38,43.24,42.76,42.90,42.92,43.20,43.04,42.68,42.65,42.51)
HH.Medage<-c(45.26,44.19,47.47,46.00,47.86,44.88,45.63,48.51,48.16,48.86)
LL.Medage<-c(39.05,39.08,39.06,38.88,39.08,38.56,38.85,38.92,38.85,39.40)
LH.Medage<-c(42.87,42.47,43.97,43.95,43.85,42.74,43.61,44.43,44.33,43.73)
HL.Medage<-c(39.68,39.95,39.61,40.71,39.72,39.87,39.73,39.73,39.80,40.52)
Neighborless.Medage<-c(48.25,43.61,45.54,39.86,37.67,41.38,NaN,37.50,42.89,37.31)

Medage<-tibble(NS.Medage,HH.Medage,LL.Medage,LH.Medage,HL.Medage)
# Minimum value
apply(Medage, 2, min)
# Maximum value
apply(Medage, 2, max)
# Mean value
round(colMeans(Medage),2)
# Standard deviation
round(apply(Medage, 2, sd),2)

# Significavity between LISA classes
#Convert to long dataframe
Medage <- Medage %>% mutate(replication = row_number()) #Add row index as column
Medage <- Medage %>% pivot_longer(-c(replication), names_to="cluster_type")
#Compute Tukey HSD test
model=lm(Medage$value ~ Medage$cluster_type )
ANOVA=aov(model)
TukeyHSD(x=ANOVA, 'Medage$cluster_type', conf.level=0.95)
