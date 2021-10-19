#YPLLGgeneva-SpatialAnalysis_RandomSample.R

#The idea is to select 10 times a random subset of 10 000 observations in the dataset (~1/3)
# and run Getis and LISA with Bonferoni correction at a 5% significance level.

# LIBRARIES ---------------------------------------------------------------

#Basic
library(tidyverse)
library(here)
library(quantreg)
#Spatial
library(sf)
library(rgeoda)
library(tmap)
source('YPLLGgeneva-FunctionSpatialAutocorrelation.R')
source('YPLLGgeneva-FunctionJoinGirecInfos.R')


# PARAMETERS --------------------------------------------------------------

adjusted <- FALSE
var <- 'YPLLG_long'
signif <- 'alpha=0.1, Bonferroni' #or 'alpha=0.1, Bonferroni', 'alpha=0.05, Bonferroni', 'alpha=0.05, FDR', 'alpha=0.01, FDR'

global_path= '/home/lasigadmin/anais/'

#path to store results
dir <- if_else(adjusted==T, paste0(global_path,'YPLLG-geneva/results/adjusted_',var,'/'), paste0(global_path,'YPLLG-geneva/results/raw_',var,'/'))
dir.create(dir,showWarnings = FALSE) #create dir


# IMPORT DATA --------------------------------------------------------

# Deads
data <- read_delim(paste0(global_path,'YPLLG-geneva/processed_data/final_dataset.csv'),';',col_names=TRUE)
data.sf <- data %>% st_as_sf(wkt='geometry')
st_crs(data.sf)<-2056

#Import GIREC infos (covariates for median regression)
# Annual median income (2005-2016) for married couples per statistical sector, % or neighborhood median age
girec_stats <- st_read(paste0(global_path,'YPLLG-geneva/processed_data/girec_info_2005_2016.gpkg'))
st_crs(girec_stats)<-21781
girec_stats.sf <- st_transform(girec_stats,2056) #Reproject in the same CRS than the others layers (EPSG:2056)

# DATA WRANGLING ----------------------------------------------------------

# set.seed(1)
# subset.sf <- sample_n(data.sf, 10000)
# subdir <- paste0(dir,1,'/')
# st_write(subset.sf,paste0(subdir,'subset1.gpkg'))

#Add years of life lost (75-Age at death)
data.sf <- data.sf %>% mutate(YPLL=75-Death_age)
data.sf$YPLL <- ifelse(data.sf$YPLL < 0, 0, data.sf$YPLL)

#Convert Gender / Nationality as factors
data.sf<- data.sf %>% mutate(Gender=as.factor(Gender),Nationality=as.factor(if_else(Nationality=='CH','swiss','non_swiss')))

#Add individuals' death year
data.sf<-data.sf %>% mutate(DeathYear=as.numeric(format(DeathDay,'%Y')))

#Remove individuals that die before 2009 and after 2016 (not consistent with federal statistics)
data.sf<-data.sf %>% filter(DeathYear>=2009 & DeathYear<=2016)

#Remove individuals with missing nationality
data.sf <- data.sf %>% drop_na(Nationality) %>% st_as_sf(sf_column_name='geometry')

#SPATIAL JOIN WITH GIREC INFOS
data.sf <- join_info(data.sf, girec_stats.sf) %>% select(!c('NO_COM_FED','NO_COMM','CODE_SECTE','SECT_VILLE','NUMERO','CODE_SOUS_','SHAPE_Leng','SHAPE_Area'))

#Save the dataset used in analysis
res_file=paste0(global_path,'YPLLG-geneva/processed_data/final_dataset_spatial_analysis.gpkg')
if (file.exists(res_file)){
  #Delete file if it exists
  file.remove(res_file)
}
st_write(data.sf,res_file)

#Select random numbers for seeds
#seeds<-sample.int(100000, 10)
seeds<-c(17078,82599,85211,37996,24735,56295,38386,11340,5490,89289)

for (i in 2){

  #Create random sample
  set.seed(seeds[i]) #use seed to obtain same samples if we run again the script
  subset.sf <- sample_n(data.sf, 10000)

  #Subdirectory to store results for a given subset
  subdir <- paste0(dir,i,'/')
  dir.create(subdir,showWarnings = FALSE)

  #DESCRIPTIVE ANALYSIS

  #Create file to store results
  file_res=paste0(subdir,'descriptive_stats.txt')
  cat(paste0("Date:", Sys.Date(),'\n'), file = file_res, append = FALSE) #Overwrite the file

  # #Plot variable
  # tmap_mode('view')
  # p.var<-tm_shape(subset.sf) + tm_dots(col=var, midpoint=0) + tm_layout(frame=F, title=paste0('Individuals from subset',i,' n=',nrow(subset.sf)))
  # tmap_save(p.var, paste0(subdir,var,'.html'), selfcontained=TRUE) #Need to move the map in correct directory after
  #

  #Median regression if adjusted YPLLG
  if(adjusted==T){
    model <- rq(formula=YPLLG_long~income+medage+Nationality,tau=0.5,data=subset.sf)
    #add residuals of both models to the dataframe
    cat('\n\nREGRESSION MODEL \n', file=file_res, append=TRUE)
    capture.output(summary(model,se = "boot"), file=file_res, append=TRUE) #use a bootstrap approach to compute standard errors (error otherwise)
    subset.sf <- subset.sf %>% mutate(adjusted_YPLLG=model$residuals) %>% st_as_sf()
    var <- 'adjusted_YPLLG' #Change var to run spatial autocorrelation
  }

  #Histogram
  (ggplot(data=subset.sf, aes_string(x=var)) + geom_histogram(binwidth = 5)) %>% ggsave(paste0(subdir,var,'_histogram.png'),.,dpi=200,width=148,height=105, units='mm')

  #Select variable of interest (+ attributes to compute descriptive statistics of the population)
  subset.sf <- subset.sf %>% select("ID", "Gender","Nationality","Death_age",var,"YPLL","income","medage","geometry")

  #Add parameters to results file
  cat('\n\nPARAMETERS \n', file=file_res, append=TRUE)
  if(adjusted==F){
    cat(paste0('Variable of interest: Raw ',var,'\n'), file = file_res, append = TRUE)
  }else{
    cat(paste0('Variable of interest: Adjusted ',var,'\n'), file = file_res, append = TRUE)
  }

  cat(paste0('Number of observations included:',nrow(subset.sf),'\n'), file = file_res, append = TRUE)
  cat(paste0('Significance level (signif):',signif,'\n'), file = file_res, append = TRUE)
  capture.output(subset.sf %>% select(var) %>% summary(), file=file_res, append=TRUE)

  cat("\n\nPOPULATION CHARACTERISTICS \n", file = file_res, append = TRUE)
  cat("\nGender\n", file = file_res, append = TRUE)
  capture.output(subset.sf %>% st_drop_geometry() %>% group_by(Gender) %>% summarise(N=n(), Percentage=(100*n()/nrow(subset.sf))) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nNationality\n", file = file_res, append = TRUE)
  capture.output(subset.sf %>% st_drop_geometry()  %>% group_by(fct_explicit_na(Nationality, na_level = "(missing)"))  %>% summarise(N=n(), Percentage=100*n()/nrow(subset.sf)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nLifespan\n", file = file_res, append = TRUE)
  capture.output(subset.sf %>% st_drop_geometry() %>% summarise(meanLifespan=mean(Death_age), sdLifespan=sd(Death_age)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nVariable of interest\n", file = file_res, append = TRUE)
  capture.output(subset.sf %>% st_drop_geometry() %>% summarise(!!(paste0('mean',quo_name(var))):=mean(!!as.name(quo_name(var))),!!paste0('sd',quo_name(var)):=sd(!!as.name(quo_name(var)))) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nIncome\n", file = file_res, append = TRUE)
  capture.output(subset.sf %>% st_drop_geometry() %>% summarise(meanIncome=mean(income), sdIncome=sd(income)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nNeighborhood median age (medage)\n", file = file_res, append = TRUE)
  capture.output(subset.sf %>% st_drop_geometry() %>% summarise(meanMedage=mean(medage), sdMedage=sd(medage)) %>% as.data.frame(),file=file_res, append=TRUE)

  #SPATIAL AUTOCORRELATION STATISTICS

  #Run spatial autocorrelation statistics (Local Moran's and Getis-Ord Gi*). User-defined function
  subset.sf <- spatial_autocorrelation(subset.sf,var,subdir,signif,99999,inverted_classes=FALSE)


}
