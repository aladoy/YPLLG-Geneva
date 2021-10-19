#Function to run a spatial join with GIREC's info (income and medage)

find_col_var <- function(variable,nearest1,nearest2,nearest3,nearest4,nearest5,nearest6,nearest7,nearest8,deathyear){
  
  #Add new column in var_value corresponding to rownames
  var_value <- girec_stats.sf %>% st_drop_geometry() %>% rownames_to_column() %>% mutate(rowname=as.integer(rowname)) #Here, we could also use OBJECTID which already correspond to rownames
  
  col=paste0(variable,'_',deathyear)

  if((var_value %>% filter(rowname==nearest1) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest1) %>% pull(var=col)
  }else if((var_value %>% filter(rowname==nearest2) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest2) %>% pull(var=col)
  }else if((var_value %>% filter(rowname==nearest3) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest3) %>% pull(var=col)
  }else if((var_value %>% filter(rowname==nearest4) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest4) %>% pull(var=col)
  }else if((var_value %>% filter(rowname==nearest5) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest5) %>% pull(var=col)
  }else if((var_value %>% filter(rowname==nearest6) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest6) %>% pull(var=col)
  }else if((var_value %>% filter(rowname==nearest7) %>% pull(var=col)) !=0 ){
    income=var_value %>% filter(rowname==nearest7) %>% pull(var=col)
  }else{
    income=var_value %>% filter(rowname==nearest8) %>% pull(var=col)
  }
  
  return(income)
}

join_info <- function(dat, girec_infos){
  
  require(nngeo)
  require(tidyverse)
  require(sf)
  
  #Spatial join between individuals and the GIREC where they lived
  dat<-st_join(dat, girec_infos, join=st_within, left=T)
  
  #Create new column with income_DeathYear (tried DeathYear %in% seq(from=2005, to=2016) ~ pull(dat, paste0('income_',DeathYear))) but did not work
  dat<-dat %>% mutate(income=case_when(
    DeathYear==2009 ~ income_2009,
    DeathYear==2010 ~ income_2010,
    DeathYear==2011 ~ income_2011,
    DeathYear==2012 ~ income_2012,
    DeathYear==2013 ~ income_2013,
    DeathYear==2014 ~ income_2014,
    DeathYear==2015 ~ income_2015,
    DeathYear==2016 ~ income_2016, 
  ))
  
  #Same for neighborhood median age
  dat<-dat %>% mutate(medage=case_when(
    DeathYear==2009 ~ medage_2009,
    DeathYear==2010 ~ medage_2010,
    DeathYear==2011 ~ medage_2011,
    DeathYear==2012 ~ medage_2012,
    DeathYear==2013 ~ medage_2013,
    DeathYear==2014 ~ medage_2014,
    DeathYear==2015 ~ medage_2015,
    DeathYear==2016 ~ medage_2016, 
  ))
  
  
  #dat <- dat %>% mutate(income=pull(dat,matches(dat,sym(income_col))))

  #Remove duplicated rows
  #dat %>% filter(duplicated(.[["ID"]])) #Print duplicated rows
  #dat <- dat %>% distinct(ID,.keep_all=TRUE) #Remove duplicated rows (keep first one)
  #print(nrow(dat)) #Should be equal to 30631

  #FILL ROWS WITH MISSING INCOME (income==0)
  dat.missing_inc <- dat %>% filter(income==0)
  print(paste0('Number of individuals with missing income at the sector level: ',nrow(dat.missing_inc)))
  #Find the first 8 closest NBID
  nearest<- st_nn(dat.missing_inc,girec_infos,k=9,returnDist = F)
  #Add the 8 nearest neighbors in dat.missing_inc dataframe
  dat.missing_inc <- dat.missing_inc %>% mutate(nearest1=sapply(nearest , "[[", 2), nearest2=sapply(nearest , "[[", 3), nearest3=sapply(nearest , "[[", 4), nearest4=sapply(nearest , "[[", 5), nearest5=sapply(nearest , "[[", 6), nearest6=sapply(nearest , "[[", 7),nearest7=sapply(nearest , "[[", 8),nearest8=sapply(nearest , "[[", 9)) %>% select(-income) #Add rownames of nearest NBID to subset.sfframe
  #Run the function to extract the income for the closest neighbor (if the first neighbor has NaN values for the given year, use the second neighbor, etc.)
  dat.missing_inc$income_nearest<-mapply(find_col_var, 'income', dat.missing_inc$nearest1, dat.missing_inc$nearest2, dat.missing_inc$nearest3, dat.missing_inc$nearest4, dat.missing_inc$nearest5, dat.missing_inc$nearest6,dat.missing_inc$nearest7, dat.missing_inc$nearest8, dat.missing_inc$DeathYear)
  #Left join with initial subset.sfframe
  dat.filled_inc<-dat %>% st_drop_geometry() %>% left_join(dat.missing_inc %>% select(ID,income_nearest), by='ID')
  dat.filled_inc<-dat.filled_inc %>% select(-geometry) %>% left_join(dat %>% select(ID,geometry), by='ID') #Add geometry (dropped in left_join)
  #Fill with nearest income value
  dat<-dat.filled_inc %>% mutate(income=if_else(income==0,income_nearest,income)) %>% st_as_sf(sf_column_name='geometry')
  
  #FILL ROWS WITH MISSING medage (medage==0)
  dat.missing_medage <- dat %>% filter(medage==0)
  print(paste0('Number of individuals with missing medage at the sector level: ',nrow(dat.missing_medage)))
  #Find the first 8 closest NBID
  nearest<- st_nn(dat.missing_medage,girec_infos,k=9,returnDist = F)
  #Add the 8 nearest neighbors in dat.missing_medage dataframe
  dat.missing_medage <- dat.missing_medage %>% mutate(nearest1=sapply(nearest , "[[", 2), nearest2=sapply(nearest , "[[", 3), nearest3=sapply(nearest , "[[", 4), nearest4=sapply(nearest , "[[", 5), nearest5=sapply(nearest , "[[", 6), nearest6=sapply(nearest , "[[", 7),nearest7=sapply(nearest , "[[", 8),nearest8=sapply(nearest , "[[", 9)) %>% select(-medage) #Add rownames of nearest NBID to subset.sfframe
  #Run the function to extract the medage for the closest neighbor (if the first neighbor has NaN values for the given year, use the second neighbor, etc.)
  dat.missing_medage$medage_nearest<-mapply(find_col_var, 'medage', dat.missing_medage$nearest1, dat.missing_medage$nearest2, dat.missing_medage$nearest3, dat.missing_medage$nearest4, dat.missing_medage$nearest5, dat.missing_medage$nearest6,dat.missing_medage$nearest7, dat.missing_medage$nearest8, dat.missing_medage$DeathYear)
  #Left join with initial subset.sfframe
  dat.filled_medage<-dat %>% st_drop_geometry() %>% left_join(dat.missing_medage %>% select(ID,medage_nearest), by='ID')
  dat.filled_medage<-dat.filled_medage %>% select(-geometry) %>% left_join(dat %>% select(ID,geometry), by='ID') #Add geometry (dropped in left_join)
  #Fill with nearest medage value
  dat<-dat.filled_medage %>% mutate(medage=if_else(medage==0,medage_nearest,medage)) %>% st_as_sf(sf_column_name='geometry')

  #Remove useless columns & convert to sf object
  dat<-dat %>% select(-c(medage_nearest,income_nearest)) %>% select(-starts_with('income_')) %>% select(-starts_with('pop_'))  %>% select(-starts_with('medage_')) %>% st_as_sf(sf_column_name='geometry')
  
  #Save intermediate file to GPKG
  res_file=paste0('../processed_data/dataset_used_with_covariates.gpkg')
  if (file.exists(res_file)){
    #Delete file if it exists
    file.remove(res_file)
  }
  st_write(dat,res_file)
  
  return(dat)
}

