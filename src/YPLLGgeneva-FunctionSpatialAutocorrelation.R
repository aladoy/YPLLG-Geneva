#Function spatial autocorrelation

spatial_autocorrelation <- function(data, var, dir_name, cutoff,perms,inverted_classes=F){

  #File to store results
  file_res=paste0(dir_name,'spatial_autocorrelation_results_',var,'.txt')

  #SPATIAL LAG

  #Convert to GeoDa object
  #print('a')
  #data.geoda <- rgeoda::sf_to_geoda(data) #Convert sf object to geoda object

  #Distance weights 1200m
  w <- distance_weights(data, 1200,is_inverse = FALSE)

  cat(paste0("Date:", Sys.Date(),'\n'), file = file_res, append = FALSE) #Overwrite the file

  if(inverted_classes==T){
    cat(paste0("Note: Classes in spatial autocorrelation statistics are inverted. Hence, High-High correspond to locations where the variable of interest is low and inversely.\n"), file = file_res, append = FALSE) #Overwrite the file
  }
  if(var=='adjusted_YPLLG' & inverted_classes==T){
    cat(paste0("However, for the adjusted LE only, Gi* classes were not inverted (we could not explain this phenomenon, but we get the same results in GeoDa)."),file = file_res, append = TRUE)
  }

  cat("\n\nSPATIAL WEIGHTS SUMMARY\n", file = file_res, append = TRUE)
  cat("\nMinimum Distance Threshold:", file = file_res, append = TRUE)
  capture.output(min_distthreshold(data), file=file_res, append=TRUE)
  capture.output(summary(w, zero.policy=TRUE), file=file_res, append=TRUE)


  # LOCAL MORAN --------------------------------------------------------------

  data.data <- as.data.frame(data)

  #Local Morans's I
  start_time <- Sys.time()
  lisa <- local_moran(w,data.data[var], permutations=perms, cpu_threads=15)
  end_time <- Sys.time()
  print(end_time-start_time)

  cat("\n\nMULTIPLE COMPARISONS CORRECTION\n", file = file_res, append = TRUE)
  cat(paste0('Bonferroni at alpha=0.1: ',0.1/nrow(data),'\n'), file = file_res, append = TRUE)
  cat(paste0('Bonferroni at alpha=0.05: ',0.05/nrow(data),'\n'), file = file_res, append = TRUE)

  #Save results

  if(cutoff=='alpha=0.1, Bonferroni'){
    data <- data %>% mutate(lisa_I=lisa_values(lisa), lisa_pval_nocorr=lisa_pvalues(lisa), lisa_clusters=lisa_clusters(lisa, cutoff=0.1))
    data <- data %>% mutate(lisa_pval_corr=p.adjust(lisa_pval_nocorr, method = 'bonferroni', n = length(lisa_pval_nocorr)))
    data$lisa_clusters[data$lisa_pval_corr>0.1] <- 0
  }else if(cutoff=='alpha=0.05, Bonferroni'){
    data <- data %>% mutate(lisa_I=lisa_values(lisa), lisa_pval_nocorr=lisa_pvalues(lisa), lisa_clusters=lisa_clusters(lisa, cutoff=0.05))
    data <- data %>% mutate(lisa_pval_corr=p.adjust(lisa_pval_nocorr, method = 'bonferroni', n = length(lisa_pval_nocorr)))
    data$lisa_clusters[data$lisa_pval_corr>0.05] <- 0
  }else if(cutoff=='alpha=0.05, FDR'){
    data <- data %>% mutate(lisa_I=lisa_values(lisa), lisa_pval_nocorr=lisa_pvalues(lisa), lisa_clusters=lisa_clusters(lisa, cutoff=0.05))
    data <- data %>% mutate(lisa_pval_corr=p.adjust(lisa_pval_nocorr, method = 'fdr', n = length(lisa_pval_nocorr)))
    data$lisa_clusters[data$lisa_pval_corr>0.05] <- 0
  }else if(cutoff=='alpha=0.01, FDR'){
    data <- data %>% mutate(lisa_I=lisa_values(lisa), lisa_pval_nocorr=lisa_pvalues(lisa), lisa_clusters=lisa_clusters(lisa, cutoff=0.01))
    data <- data %>% mutate(lisa_pval_corr=p.adjust(lisa_pval_nocorr, method = 'fdr', n = length(lisa_pval_nocorr)))
    data$lisa_clusters[data$lisa_pval_corr>0.01] <- 0
  }


  #For YPLLG, vulnerable populations correspond to individuals with high YPLLG so we switch all the LISA classes to have High-High corresponding to vulnerable pop.
  if(inverted_classes==TRUE){
    data<-data %>% mutate(lisa_clusters=case_when(.$lisa_clusters==0~0, .$lisa_clusters==1~2, .$lisa_clusters==2~1, .$lisa_clusters==3~4, .$lisa_clusters==4~3, .$lisa_clusters==5~5, .$lisa_clusters==6~6))
  }


  cat("\n\nLOCAL MORAN'S I SUMMARY\n", file = file_res, append = TRUE)
  cat(paste0('Not significant (NS): ',data %>% filter(lisa_clusters==0) %>% nrow(),'\n'), file = file_res, append = TRUE)
  cat(paste0('High-High (HH): ',data %>% filter(lisa_clusters==1) %>% nrow(),'\n'), file = file_res, append = TRUE)
  cat(paste0('Low-Low (LL): ',data %>% filter(lisa_clusters==2) %>% nrow(),'\n'), file = file_res, append = TRUE)
  cat(paste0('Low-High (LH): ',data %>% filter(lisa_clusters==3) %>% nrow(),'\n'), file = file_res, append = TRUE)
  cat(paste0('High-Low (HL): ',data %>% filter(lisa_clusters==4) %>% nrow(),'\n'), file = file_res, append = TRUE)
  cat(paste0('Undefined: ',data %>% filter(lisa_clusters==5) %>% nrow(),'\n'), file = file_res, append = TRUE)
  cat(paste0('Neighborless: ',data %>% filter(lisa_clusters==6) %>% nrow(),'\n'), file = file_res, append = TRUE)


  #Create boxplot for LISA classes
  # YPLLG
  options(scipen=10000)
  data.box <- data %>% filter(lisa_clusters %in% c(0,1,2,3,4)) %>% st_drop_geometry()
  data.box$lisa_clusters <- factor(data.box$lisa_clusters, levels=c(2,3,0,4,1), labels=c('LL','LH','NS','HL','HH'))
  (ggplot(data=data.box,aes_string(x='lisa_clusters',y=var)) + geom_boxplot() + xlab('LISA clusters classes') + ylab('YPLLG')) %>% ggsave(paste0(dir_name,var,'_lisa_classes.png'),.,dpi=200,width=148,height=105, units='mm')
  # Income (sector level)
  #(ggplot(data=data.box,aes_string(x='lisa_clusters',y='income_sect')) + xlab('LISA clusters classes') + ylab('Income (statistical sector level)') + geom_boxplot()) %>% ggsave(paste0(dir_name,'income_sect_lisa_classes.png'),.,dpi=200,width=148,height=105, units='mm')
  # Income (microregion level)
  #(ggplot(data=data.box,aes_string(x='lisa_clusters',y='income_micro')) + xlab('LISA clusters classes') + ylab('Income (microregion level)') + geom_boxplot()) %>% ggsave(paste0(dir_name,'income_micro_lisa_classes.png'),.,dpi=200,width=148,height=105, units='mm')


  #Rename factors
  data$lisa_clusters <- factor(data$lisa_clusters, levels=c(0,1,2,3,4,5,6), labels=c('NS','HH','LL','LH','HL','Undefined','Neighborless'))

  cat("\n\nPOPULATION CHARACTERISTICS \n", file = file_res, append = TRUE)
  cat("\nGender\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters,Gender) %>% summarise(N=n(), Percentage=round((100*n()/nrow(subset.sf)),2)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nNationality\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters, fct_explicit_na(Nationality, na_level = "(missing)"))  %>% summarise(N=n(), Percentage=round(100*n()/nrow(subset.sf),2)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nLifespan\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters) %>% summarise(meanLifespan=round(mean(Death_age),2), sdLifespan=round(sd(Death_age)),2) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nVariable of interest\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters) %>% summarise(!!(paste0('mean',quo_name(var))):=round(mean(!!as.name(quo_name(var))),2),!!paste0('sd',quo_name(var)):=round(sd(!!as.name(quo_name(var))),2)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nYPLL\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters) %>% summarise(meanYPLL=round(mean(YPLL),2), sdYPLL=round(sd(YPLL),2)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nIncome\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters) %>% summarise(meanIncome=round(mean(income),2), sdIncome=round(sd(income),2)) %>% as.data.frame(),file=file_res, append=TRUE)
  cat("\nPopulation median age\n", file = file_res, append = TRUE)
  capture.output(data %>% st_drop_geometry() %>% group_by(lisa_clusters) %>% summarise(meanMedage=round(mean(medage),2), sdMedage=round(sd(medage),2)) %>% as.data.frame(),file=file_res, append=TRUE)

  #Save to file
  res_file=paste0(dir_name,'results_lisa.gpkg')
  if (file.exists(res_file)){
    #Delete file if it exists
    file.remove(res_file)
  }
  st_write(data,res_file)

  return(data)

}
