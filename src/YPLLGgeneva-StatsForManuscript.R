#YPLLGgeneva-StatsForManuscript.R

# LIBRARIES ---------------------------------------------------------------

#Basic
library(tidyverse)
library(here)
#Spatial
library(sf)
library(ggplot2)

# IMPORT DATA --------------------------------------------------------

# Deads
data.sf <- st_read('../processed_data/final_dataset_spatial_analysis.gpkg')
st_crs(data.sf)<-2056
data <- data.sf %>% st_drop_geometry() # create non spatial dataset for statistics



# DISTRIBUTION OF OUTCOME VARIABLES ---------------------------------------

distribution_plot <- ggplot(data = data, mapping = aes(YPLLG_long)) +
  geom_histogram(fill = '#8caabe', color='grey', size=0.5) +
  labs(
    x = "YPLLG [y]", # x axis title
    y = "Count",   # y axis title     # main title of figure
  ) +
  theme_bw(base_size=10)

ggsave(filename = "../results/YPLLG_distribution.png", plot = distribution_plot, width = 15, height = 10, dpi = 300, units = "cm")


# DATASET DESCRIPTION -----------------------------------------------------

# Gender
data <- data %>% mutate(Gender=as.factor(Gender))
data %>% group_by(Gender) %>% summarise(count=n(), perc=round(n()*100/nrow(data),2))

# Nationality
data <- data %>% mutate(Nationality=as.factor(Nationality))
data %>% group_by(Nationality) %>% summarise(count=n(), perc=round(n()*100/nrow(data),2))

# Median neighborhood income
summary(data$income)
sd(data$income)

# Age at death
mean(data$Death_age)
sd(data$Death_age)

# YPLLG
summary(data$YPLLG_long)
sd(data$YPLLG_long)

# Test statistical difference for YPLLG between gender
data %>% group_by(Gender) %>% summarise(median(YPLLG_long))
wilcox.test(YPLLG_long ~ Gender, data = data) #is median statistically different

# Test statistical difference for YPLLG between nationality
data %>% group_by(Nationality) %>% summarise(median(YPLLG_long))
wilcox.test(YPLLG_long ~ Nationality, data = data) #is median statistically different

# Test statistical difference for Neighborhood income between gender
data %>% group_by(Gender) %>% summarise(median(income))
wilcox.test(income ~ Gender, data = data) #is median statistically different

# Test statistical difference for neighborhood population age between gender
data %>% group_by(Gender) %>% summarise(median(medage))
wilcox.test(medage ~ Gender, data = data) #is median statistically different

# Test statistical difference for Neighborhood income between nationality
data %>% group_by(Nationality) %>% summarise(median(income))
wilcox.test(income ~ Nationality, data = data) #is median statistically different

# Test statistical difference for neighborhood population age between nationality
data %>% group_by(Nationality) %>% summarise(median(medage))
wilcox.test(medage ~ Nationality, data = data) #is median statistically different



#Comparison for raw YPLLG
res= st_read('../outputs/adjusted_YPLLG_long/5/results_lisa_getis.gpkg')

vary='res$adjusted_YPLLG'
varx='res$lisa_clusters'
file_name<-'tukeyTest_category_duration'
model=lm(as.formula(paste0(vary,'~',varx)))
anova=aov(model)
tukey<- TukeyHSD(x=anova, varx, conf.level=0.95)
png(paste0(here(),file_name,'.png'),dpi=200,width=100,height=90, units='mm')
plot(tukey , las=1 , col="brown",cex.axis=0.6)
dev.off()
