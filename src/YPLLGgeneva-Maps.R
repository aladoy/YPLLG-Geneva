#Create maps in SVG (need to work with Inkscape after)

# LIBRARIES ---------------------------------------------------------------

#Basic
library(tidyverse)
library(here)
library(ggplot2)
library(ggsn)
library(ggtext)
library(pacman)
library(svglite)
#Spatial
library(sf)
library(lwgeom)



# MAPPING PARAMETERS ------------------------------------------------------

theme_map <- theme(
  panel.background=element_rect(fill="transparent"),
  plot.background=element_rect(fill="transparent",color="NA"),
  #plot.margin(25,25,25,25,unit='cm'),
  panel.grid.major=element_line(colour="transparent"),
  panel.grid.minor=element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  legend.background=element_blank(),
  legend.box.margin=margin(10,10,10,10),
  legend.box.background=element_rect(size=0,colour=NA,fill=NA),
  legend.title=element_text(size=24),
  legend.position=c(0.2,0.89),
  legend.spacing.x = unit(0.5, 'cm'),
  legend.key=element_rect(colour=NA,fill=NA),
  #legend.background = element_rect(colour = NA),
  legend.text=element_text(size=23),
  axis.text=element_blank(),axis.title=element_blank(),
  axis.ticks=element_blank())


#Include or remove an extra area of the map
xmin_extra <- -100
xmax_extra <- 100
ymin_extra <- -300
ymax_extra <- 500
#Size of the points
points_size <- 1.8
#scale bar distance
scale_dist <- 1.5
#location of the scale bar
scale_location <- "bottomright"
#Include or remove an extra margin area for the scale bar
scalexmin_extra <- 0
scalexmax_extra <- -1500
scaleymin_extra <- 1500
scaleymax_extra <- 0
#legend margins for Moran
themex_margin <- 0.16
themey_margin <- 0.86
#Text color of the scale bar
scale_color <- "black"
#Colors  of the scale bar
box_color1 <- "black"
box_color2 <- "white"
#Text size of the scale bar
scale_text_size <- 7




# FUNCTION TO CREATE MAPS -------------------------------------------------

svg_map <- function(dataset, dirname,subtitle){

  # #Add new variable to results for plotting order
  # dataset <- dataset %>% mutate(order=case_when(lisa_clusters=='Neighborless'~0,lisa_clusters=='Undefined'~1,lisa_clusters=='NS'~2,lisa_clusters=='HL'~3,lisa_clusters=='LH'~4,lisa_clusters=='LL'~5,lisa_clusters=='HH'~6))
  # dataset <- arrange(dataset,order)
  # print(dataset$order)

  bb <- st_bbox(c(xmin = 2482240, xmax = 2513704, ymax = 1130500, ymin = 1109202), crs = st_crs(2056))

  #IMPORT BACKGROUND LAYERS
  # Canton of Geneva
  ge <- st_read('/mnt/data/GEOSAN/RESEARCH PROJECTS/YPLLG GENEVA @ GIRAPH (EPFL)/YPLLG-geneva/data/administrative_boundaries_ge/canton_geneva.geojson') %>% st_crop(bb)

  # Lake Geneva
  lake <- st_read('/mnt/data/GEOSAN/RESEARCH PROJECTS/YPLLG GENEVA @ GIRAPH (EPFL)/YPLLG-geneva/data/lake_geo/GEO_LAC.shp')
  st_crs(lake) <- 2056
  st_is_valid(lake) #Check validity of the layer
  lake <- st_make_valid(lake) #Fix invalid geometries
  lake <- st_crop(lake, bb)

  #Crop dataset according to boundinb box for plotting
  dataset_plot<- st_crop(dataset, bb)

  #LOCAL MORAN'S MAP
  map_moran <- ggplot(dataset_plot) +
    #Map of the city being studied
    geom_sf(data=ge,fill='#404040',colour='#404040')+
    #Water map of the city being studied"
    geom_sf(data=lake,fill='#c6ccd7',colour='#666666', lwd=0.1)+
    #Plot the location of all observations,
    geom_point(aes(geometry=geom,fill=lisa_clusters,colour=lisa_clusters),stat="sf_coordinates",size=points_size,shape=21)+
    scale_colour_manual(values=c(
      #color values for dots margins (classes are inverted (if necessary) in the spatial autocorrelation function so no need to switch the colors)
      # Neighborless
      #"Neighborless"='#a6a6a6',
      #NS (white)
      "NS"='#f5f5f5',
      #LH (light purple)
      "LH"='#cdb6d8',
      #HL (light green)
      "HL"='#b7e2b2',
      #LL (dark purple)
      "LL"='#8b38a7',
      #HH (dark green)
      "HH"='#00a241'))+
    scale_fill_manual(
      #Legend text, I am using ggtext here to setup the text based on markdowns format, so "__" is for bold text, <br> for a new line etc, "p\U2264" is the code for "equal or lower than"
      name=paste0("<span style='font-size:30pt'>__Local Moran cluster map__</span><br/>",subtitle," (n=",format(nrow(dataset),nsmall=0,big.mark = ","),")<br/>","Spatial lag: 1,200m<br/>","Significance level: 0.1<br/>Bonferroni correction included <br/> (99,999 permutations)"),
      #Reorder values in legend, so HH are the ones that first appear in the legend, then HL, NS, LH and LL
      breaks=c("NS","HH","LL","LH","HL"),
      #Color values for dots (put HH and LL above)
      values=c(
        #6 - Neighborless
        #"Neighborless"='#a6a6a6',
        #0 - NS (white)
        "NS"='#f5f5f5',
        #1 - HH (dark green)
        "HH"='#00a241',
        #2 - LL (dark purple)
        "LL"='#8b38a7',
        #3 - LH (light purple)
        "LH"='#cdb6d8',
        #4 - HL (light green)
        "HL"='#b7e2b2'),
      #Labels, based on the order of the legend. Here I am also including the calculation of the N for each cluster
      labels=c(
        #paste0("Neighborless"," (n=",dataset %>% filter(lisa_clusters=='Neighborless') %>% nrow() %>% format(nsmall=0,big.mark = ","),")"),
        paste0("Not significant"," (n=",dataset %>% filter(lisa_clusters=='NS') %>% nrow() %>% format(nsmall=0,big.mark = ","),")"),
        paste0("High-High"," (n=",dataset %>% filter(lisa_clusters=='HH') %>% nrow() %>% format(nsmall=0,big.mark = ","),")"),
        paste0("Low-Low"," (n=",dataset %>% filter(lisa_clusters=='LL') %>% nrow() %>% format(nsmall=0,big.mark = ","),")"),
        paste0("Low-High"," (n=",dataset %>% filter(lisa_clusters=='LH') %>% nrow() %>% format(nsmall=0,big.mark = ","),")"),
        paste0("High-Low"," (n=",dataset %>% filter(lisa_clusters=='HL') %>% nrow() %>% format(nsmall=0,big.mark = ","),")")))+
    labs(title=NULL,x=NULL,y=NULL)+
    theme_map+
    #Increase the size of dots in legend and remove the border colors legend
    guides(fill=guide_legend(override.aes=list(size=5)),colour=F)+
    #Elemnt_markdown is to indicate that we are using the ggtext format for the legend
    theme(legend.title=element_markdown(),
          #I used a different legend position for Getis and Moran so they can be plotted in the same location
          legend.position=c(themex_margin,themey_margin))+
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(expand = c(0,0))+
    annotate(geom='text',x=2502328, y=1122295, label="Lake Geneva", fontface = "italic", color = "#404040", size = 7)+
    annotate(geom='text',x=2488550, y=1121263, label="France", fontface = "italic", color = "#404040", size = 9)+
    annotate(geom='text',x=2505973, y=1115906, label="France", fontface = "italic", color = "#404040", size = 9)+
    annotate(geom='text',x=2504720, y=1114074, label="Arve river", fontface = "italic", color = "#404040", size = 7)+
    annotate(geom='text',x=2486190, y=1112960, label="Rhône\nriver", fontface = "italic", color = "#404040", size = 7)+
    #Scale bar
    ggsn::scalebar(x.min=unname(bb$xmin)+scalexmin_extra,x.max=unname(bb$xmax)+scalexmax_extra,y.min=unname(bb$ymin)+scaleymin_extra,y.max=unname(bb$ymax)+scaleymax_extra,dist=scale_dist,dist_unit="km",location=scale_location,height=0.0075,st.size=scale_text_size,transform=F,st.color=scale_color,box.fill=c(box_color1,box_color2))

    svglite(filename=paste0(dirname,'lisa_map.svg'), width=20, height=15, standalone = FALSE, always_valid = TRUE)
    map_moran
    #ggsave(file=paste0(dirname,'lisa_map.svg'),plot=map_moran,width=20,height=20,units="in",dpi=200,bg="white")
    #ggsave(file=paste0(dirname,'lisa_map_low_res.png'),plot=map_moran,width=20,height=20,units="in",dpi=90,bg="white")



  #GETIS-ORD GI* MAP

  # #Reswitch
  # dataset<-dataset %>% mutate(localgstar_clusters=case_when(.$localgstar_clusters==0~0, .$localgstar_clusters==1~2, .$localgstar_clusters==2~1, .$localgstar_clusters==3~3, .$localgstar_clusters==4~4))
  #
  # #Redo boxplot
  # data.box <- dataset %>% filter(localgstar_clusters %in% c(0,1,2)) %>% st_drop_geometry()
  # data.box$localgstar_clusters <- factor(data.box$localgstar_clusters, levels=c(1,0,2), labels=c('Hot spot','Non significant','Cold spot'))
  # (ggplot(data=data.box,aes_string(x='localgstar_clusters',y='adjusted_YPLLG')) + geom_boxplot() + xlab('Getis-Ord clusters') + ylab('Life Expectancy')) %>% ggsave(paste0(here(dirname),'adjusted_YPLLG_getis_classes.png'),.,dpi=200,width=148,height=105, units='mm')
  #
#
#
#   map_gstar <- ggplot(dataset) +
#     geom_sf(data=ge,fill='#352f44ff',colour='#352f44ff')+
#     geom_sf(data=lake,fill='#c6ccd7',colour='#666666ff', lwd=0.1)+
#     geom_point(aes(geometry=geom,fill=as.factor(localgstar_clusters),colour=as.factor(localgstar_clusters)),stat="sf_coordinates",size=points_size,shape=21)+
#     scale_colour_manual(values=c(
#       #NS (white)
#       "0"='#F5F5F5',
#       #Undefined
#       "3"='#404040',
#       #Neighborless
#       "4"='#a6a6a6',
#       #Hot spot
#       "1"='#d7191c',
#       #Cold spot
#       "2"='#2c7bb6'))+
#     scale_fill_manual(
#       name=paste0("<span style='font-size:30pt'>__Local Getis-Ord Gi*__</span><br/>", subtitle," (n=",nrow(dataset),")<br/>","Spatial lag: 1200m<br/>","Significance level: 0.1 with Bonferroni correction <br/> (99999 permutations)"),
#       breaks=c("0","1","2","3","4"),
#       values=c(
#         "0"='#F5F5F5',
#         "1"='#d7191c',
#         "2"='#2c7bb6',
#         "3"='#404040',
#         "4"='#a6a6a6'),
#       labels=c(
#         paste0("Not significant"," (n=",nrow(dataset[dataset$localgstar_clusters=="0",]),")"),
#         paste0("Hot spots"," (n=",nrow(dataset[dataset$localgstar_clusters=="1",]),")"),
#         paste0("Cold spots"," (n=",nrow(dataset[dataset$localgstar_clusters=="2",]),")"),
#         paste0("Undefined"," (n=",nrow(dataset[dataset$localgstar_clusters=="3",]),")"),
#         paste0("Neighborless"," (n=",nrow(dataset[dataset$localgstar_clusters=="4",]),")")))+
#     labs(title=NULL,x=NULL,y=NULL)+
#     theme_map+
#     guides(fill=guide_legend(override.aes=list(size=5)),colour=F)+
#     theme(legend.title=element_markdown(),
#           legend.position=c(themegx_margin,themegy_margin))+
#     scale_y_continuous(expand = c(0,0))+
#     scale_x_continuous(expand = c(0,0))+
#     annotate(geom='text',x=2502328, y=1122295, label="Lake Geneva", fontface = "italic", color = "#352f44ff", size = 7)+
#     annotate(geom='text',x=2488550, y=1121263, label="France", fontface = "italic", color = "#352f44ff", size = 9)+
#     annotate(geom='text',x=2505973, y=1115906, label="France", fontface = "italic", color = "#352f44ff", size = 9)+
#     annotate(geom='text',x=2504720, y=1114074, label="Arve river", fontface = "italic", color = "#352f44ff", size = 7)+
#     annotate(geom='text',x=2486190, y=1112960, label="Rhône\nriver", fontface = "italic", color = "#352f44ff", size = 7)+
#     ggsn::scalebar(x.min=unname(bb$xmin)+scalexmin_extra,x.max=unname(bb$xmax)+scalexmax_extra,y.min=unname(bb$ymin)+scaleymin_extra,y.max=unname(bb$ymax)+scaleymax_extra,dist=scale_dist,dist_unit="km",location=scale_location,height=0.0075,st.size=scale_text_size,transform=F,st.color=scale_color,box.fill=c(box_color1,box_color2))
#
#     ggsave(file=paste0(dirname,'getis_map.svg'),plot=map_gstar,width=20,height=20,units="in",dpi=200,bg="white")
#     ggsave(file=paste0(dirname,'getis_map_low_res.png'),plot=map_gstar,width=20,height=20,units="in",dpi=90,bg="white")
#
#
#
 }



# IMPORT DATA AND RUN FUNCTION --------------------------------------------

#Load results (Geopackage file)
adjusted <- T
subset <- 8
var <- 'YPLLG_long'

#Import results
dirname <- if_else(adjusted==F,paste0('/mnt/data/GEOSAN/RESEARCH PROJECTS/YPLLG GENEVA @ GIRAPH (EPFL)/YPLLG-geneva/results/raw_',var,'/',subset,'/'), paste0('/mnt/data/GEOSAN/RESEARCH PROJECTS/YPLLG GENEVA @ GIRAPH (EPFL)/YPLLG-geneva/results/adjusted_',var,'/',subset,'/'))
results <- st_read(paste0(here(dirname),'results_lisa.gpkg'))
st_crs(results) <- 2056


#Create maps
if(var=='YPLLG_long'){
  subtitle <- if_else(adjusted==F, paste0('Raw YPLLG - Subset ',subset), paste0('Adjusted YPLLG - Subset ',subset))
}else if(var=='YPLL'){
  subtitle<-paste0('YPLL, Subset ',subset)
}

svg_map(results,dirname,subtitle=subtitle)

dev.off()
