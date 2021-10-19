'''
Compare our dataset with cantonal statistics at the municipality level.
'''

#LIBRARIES
import pandas as pd
import geopandas as gpd
import numpy as np
import os
from shapely import wkt

#IMPORT DATASET

#CANTONAL STATISTICS
stats=pd.read_csv('../data/ge_mortality/Deaths_Municipalities_Sex_Citizenship_Year.csv', sep=';', encoding='iso-8859-1')
stats.columns=['year','municipality','sex','citizenship', 'deaths']

#Remove rows that correspond to Total stats
stats=stats[stats.municipality.str.startswith('......')]

#Remove 2017 statistics
stats=stats[stats.year!=2017]

#Extract municipality name and number
stats[['municipality_no','name']]=stats.municipality.str.split(' ',expand=True)[[0,1]]
stats['name']=stats.name.map(str.strip)
stats['municipality_no']=stats.municipality_no.str.replace('.','').str.strip().astype('int')
stats.drop('municipality',axis=1,inplace=True)

#Convert to tidy dataframe
#stats=pd.melt(stats, id_vars=['year','sex','citizenship','municipality_no','name'], value_vars=['<20yrs','20_29yrs','30_39yrs','40_49yrs','50_59yrs','60_69yrs','70_79yrs','80_89yrs','>90yrs'])
stats=stats[['municipality_no','year','sex','citizenship','deaths']]
stats=stats.sort_values(['municipality_no','year'])


#DECEASED
deaths=pd.read_csv('../processed_data/final_dataset.csv',sep=';')

#Remove deceased before 2009
deaths['DeathYear']=deaths.DeathDay.map(pd.to_datetime).dt.year
deaths=deaths[deaths.DeathYear>=2009]
deaths=deaths[deaths.DeathYear<2017]
deaths.shape

#Convert columns values to match cantonal stats
deaths.loc[deaths.Nationality != 'CH', 'Nationality']= 'Foreign country'
deaths.loc[deaths.Nationality == 'CH', 'Nationality']= 'Switzerland'
deaths.loc[deaths.Gender == 'male', 'Gender']= 'Male'
deaths.loc[deaths.Gender == 'female', 'Gender']= 'Female'

#Convert to spatial dataframe
deaths['geometry'] = deaths.geometry.apply(wkt.loads)
deaths_geo=gpd.GeoDataFrame(deaths, geometry='geometry', crs={'init': 'epsg:2056'})
print('Size of geodataframe = Size of initial dataframe: ', deaths_geo.shape[0]==deaths.shape[0])

#GENEVA MUNICIPALITIES
mun=gpd.read_file('../data/administrative_boundaries_ge/GE municipalities/CAD_COMMUNE.shp',encoding='utf-8')
mun.crs

print('Do all municipalities have correspondance with the stats dataframe: ', mun[mun.NO_COM_FED.isin(stats.municipality_no)==False].shape[0]==0)


#COMPUTE NUMBER OF DEATHS BY MUNICIPALITIES

#Merge the deaths dataframe with municipalities
deaths_by_mun=gpd.sjoin(deaths_geo, mun, how="left", op="within")
print('Are all deaths located within the municipalities: ', deaths_by_mun[deaths_by_mun.NO_COM_FED.isna()].shape[0]==0)

#Group by municipalities
deaths_by_mun=deaths_by_mun[['ID','Nationality','Gender','DeathYear','NO_COM_FED']]
deaths_by_mun=pd.DataFrame(deaths_by_mun.groupby(by=['NO_COM_FED','Gender','Nationality','DeathYear']).size().reset_index(drop=False))
deaths_by_mun.rename(columns={0:'Deaths'},inplace=True)


#COMPARE OFFICIAL STATISTICS AND OUR DATASET

#Merge dataframes deaths_by_mun and stats
stats_combined=pd.merge(stats,deaths_by_mun,how='left',left_on=['municipality_no','year','sex','citizenship'],right_on=['NO_COM_FED','DeathYear','Gender','Nationality'])
stats_combined.drop(['NO_COM_FED','Gender','Nationality','DeathYear'],axis=1,inplace=True)
stats_combined.rename(columns={'deaths':'ofs','Deaths':'study'},inplace=True)

#Fill missing values by 0
stats_combined['study']=stats_combined.study.fillna(0)

#Compute difference between statistics
stats_combined['diff']=stats_combined['ofs']-stats_combined['study'].astype(np.uint8)

#Convert study to int
stats_combined['study']=stats_combined['study'].astype(np.uint8)

#Add municipality name
stats_combined=pd.merge(stats_combined,mun[['NO_COM_FED','COMMUNE']],how='left',left_on='municipality_no',right_on='NO_COM_FED').drop('NO_COM_FED',axis=1)
stats_combined.rename(columns={'COMMUNE':'name'},inplace=True)
stats_combined.drop('municipality_no',axis=1,inplace=True)

#Change Nationality format
stats_combined.loc[stats_combined.citizenship=='Switzerland','citizenship']='Swiss'
stats_combined.loc[stats_combined.citizenship=='Foreign country','citizenship']='Non Swiss'

#Reshape dataframe
stats_combined_table=stats_combined.pivot_table(index=["name", "sex", "citizenship"],columns=["year"],values=["ofs","study","diff"])
stats_combined_table=stats_combined_table.swaplevel(axis='columns')
stats_combined_table.sort_index(axis=1, level=0, inplace=True)


#ADD STYLE TO DATAFRAME
#Compute max and min values for differnce (to use in style)
max_diff=stats_combined['diff'].max()
min_diff=stats_combined['diff'].min()

#Reorder levels
stats_combined_table=stats_combined_table.reindex(['ofs', 'study', 'diff'], level=1, axis=1)

#Define style
style=stats_combined_table.style.highlight_between(subset=stats_combined_table.columns.get_level_values(1)=='diff',left=1, right=max_diff,axis=1, color="#fffd75")
#Export to latex
print(style.to_latex(sparse_columns=True, sparse_index=True, siunitx=False, convert_css=True))
