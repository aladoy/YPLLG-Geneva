'''
Assign median neighborhood income and neighborhood median age to GIREC polygons.
'''

#LIBRARIES
import pandas as pd
import geopandas as gpd
import numpy as np
import os
import statistics
import itertools

#IMPORT DATASET

#ANNUAL MEDIAN INCOME FOR MARRIED PEOPLE PER STATISTICAL SUB-SECTOR (GIREC)
income=pd.read_excel('../data/girec_income/ge_income_girec_2005_2016.xlsx',header=7, skipfooter=9)
#Remove the first two rows containing only nan values
income=income.iloc[2:]
#Convert Code Sous-secteur from float to integer
income['Code Sous-secteur']=income['Code Sous-secteur'].astype(int)
#Add prefix to specify that data concern income
income=income.add_prefix('income_')
#Convert to np.nan the values () which correspond to places where there is not enough people and ... which correspond to places where there is no married couple.
income=income.replace('( )',np.nan)
income=income.replace('…',np.nan)
print('Is the number of rows in the income dataframe equal to 475: ', income.shape[0]==475)



def compute_median_grouped(row, first_med, last_med, inter):

    if np.all(row.values[1:] == 0):
        median_grouped = np.nan

    else:
        med_group = list(range(first_med, last_med, inter))

        if len(med_group) != len(row.values[1:]):
            raise ValueError

        l=list()
        for i in range(0,len(med_group)):
            l.append(list(np.repeat(med_group[i], row.values[i+1])))

        final_list=list(itertools.chain(*l))
        median_grouped = round(statistics.median_grouped(final_list, inter), 2)

    return median_grouped



# RESIDENT POPULATION BY FIVE-YEAR AGE GROUP BY STATISTICAL SUB-SECTOR (GIREC)
pop_filename='../data/girec_population/T_01_01_4_04.xls'

for sheet_name in range(2005,2017):

    #Open corresponding file
    if sheet_name==2005 or sheet_name==2013: #additional footers for the years 2005 and 2013 (check the excel file for more details)
        pop_year=pd.read_excel(pop_filename, sheet_name=str(sheet_name), header=10, skipfooter=5)
    else:
        pop_year=pd.read_excel(pop_filename, sheet_name=str(sheet_name), header=10, skipfooter=3)

    # Convert - values to 0
    pop_year = pop_year.replace('-',0, regex=False)
    # Drop unecessary columns
    pop_year.drop(['Unnamed: 1','Total'], axis=1, inplace=True)
    # Rename columns
    pop_year.rename(columns = {'Unnamed: 0':'subsector_name'}, inplace = True)
    # Select only subsector (not sector) -> subsector start with 5 leading spaces
    pop_year=pop_year[(~pop_year.subsector_name.isna()) & (pop_year.subsector_name.str.startswith('     '))]
    # Remove leading and trailing space in subsector names
    pop_year['subsector_name']=pop_year['subsector_name'].str.strip()

    # Compute grouped median
    pop_year['medage']=pop_year.apply(lambda row: compute_median_grouped(row, 2, 105, 5), axis=1)

    # Remove uncessary columns
    pop_year = pop_year[['subsector_name', 'medage']]

    #Add suffix for year
    pop_year = pop_year.rename(columns={col: col+'_'+str(sheet_name) for col in pop_year.columns if col!='subsector_name'})

    #Merge resulting dataframe with final dataframe
    if sheet_name==2005:
        pop_medage=pop_year.copy()
    else:
        pop_medage=pop_medage.merge(pop_year, on='subsector_name',how='inner')


print('Is the number of rows in the pop_medage dataframe equal to 475: ', pop_medage.shape[0]==475)


#GIREC SHAPEFILE
girec=gpd.read_file('../data/girec_geo/sous_secteurs_475.shp')
girec.crs
print('Number of rows of the GIREC dataframe: ', girec.shape[0])


#MATCH GIREC WITH POPULATION AND INCOME LONGITUDINAL DATA

#Convert GIREC's NUMERO from object to integer
girec['NUMERO']=girec.NUMERO.astype(int)
#Convert subsector names to upper case
pop_medage['subsector_name']=pop_medage.subsector_name.map(str.upper)
girec['NOM']=girec.NOM.map(str.upper)

#Correct manually subsector names that does not match girec names
pop_medage.loc[pop_medage.subsector_name=='VERNIER VILLAGE','subsector_name']='VERNIER - VILLAGE'
pop_medage.loc[pop_medage.subsector_name=='PARC-BERTRAND','subsector_name']='PARC BERTRAND'
pop_medage.loc[pop_medage.subsector_name=='DE-BUDÉ','subsector_name']='DE-BUDE'
pop_medage.loc[pop_medage.subsector_name=='CONCHES - LA-PETITE-PAUMIÈRE','subsector_name']='CONCHES - LA PETITE-PAUMIÈRE'
pop_medage.loc[pop_medage.subsector_name=="SACONNEX D'ARVE - DESSUS",'subsector_name']="SACONNEX-D'ARVE - DESSUS"
pop_medage.loc[pop_medage.subsector_name=="SACONNEX D'ARVE - DESSOUS",'subsector_name']="SACONNEX-D'ARVE - DESSOUS"
pop_medage.loc[pop_medage.subsector_name=="COMMUNAUX D'AMBILLY",'subsector_name']="COMMUNAUX-D'AMBILLY"
pop_medage.loc[pop_medage.subsector_name=="VERSOIX-BOURG",'subsector_name']="VERSOIX - BOURG"
pop_medage.loc[pop_medage.subsector_name=="CRÈVE-CŒUR",'subsector_name']="CRÈVE-COEUR"
pop_medage.loc[pop_medage.subsector_name=="THÔNEX - EGLISE",'subsector_name']="THÔNEX - ÉGLISE"


#Merge GIREC and income
girec_w_info=girec.merge(income[income.columns.difference(['income_Sous-secteur'])],how='inner',left_on='NUMERO',right_on='income_Code Sous-secteur')

#Merge GIREC and population
girec_w_info=girec_w_info.merge(pop_medage,how='inner',left_on='NOM',right_on='subsector_name')

print('Is the number of rows in the merged dataframe equal to 475: ',girec_w_info.shape[0]==475)

#Drop non essential columns
girec_w_info.drop(['Code','Sous_sect','Contrib','Rev_median','income_Code Sous-secteur','subsector_name'],axis=1,inplace=True)

#Replace Nan values by 0
girec_w_info.loc[:,girec_w_info.columns[11:]]=girec_w_info[girec_w_info.columns[11:]].fillna(0)


#SAVE FILE
path='../processed_data/girec_info_2005_2016.gpkg'

if os.path.exists(path):
    os.remove(path)
girec_w_info.to_file(path,driver='GPKG')
