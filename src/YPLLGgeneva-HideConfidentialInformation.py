# YPLLGgeneva-HideConfidentialInformation.py

# IMPORT LIBRARIES
import pandas as pd
import geopandas as gpd

# Initial dataset
raw_dataset=pd.read_excel('../data/death_notices/Datas_csv_excel.xlsx',sheet_name='Feuil1',index_col='id')
raw_dataset.loc[:,['LastName','FirstName','Birthday','DeathDay','CivilStatus','Street','AddressComplement']] = 'Protected'
raw_dataset.to_csv('../data/death_notices/Datas_csv_excel_protected.csv')

# Dataset used for analysis
dataset_analysis=pd.read_csv('../processed_data/final_dataset.csv',sep=';')
dataset_analysis.loc[:,['LastName','FirstName','Birthday','CivilStatus','DeathDay','FirstName_Only','Street','AddressComplement','Addr_full_concat']] = 'Protected'
dataset_analysis.to_csv('../processed_data/final_dataset_protected.csv')

# Spatial version of the dataset used for analysis
dataset_analysis=gpd.read_file('../processed_data/final_dataset_spatial_analysis.gpkg')
dataset_analysis.loc[:,['LastName','FirstName','Birthday','CivilStatus','DeathDay','FirstName_Only','Street','AddressComplement','Addr_full_concat']] = 'Protected'
dataset_analysis.to_file('../processed_data/final_dataset_spatial_analysis_protected.gpkg', driver='GPKG')
