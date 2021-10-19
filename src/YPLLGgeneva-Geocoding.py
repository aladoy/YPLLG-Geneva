#YPLLGgeneva-Geocoding.py

#IMPORT LIBRARIES
import pandas as pd
import numpy as np
import pickle
import geopandas as gpd
import difflib

#IMPORT DATASET
deads = pickle.load(open("../processed_data/deads_wYPLLG.p","rb")) #(32326, 22)
cad = gpd.read_file('../data/ge_addresses/CAD_ADRESSE.shp',encoding='utf-8') # (53123, 19) contains the unique identifier (IDPADR) and the complete address (ADRESSE) for all the canton of Geneva (extraction date: February 2020)
cad_archive = gpd.read_file('../data/ge_addresses/CAD_ADRESSE_ARCHIVE.shp',encoding='utf-8') # (18327, 22) contains all the addresses modifications for a specific IDPADR, with the old address (ADRESSE_OLD) and the new one (ADRESSE_NEW) (extraction date: February 2020)

deads['ZipCode']=deads['ZipCode'].fillna(0).astype('int')

#DATA WRANGLING ON ADDRESSES
cad=cad[['IDPADR','TYPE','ADRESSE','NO_POSTAL','COMMUNE','geometry']] #Keep only useful columns
cad_archive=cad_archive[['IDPADR','ADRESSE_OL','ADRESSE_NE','NPA_OLD','NOM_NPA_OL','DATEMO','geometry']] #Keep only useful columns

cad['Addr_full_concat']=cad['ADRESSE'].str.lower()+" "+cad['NO_POSTAL'].astype('str')+" "+cad['COMMUNE'].str.lower()
cad['Addr_concat']=cad['ADRESSE'].str.lower()+" "+cad['COMMUNE'].str.lower()
cad['ADRESSE']=cad['ADRESSE'].str.lower()

cad_archive['Addr_full_concat']=cad_archive['ADRESSE_OL'].str.lower()+" "+cad_archive['NPA_OLD'].astype('str')+" "+cad_archive['NOM_NPA_OL'].str.lower()
cad_archive['Addr_concat']=cad_archive['ADRESSE_OL'].str.lower()+" "+cad_archive['NOM_NPA_OL'].str.lower()
cad_archive['ADRESSE_OL']=cad_archive['ADRESSE_OL'].str.lower()

#DROP DUPLICATED ADDRESSES
cad.drop(cad[(cad.duplicated(subset=['Addr_full_concat'],keep=False)) & (cad.TYPE.str.contains('Projet'))].index,inplace=True) #(remove "Project" addresses). 1448 were duplicated and 737 were kept
cad_archive.drop(cad_archive[cad_archive.duplicated(subset=['ADRESSE_OL'],keep='first')].index,inplace=True) #(keep first one as it is the last modification). 6225 were duplicated and 3396 were kept

#GEOCODING PROCESS
deads.insert(0, 'ID', deads.index) #Create an ID

#1. Merge on full adress (street+commune) -> 10990 on 32326
df1=deads.merge(cad,how='left',left_on='Addr_concat',right_on='Addr_concat',suffixes=['_deads','_cad'])
df1=df1[df1.ADRESSE.isnull()==False]
df1.drop(['TYPE','ADRESSE','NO_POSTAL','COMMUNE','Addr_concat'],axis=1,inplace=True)
df1.shape

#2. Merge on street + zip code -> 87 on 21336
df2 = deads[~deads.ID.isin(df1.ID)].merge(cad,how='left',left_on=['Street','ZipCode'],right_on=['ADRESSE','NO_POSTAL'],suffixes=['_deads','_cad'])
df2=df2[df2.ADRESSE.isnull()==False]
df2.drop(['TYPE','ADRESSE','NO_POSTAL','COMMUNE','Addr_concat_cad','Addr_concat_deads'],axis=1,inplace=True)
df2.shape

#3.Merge with cad_archive -> 428 on 21249
df3=deads[~deads.ID.isin(pd.concat([df1,df2],sort=False).ID)].merge(cad_archive,how='left',left_on=['Addr_concat'],right_on=['Addr_concat'],suffixes=['_deads','_cad_archive'])
df3=df3[df3.ADRESSE_OL.isnull()==False]
df3.drop(['ADRESSE_OL','ADRESSE_NE','NPA_OLD','NOM_NPA_OL','DATEMO','Addr_concat'],axis=1,inplace=True)
df3.shape

#4. Merge on address -> 11273 on 20821
df4=deads[~deads.ID.isin(pd.concat([df1,df2,df3],sort=False).ID)].merge(cad,how='left',left_on=['Street'],right_on=['ADRESSE'],suffixes=['_deads','_cad'])
df4=df4[df4.ADRESSE.isnull()==False]
df4.drop(df4[(df4.duplicated(subset='ID',keep=False)) & (df4.COMMUNE.str.contains('Genève-')==False)].index, inplace=True) #Remove duplicated rows
df4.drop(['TYPE','ADRESSE','NO_POSTAL','COMMUNE','Addr_concat_cad','Addr_concat_deads'],axis=1,inplace=True)
df4.shape
#We have verified that number of rows with left join = number of rows with inner join

#5. Remove foreign countries (1217 on 9548)
df5=deads[~deads.ID.isin(pd.concat([df1,df2,df3,df4],sort=False).ID)]
df5=df5[(df5.Country=='CH') | (df5.Country.isnull())]
df5.shape

#6. Remove streets that don't have a complete address (i.e. no street number) (352 on 8331)
df5=df5[df5.Street.str.contains('[0-9]')]
df5.shape

#7.Fuzzy matching (0.8 similarity cutoff) (6288 on 7968)
df5['fuzzy_match']=df5.Addr_concat.map(lambda x: difflib.get_close_matches(x,cad.Addr_concat,1,0.8))
df5['fuzzy_match']=df5.fuzzy_match.str[0]
df6=df5[df5.fuzzy_match.isnull()==True] #Create new dataframe with non-matching rows
df5=df5[df5.fuzzy_match.isnull()==False] #Remove non-matching rows
df5=df5.merge(cad,how='left',left_on='fuzzy_match',right_on='Addr_concat',suffixes=['_deads','_cad'])
df5.drop(['fuzzy_match','TYPE','ADRESSE','NO_POSTAL','COMMUNE','Addr_concat_cad','Addr_concat_deads'],axis=1,inplace=True)
df5.shape

#8. Fuzzy matching on street only (0.8 similarity cutoff)
df6.shape
df6['fuzzy_match']=df6.Street.map(lambda x: difflib.get_close_matches(x,cad.ADRESSE,1,0.8))
df6['fuzzy_match']=df6.fuzzy_match.str[0]
df6=df6.merge(cad,how='left',left_on='fuzzy_match',right_on='ADRESSE',suffixes=['_deads','_cad'])
df6.shape
df6.drop_duplicates(subset='ID', keep=False,inplace=True) #Remove ALL duplicates (n=5)
df6=df6[df6.fuzzy_match.isnull()==False]
df6.drop(['fuzzy_match','TYPE','ADRESSE','NO_POSTAL','COMMUNE','Addr_concat_cad','Addr_concat_deads'],axis=1,inplace=True)
df6.shape

#CONCATEBATE FINAL RESULTS
final_dataset=pd.concat([df1,df2,df3,df4,df5,df6],sort=False)

#REMOVE ABSURD ROWS
#People < 18 y/o and married / widowers / divorced
final_dataset.drop(final_dataset[(final_dataset.Death_age<18) & ((final_dataset.CivilStatus!='Célibataire') & (final_dataset.CivilStatus!='célibataire') & (final_dataset.CivilStatus.notnull()))].index, inplace=True)
final_dataset.shape

#SAVE RESULTS
final_dataset.to_csv('../processed_data/final_dataset.csv',sep=';',index=False)
