#YPLLGgeneva-Cleaning.py

#IMPORT LIBRARIES
import pandas as pd
import pickle

#IMPORT DATA
deads_raw=pd.read_excel('../data/death_notices/Datas_csv_excel.xlsx',sheet_name='Feuil1',index_col='id')
deads_raw.head()
print(deads_raw.shape) #Shape of the dataset
print(min(deads_raw.Birthday),'-',max(deads_raw.Birthday)) #Birthday
print(min(deads_raw.DeathDay),'-',max(deads_raw.DeathDay)) #Deathday
deads_raw.index.is_unique #Check id is unique


#CLEANING

#Create a concatenated address
deads_raw['Street']=deads_raw.Street.str.lower()
deads_raw['City']=deads_raw.City.str.lower()
deads_raw['Addr_concat']=deads_raw['Street']+" "+deads_raw['City'].fillna('genève')

#Remove useless columns
deads_raw.drop(["LastUpdated","DeathPlace","TownshipId","AddressComplement"],axis=1,inplace=True)

## MISSING DATA
#Remove deads without birthday, deathday or street indication
print('wo birthday:', deads_raw[deads_raw.Birthday.isnull()].shape[0], '\nwo deathday:', deads_raw[deads_raw.DeathDay.isnull()].shape[0], '\nwo street:',  deads_raw[deads_raw.Street.isnull()].shape[0])
deads=deads_raw[(deads_raw.Birthday.isnull()==False) & (deads_raw.DeathDay.isnull()==False) & (deads_raw.Street.isnull()==False)]
print(deads.shape)

## DUPLICATED DATA
# def: same LastName/FirstName/Birthday/DeathDay or LastName/FistName/Birthday/Street or LastName/FistName/Birthday/DeathDay
# note: If different deadthday (or birthday), we decided to keep the oldest one
print('1st case:', len(deads[deads.duplicated(subset=['LastName','FirstName','Birthday','DeathDay'])==True]), '\n2nd case:', len(deads[deads.duplicated(subset=['LastName','FirstName','Birthday','Street'])==True]), '\n3rd case:', len(deads[deads.duplicated(subset=['LastName','FirstName','DeathDay','Street'])==True]))
deads=deads.drop_duplicates(subset=['LastName','FirstName','Birthday','DeathDay'])
deads=deads.drop_duplicates(subset=['LastName','FirstName','Birthday','Street'])
deads=deads.drop_duplicates(subset=['LastName','FirstName','DeathDay','Street'])
print(deads.shape)

#SAVE DATA
pickle.dump(deads,open('../processed_data/deads.p','wb'))
