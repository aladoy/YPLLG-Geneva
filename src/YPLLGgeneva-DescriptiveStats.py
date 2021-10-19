#YPLLGgeneva-Descriptive_Stats.py

#IMPORT LIBRARIES
import pandas as pd
import geopandas as gpd
import numpy as np
import pickle
import seaborn as sns
import matplotlib.pyplot as plt

#IMPORT DATASETS
#Deads
deads = gpd.read_file('../processed_data/final_dataset_spatial_analysis.gpkg')
deads.shape
deads = pd.DataFrame(deads)

#Geneva deceased
stats = pd.read_csv('../data/ge_mortality/Deaths_Cantons_Sex_AgeGroup_Year.csv', sep=';')
stats = stats[stats.CANTON == '26']
stats = stats[(stats.YEAR >= 2009) & (stats.YEAR <= 2016)]
stats = stats[stats.SEX == 'T']
stats = stats[stats.AGE != '_T']

deads

#DISTRIBUTION OF YPLLG - COMPARISON BETWEEN TRANSVERSE AND LONGITUDINAL MORTALITY TABLES
sns.set()
plt.rcParams.update({'font.size': 17})
fig, ax = plt.subplots(figsize=(15,10))
ax=sns.distplot( deads.YPLLG_long , color="skyblue", label="with LEB estimated from cohort life tables")
ax=sns.distplot( deads.YPLLG_trans , color="red", label="with LEB estimated from period life tables")
plt.xlabel('YPLLG [yr]')
plt.legend()
plt.savefig("../results/distrib_YPLLG.png", dpi=200)
plt.show()

stats

#COMPARISON OF THE LEB PER GENDER AND LONGITUDINAL/TRANSVERSE TABLES
sns.set()
plt.rcParams.update({'font.size': 17})
fig, ax = plt.subplots(figsize=(15,10))
ax=sns.lineplot(x="Year", y="LEB",hue="Gender",style='Type',data=ofs_leb)
plt.ylabel('YPLLG')
plt.savefig("img/diff_mortality_tables.png")



#NUMBER OF DEATHS PER YEAR
nb_death_year=deads.DeathDay.dt.year.value_counts().sort_index()
nb_death_year

#COMPARISON WITH CANTONAL STATISTICS
plt.rcParams.update({'font.size': 17})
plt.figure(figsize=(15,10))
plt.plot(ge_stat.index,ge_stat['Total'],color="#0c457d",marker="o",label="cantonal statistics")
plt.plot(nb_death_year.index,nb_death_year.values,color='#e8702a',marker="o",label="dataset")
plt.xlabel('Year')
plt.ylabel('Number of deaths')
plt.legend()
plt.savefig("img/diff_cantonal_stat.png")
plt.show()


#COMPARISON OF THE LEB PER GENDER AND LONGITUDINAL/TRANSVERSE TABLES
sns.set()
plt.rcParams.update({'font.size': 17})
fig, ax = plt.subplots(figsize=(15,10))
ax=sns.lineplot(x="Year", y="LEB",hue="Gender",style='Type',data=ofs_leb)
plt.ylabel('LEB')
plt.savefig("img/diff_mortality_tables.png")


#DISTRIBUTION OF YPLLG - COMPARISON BETWEEN TRANSVERSE AND LONGITUDINAL MORTALITY TABLES
sns.set()
plt.rcParams.update({'font.size': 17})
fig, ax = plt.subplots(figsize=(15,10))
ax=sns.distplot( deads.YPLLG_long , color="skyblue", label="Longitudinal YPLLG")
ax=sns.distplot( deads.YPLLG_trans , color="red", label="Transverse YPLLG")
plt.xlabel('Life Expectancy Difference [yr]')
plt.legend()
plt.savefig("img/distrib_YPLLG.png")
plt.show()

#SUMMARY STATISTICS OF CONTINUOUS VARIABLES
pd.concat([deads.Birthday.dt.year, deads.DeathDay.dt.year, deads.LEB_long, deads.LEB_trans, deads.Death_age, deads.YPLLG_long, deads.YPLLG_trans], axis=1).describe().round(0).astype(int)

#PROPORTION OF DECEASED BY GENDER
#Convert Gender and Nationality into categorical variables
deads.Gender=deads.Gender.astype('category')
deads.Nationality=deads.Nationality.astype('category')
print(round(deads.groupby('Gender').size() * 100 / len(deads),2))

df_nat=deads.groupby('Nationality').size().reset_index(name='counts').sort_values('counts',ascending=False).set_index('Nationality')
df_nat['perc']=round(df_nat['counts']*100/deads.shape[0],2)
df_nat.head(20)
