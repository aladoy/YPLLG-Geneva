#YPLLGgeneva-Wrangling.py

#IMPORT LIBRARIES
import pandas as pd
import numpy as np
import pickle
import glob

#IMPORT DATASET (cleaned data)
deads = pickle.load(open(r"../processed_data/deads.p","rb"))
print(deads.shape)

#AGE AT DEATH
# def: Age at death = (DeathDay - BirthDay) / 365.2425 where 365.2425 is the average number of days in a year according to the Gregorian calendar
deads['Death_age']=deads.apply(lambda row : (((row.DeathDay-row.Birthday).days)/365.2425),axis=1).round().astype(int)

#BIRTH YEAR
deads['Birthyear']=deads['Birthday'].dt.year

#MISSING GENDER

#1. Extract FirstName
#Add the firstname (no second name) and the occurence in the dataset to the deads dataframe
deads_firstnames=pd.DataFrame(deads.FirstName.apply(str).str.split(' ',1).str[0])
deads['FirstName_Only']=deads_firstnames #new column in deads df
deads_firstnames=deads_firstnames['FirstName'].value_counts().to_frame().rename(columns={'FirstName':'Occurence'}) #occurence by firstname
deads=deads.merge(deads_firstnames,left_on='FirstName_Only',right_index=True) #Add the occurence to the deads df
#Save unique firstname list in csv
pd.DataFrame(deads.FirstName_Only.unique()).to_csv('data/deads_unique_firstnames.csv')

#2.Use Genderize.io API to extract the gender and associated probability for each firstname (limited to 1000 requests a day)
def get_gender(firstname,country_id=None):
    if country_id :
        r=requests.get('https://api.genderize.io/?name='+firstname+'&country_id='+country_id)
    else :
        r=requests.get('https://api.genderize.io/?name='+firstname)
    try:
        gender=r.json()['gender']
    except:
        gender=np.nan
    try:
        prob=r.json()['probability']
    except:
        prob=np.nan
    return gender, prob
#Example to extract results
#firstname_unique_part=firstname_unique[4683:]
#firstname_unique_part.columns=['FirstName']
#firstname_unique_part.is_copy = False
#firstname_unique_part['Gender'],firstname_unique_part['Probability']=zip(*firstname_unique_part['FirstName'].map(get_gender))
#firstname_unique_part.to_csv('data/gender_api_results/result_gender_4683_end.csv')

#3. Concatenate results
path = "../processed_data/gender_api_results/"
result_gender_files = glob.glob(path+"*.csv")
result_gender= pd.concat(pd.read_csv(f, header=0, index_col=0) for f in result_gender_files)
result_gender.to_csv('../processed_data/gender_api_results_concat.csv')

#4. For probabilities below 0.9, run again the api for country=France (name e.g. Jean has different gender according to countries)
# note: France is taken instead of Switzerland because of data numbers in the API
#to_test_France=result_gender[result_gender.Probability<0.9]
#to_test_France=to_test_France.drop(['Gender','Probability'],axis=1)
#to_test_France.is_copy = False
#to_test_France['Gender'],to_test_France['Probability']=zip(*[get_gender(names,'fr') for names in to_test_France['FirstName']])
#to_test_France.to_csv('../processed_data/result_gender_testFrance_below90.csv')
result_gender_France=pd.read_csv('../processed_data/result_gender_testFrance_below90.csv',index_col=0)
result_gender_France=result_gender_France.dropna(axis=0) #drop missing first names
result_gender.update(result_gender_France) #replace gender with prob < 0.9 with this new information
deads=deads.merge(result_gender,left_on='FirstName_Only',right_on='FirstName').rename(columns={'FirstName_x':'FirstName'}).drop('FirstName_y',axis=1) #Merge with deads dataframe

# 5. Add manually some genders (since 1258 genders (26.15%) are still unknown)
to_find=deads[(pd.isnull(deads['Gender'])) & (deads['Occurence']>3)].sort_values('Occurence',ascending=False)
to_find=to_find[['FirstName_Only','Occurence']].drop_duplicates()
to_find=to_find.assign(Gender = ['female','female','male','male','male','female','male','male','male','male','female','female','female','female','female','male','female','female','female','female'])
to_find=to_find.assign(Probability=1).drop('Occurence',axis=1)

#Merge the results with the deads df
deads=deads.merge(to_find,how='left',left_on='FirstName_Only',right_on='FirstName_Only')
deads['Gender_x']=np.where(deads['Gender_x'].isnull(),deads['Gender_y'],deads['Gender_x'])
deads['Probability_x']=np.where(deads['Probability_x'].isnull(),deads['Probability_y'],deads['Probability_x'])
deads=deads.drop(['Gender_y','Probability_y'],axis=1)
deads=deads.rename(columns={'Gender_x':'Gender','Probability_x':'Probability'})

# 6. Remove from the dataset deads with gender prob. <0.9
deads_wgender=deads[deads.Probability>=0.9]
deads_wgender.loc[:,'Gender']=deads_wgender['Gender'].astype('category')

# IMPORT MORTALITY TABLES

#Import longitudinal mortality tables
ofs_long=pd.read_csv('../data/life_expectancy_ofs/ofs_leb_longitudinal.csv',delimiter=',',skiprows=1 ,encoding='iso-8859-1').drop(['Age'],axis=1)
ofs_long.columns=['Year','Gender','LEB']
ofs_long['Year']=ofs_long['Year'].astype(int)
ofs_long['Type']='Longitudinal'

#Import transverse mortality tables
ofs_trans=pd.read_csv('../data/life_expectancy_ofs/ofs_leb_transverse.csv',delimiter=',',skiprows=1 ,encoding='iso-8859-1').drop(['Age'],axis=1)
ofs_trans.columns=['Year','Gender','LEB']
ofs_trans['Year']=ofs_trans['Year'].astype(int)
ofs_trans['Type']='Transverse'

#Concatenate the two mortality tables
ofs_LEB=pd.concat([ofs_long,ofs_trans],ignore_index=True)
ofs_LEB['Gender'].replace({'Homme':'male', 'Femme':'female'},inplace=True)

# COMPUTE LIFE EXPECTANCY DIFFERENCE
# def: YPLLG=Age at death - LEB
#Remove deaths < 2003 (year from which we start to have consistent information with the census data)
deads_wgender=deads_wgender[deads_wgender.DeathDay.dt.year>=2003]
#Remove deads having inconsistent lifespan
deads_wgender=deads_wgender[deads_wgender.Death_age<110]

#Add the Life Expectancy at birth with Mortality tables for every deceased
#w. longitudinal mortality table
deads_wLE=deads_wgender.merge(ofs_LEB[ofs_LEB.Type=='Longitudinal'],how='left',left_on=['Birthyear','Gender'],right_on=['Year','Gender']).rename(columns={'LEB':'LEB_long'}).drop(['Year','Type'],axis=1)
#w. tranverse mortality table
deads_wLE=deads_wLE.merge(ofs_LEB[ofs_LEB.Type=='Transverse'],how='left',left_on=['Birthyear','Gender'],right_on=['Year','Gender']).rename(columns={'LEB':'LEB_trans'}).drop(['Year','Type'],axis=1)

#Check
print('Number of missing LE_long (should be equal to 0):', deads_wLE[deads_wLE.LEB_long.isnull()].shape[0])
print('Number of missing LE_trans (should be equal to 0):', deads_wLE[deads_wLE.LEB_trans.isnull()].shape[0])

#Compute the Life Expectancy Difference
deads_wLE['YPLLG_long']=deads_wLE['Death_age']-deads_wLE['LEB_long']
deads_wLE['YPLLG_trans']=deads_wLE['Death_age']-deads_wLE['LEB_trans']
deads_wLE.shape

#SAVE DATA
pickle.dump(deads_wLE,open('../processed_data/deads_wYPLLG.p','wb'))
pickle.dump(ofs_LEB,open('../processed_data/ofs_LEB.p','wb'))
