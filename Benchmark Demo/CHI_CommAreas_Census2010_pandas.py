#!/usr/bin/env python3
import os

import pandas as pd
from matplotlib import pyplot as plt
import seaborn

cd = os.path.join(os.path.dirname(__file__))
pd.set_option('display.width', 1000)


#################
## IMPORT
#################
census_df = pd.read_csv(os.path.join(cd, 'Data', 'CHI_CommAreas_Census2010.csv'), skiprows=1)


#################
## EXTRACT
#################
def extract_fct():
    global race_df
    race_df = census_df.filter(regex='Geog|Hispanic', axis=1)
    
    global age_df
    age_df = census_df.filter(regex='Geog|years', axis=1)
      
    global gender_df
    gender_df = census_df.filter(regex='Geog|Total|Male|Female', axis=1)
      
    global educ_df
    educ_df = pd.read_csv(os.path.join(cd, 'Data', 'CHI_CommAreas_Educ2010.csv'))\
                 .filter(regex='Geog|Total', axis=1)
      
    global hhold_df
    hhold_df = census_df.filter(regex='Geog|Household', axis=1)
      
    global housing_df
    housing_df = census_df.filter(regex='Geog|Total\\.Housing|Occupied|Vacant', axis=1)\
                                .drop(['Occupied Housing Units.1'], axis=1) 
    
    global occupied_df
    occupied_df = census_df.filter(regex='Geog|Owned|Renter', axis=1)


#################
## RESHAPE
#################

def reshape_fct():
    # RACE
    global race_long_df
    race_long_df = race_df.melt(id_vars=['Geog', 'GeogKey'], var_name='Race_Ethnicity', value_name='Population')
    
    
    # AGE
    global age_long_df
    age_df[[i.replace('Male', 'Age') for i in age_df.columns if "Male" in i]] = \
                  age_df.filter(regex="Male", axis=1).\
                      rename(columns={i:i.replace('Male', 'Age') for i in age_df.columns if "Male" in i}) + \
                  age_df.filter(regex="Female", axis=1).\
                      rename(columns={i:i.replace('Female', 'Age') for i in age_df.columns if "Female" in i})
    
    
    age_long_df = age_df.filter(regex="Geog|Age", axis=1).melt(id_vars=['Geog', 'GeogKey'], 
                                                               var_name='Age_Group', value_name='Population')
    
    # GENDER
    global gender_long_df
    gender_df["male"] = gender_df.filter(regex="Male", axis=1).apply(sum, axis=1)
    gender_df["female"] = gender_df.filter(regex="Female", axis=1).apply(sum, axis=1)
    
    gender_long_df = gender_df.filter(regex='Geog|(^male)|female', axis=1).\
                               melt(id_vars=['Geog', 'GeogKey'], var_name='Gender', value_name='Population')
    
    
    # EDUCATION
    global educ_long_df
    educ_long_df = educ_df.melt(id_vars=['Geog', 'GeogKey'], var_name='Education_Level', value_name='Population')
    
    
    # HOUSEHOLDS
    global hholds_long_df
    hholds_long_df = hhold_df
    
    
    # HOUSING
    global housing_long_df
    housing_long_df = housing_df.melt(id_vars=['Geog', 'GeogKey'], var_name='Occupied_Status', value_name='Housing_Units')
    
    # OCCUPIED
    global occupied_long_df
    occupied_long_df = occupied_df.melt(id_vars=['Geog', 'GeogKey'], var_name='Occupied_Type', value_name='Housing_Units')
    

#################
## SUMMARIZE
#################

def summarize_fct():
    # COMM_AREA
    comm_agg_ser = census_df['Total Population'].agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    
    # RACE
    race_agg_df = race_long_df.groupby('Race_Ethnicity')['Population']\
                                       .agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    race_agg_df = race_agg_df.rename(columns={col:col+'_population' for col in race_agg_df.columns})
    
    # AGE
    age_agg_df = age_long_df.groupby('Age_Group')['Population']\
                                     .agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    age_agg_df = age_agg_df.rename(columns={col:col+'_population' for col in age_agg_df.columns})
    
    
    # GENDER
    gender_agg_df = gender_long_df.groupby('Gender')['Population']\
                                           .agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    gender_agg_df = gender_agg_df.rename(columns={col:col+'_population' for col in gender_agg_df.columns})
    
    
    # EDUCATION
    educ_agg_df = educ_long_df.groupby('Education_Level')['Population']\
                                       .agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    educ_agg_df = educ_agg_df.rename(columns={col:col+'_population' for col in educ_agg_df.columns})
    
    
    # HOUSEHOLDS
    hholds_agg_ser = hholds_long_df['Total Households'].agg({'mean', 'median', 'min', 'max', 'sum'})
                                               
    # HOUSING
    housing_agg_df = housing_long_df.groupby('Occupied_Status')['Housing_Units']\
                                       .agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    housing_agg_df = housing_agg_df.rename(columns={col:col+'_housing_units' for col in housing_agg_df.columns})
    
    # OCCUPIED
    occupied_agg_df = occupied_long_df.groupby('Occupied_Type')['Housing_Units']\
                                       .agg({'mean', 'median', 'min', 'max', 'sum'})
                                       
    occupied_agg_df = occupied_agg_df.rename(columns={col:col+'_occupied_units' for col in occupied_agg_df.columns})
    
    return [comm_agg_ser, race_agg_df, age_agg_df, gender_agg_df, educ_agg_df, hholds_agg_ser, housing_agg_df, occupied_agg_df]
    

#################
## GRAPHING
#################

seaborn.set()
    
    
def graphing_fct():
    def runplot(pvtdf, ytitle, main_title):    
        
        fig = pvtdf.plot(kind='bar', edgecolor='w',figsize=(15,5), width=0.5, fontsize = 10)
        locs, labels = plt.xticks()    
        plt.title(main_title, weight='bold', size=24)
        lgd = plt.legend(loc='right', ncol=5, frameon=True, shadow=False, prop={'size': 12},
                         bbox_to_anchor=(0.87, -0.35))

        for i in range(len(lgd.get_texts())):        
            txt = lgd.get_texts()[i].get_text().replace('(Population, ', '(').replace('(Housing_Units, ', '(')
            lgd.get_texts()[i].set_text(txt)
    
        plt.xlabel('Chicago Community Areas', weight='bold', size=24)
        plt.ylabel(ytitle, weight='bold', size=20)
        plt.tick_params(axis='x', bottom='off', top='off', labelsize=15)
        plt.tick_params(axis='y', left='off', right='off', labelsize=15)
            
        plt.grid(b=True)
        plt.setp(labels, rotation=0, rotation_mode="anchor", ha="center")
        plt.tight_layout()
        #plt.show()
        plt.clf()
        
        return fig            
        
    plot_list = []
    
    # COMM AREA
    grp_df = census_df['Total Population'].nlargest(10)\
                  .to_frame().reset_index().rename_axis(None)\
                  .join(census_df[['Geog']]).set_index('Geog')
                  
    plot_list.append(runplot(grp_df, 'Population', 'Top Ten Chicago CAs By Population'))
 
    # RACE
    for g in race_long_df['Race_Ethnicity'].unique():
        
        grp_df = race_long_df[race_long_df['Race_Ethnicity']==g].groupby(['Race_Ethnicity'])['Population'].nlargest(10)\
                      .to_frame().reset_index().set_index('level_1').rename_axis(None)\
                      .join(race_long_df[['Geog']])\
                      .pivot_table(index=['Race_Ethnicity'], columns=['Geog'], values=['Population'])
    
        plot_list.append(runplot(grp_df, 'Population', 'Top Ten Chicago CAs - {}'.format(g.replace('Not Hispanic or Latino,', ''))))
    
    

    # AGE
    for g in age_long_df['Age_Group'].unique():
        
        grp_df = age_long_df[age_long_df['Age_Group']==g].groupby(['Age_Group'])['Population'].nlargest(10)\
                      .to_frame().reset_index().set_index('level_1').rename_axis(None)\
                      .join(age_long_df[['Geog']])\
                      .pivot_table(index=['Age_Group'], columns=['Geog'], values=['Population'])
    
        plot_list.append(runplot(grp_df, 'Population', 'Top Ten Chicago CAs - {}'.format(g)))
    
    plt.close()

    # GENDER
    for g in gender_long_df['Gender'].unique():
        
        grp_df = gender_long_df[gender_long_df['Gender']==g].groupby(['Gender'])['Population'].nlargest(10)\
                      .to_frame().reset_index().set_index('level_1').rename_axis(None)\
                      .join(gender_long_df[['Geog']])\
                      .pivot_table(index=['Gender'], columns=['Geog'], values=['Population'])
    
        plot_list.append(runplot(grp_df, 'Population', 'Top Ten Chicago CAs - {}'.format(g)))
    
    plt.close()

    # EDUCATION
    for g in educ_long_df['Education_Level'].unique():
        
        grp_df = educ_long_df[educ_long_df['Education_Level']==g].groupby(['Education_Level'])['Population'].nlargest(10)\
                      .to_frame().reset_index().set_index('level_1').rename_axis(None)\
                      .join(educ_long_df[['Geog']])\
                      .pivot_table(index=['Education_Level'], columns=['Geog'], values=['Population'])
    
        plot_list.append(runplot(grp_df, 'Population', 'Top Ten Chicago CAs - {}'.format(g)))
    
    plt.close()

    # HOUSEHOLDS
    grp_df = hholds_long_df['Total Households'].nlargest(10)\
                  .to_frame().reset_index().rename_axis(None)\
                  .join(hholds_long_df[['Geog']]).set_index('Geog')
                  
    runplot(grp_df, 'Households', 'Top Ten Chicago CAs By Households')

    # HOUSING
    for g in housing_long_df['Occupied_Status'].unique():
        
        grp_df = housing_long_df[housing_long_df['Occupied_Status']==g].groupby(['Occupied_Status'])['Housing_Units'].nlargest(10)\
                      .to_frame().reset_index().set_index('level_1').rename_axis(None)\
                      .join(housing_long_df[['Geog']])\
                      .pivot_table(index=['Occupied_Status'], columns=['Geog'], values=['Housing_Units'])
    
        plot_list.append(runplot(grp_df, 'Housing Units', 'Top Ten Chicago CAs - {}'.format(g)))
    
    plt.close()


    # OCCUPIED
    for g in occupied_long_df['Occupied_Type'].unique():
        
        grp_df = occupied_long_df[occupied_long_df['Occupied_Type']==g].groupby(['Occupied_Type'])['Housing_Units'].nlargest(10)\
                      .to_frame().reset_index().set_index('level_1').rename_axis(None)\
                      .join(occupied_long_df[['Geog']])\
                      .pivot_table(index=['Occupied_Type'], columns=['Geog'], values=['Housing_Units'])
    
        plot_list.append(runplot(grp_df, 'Occupied Units', 'Top Ten Chicago CAs - {}'.format(g)))
    
    plt.close()

    return plot_list
