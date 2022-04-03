from asyncore import write
from cProfile import label
from operator import index
from pickle import TRUE
from tkinter import Grid
from unittest import result
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from skimpy import skim

file = 'titanic.csv'
titanic = pd.read_csv(file)
skim(titanic)
titanic.head()
#View Columns with missing values
titanic.isnull().sum()
 
#Correct age for missing to show as mean
titanic['Age'].fillna(titanic['Age'].mean(), inplace=True)
 
#Combine SibSp and Parch columns
titanic['Household_total'] = titanic['SibSp'] + titanic['Parch']
 
#Drop unnecessary vairables
titanic.drop(['PassengerId', 'SibSp', 'Parch'], axis = 1, inplace = True)
 
#Fill in missing and create indicator for Cabin variable
titanic.isnull().sum()
titanic['Cabin_ind'] = np.where(titanic['Cabin'].isnull(), 0, 1)

#Wealthy Variable Creation
Fare_Avg = titanic['Fare'].mean()
titanic['Wealthy_Pclass'] = np.where((titanic['Pclass'] == 1), 1, 0)
titanic['Wealthy_Fare'] = np.where((titanic['Fare'] > Fare_Avg), 1, 0)

titanic['Wealthy'] = np.logical_and(titanic['Wealthy_Pclass'] == 1, titanic['Wealthy_Fare'] == 1)
titanic['Wealthy'] = np.where((titanic['Wealthy'] == False), 0, 1)

titanic.drop(['Wealthy_Pclass', 'Wealthy_Fare'], axis=1, inplace = True)

#Wealthy Location Variable


#Pclass and Cabin assignment
titanic['ThirdClass_Cabin'] = np.logical_and(titanic['Pclass'] == 3, titanic['Cabin_ind'] == 1)
titanic['ThirdClass_Cabin'] = np.where((titanic['ThirdClass_Cabin'] == False), 0, 1)

#Age Bins
titanic['Child'] = np.where((titanic['Age'] <=15), 1, 0)
titanic['Young_Adult'] = np.logical_and(titanic['Age'] >15, titanic['Age']<=29)
titanic['Young_Adult'] = np.where((titanic['Young_Adult'] == False), 0, 1)
titanic['Middle_Aged'] = np.logical_and(titanic['Age']>29, titanic['Age']<=54)
titanic['Middle_Aged'] = np.where((titanic['Middle_Aged'] == False), 0, 1)
titanic['Senior'] = np.where((titanic['Age']>54), 1, 0)

#Cheap Cabin: Fare less than avg and cabin indicator is present
titanic['Cheap_Cabin'] = np.logical_and(titanic['Fare'] < Fare_Avg, titanic['Cabin_ind'] == 1)
titanic['Cheap_Cabin'] = np.where((titanic['Cheap_Cabin'] == False), 0, 1)

#Cheap tickets based on Age
titanic['Age_Fare_Child'] = np.logical_and(titanic['Child'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_Child'] = np.where((titanic['Age_Fare_Child'] == False), 0, 1)
titanic['Age_Fare_YA'] = np.logical_and(titanic['Young_Adult'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_YA'] = np.where((titanic['Age_Fare_YA'] == False), 0, 1)
titanic['Age_Fare_MA'] = np.logical_and(titanic['Middle_Aged'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_MA'] = np.where((titanic['Age_Fare_MA'] == False), 0, 1)
titanic['Age_Fare_Senior'] = np.logical_and(titanic['Senior'] == 1, titanic['Fare'] < Fare_Avg)
titanic['Age_Fare_Senior'] = np.where((titanic['Age_Fare_Senior'] == False), 0, 1)


#Convert Sex to numeric, notice use of dictionary where in R we would use ifelse
gender_num = {'male': 0, 'female': 1}
titanic['Sex'] = titanic['Sex'].map(gender_num)
 
#Drop unnecessary variables
titanic.drop(['Cabin', 'Embarked', 'Name', 'Ticket'], axis=1, inplace=True)
 
#Write cleaned data to csv
titanic.to_csv('titanic_cleaned_vscode.csv', index=False)
