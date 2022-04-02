from asyncore import write
from cProfile import label
import imp
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
titanic['Family_total'] = titanic['SibSp'] + titanic['Parch']
 
#Drop unnecessary vairables
titanic.drop(['PassengerId', 'SibSp', 'Parch'], axis = 1, inplace = True)
 
#Fill in missing and create indicator for Cabin variable
titanic.isnull().sum()
titanic.groupby(titanic['Cabin'].isnull())['Survived'].mean()
titanic['Cabin_ind'] = np.where(titanic['Cabin'].isnull(), 0, 1)

Fare_Avg = titanic['Fare'].mean()



#Convert Sex to numeric, notice use of dictionary where in R we would use ifelse
gender_num = {'male': 0, 'female': 1}
titanic['Sex'] = titanic['Sex'].map(gender_num)
 
#Drop unnecessary variables
titanic.drop(['Cabin', 'Embarked', 'Name', 'Ticket'], axis=1, inplace=True)
 
#Write cleaned data to csv
titanic.to_csv('titanic_cleaned_vscode.csv', index=False)
