import pandas as pd
import numpy as np

#import folium
#import matplotlib.pyplot as plt
#from folium.plugins import HeatMap
# Load the data
data = pd.read_csv("Crime_DataSet.csv", dtype={'Perpetrator Age': 'str'})
columns_to_drop = ['Record ID', 'Agency Code', 'Agency Name', 'Record Source', 'Perpetrator Count', 'Victim Count']
data.drop(columns=columns_to_drop, inplace=True)
data.replace('Unknown', np.nan, inplace=True)
data.dropna(inplace=True)

print(data.head())
print(data.info()) 
data.to_csv('full_data_output.csv', index=False)


#data.drop_duplicates(inplace=True)




#data['Perpetrator Age'] = data['Perpetrator Age'].astype(str)

