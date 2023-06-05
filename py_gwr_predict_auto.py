import math
import sys,os
import geopandas
import numpy as np
import pandas as pd
from mgwr.sel_bw import Sel_BW
from mgwr.gwr import GWR,GWRResults

sales_py = pd.read_csv('C:/Users/Public/rgwrc_hold/sales_r.csv',sep=',')
pop_py = pd.read_csv('C:/Users/Public/rgwrc_hold/pop_r.csv',sep=',')
split_py = pd.read_csv('C:/Users/Public/rgwrc_hold/split_r.csv',sep=',')
split_py_e = split_py.iloc[0]["splitnum"]
TFdf_py = pd.read_csv('C:/Users/Public/rgwrc_hold/TFdf_r.csv',sep=',')
TF_py = TFdf_py.iloc[0]["TF"]
#unsampled data cannot be larger than the modeled data so break the population into segments to be fitted
#slipt pop data with numpy
pop_py_list = np.array_split(pop_py, split_py_e) #number of distinct dataframe to create

#define independent and dependent variables for the sales
#Y = sales.Amount.values.reshape(-1,1) #Non-Time Adjusted Sales Price
Y = sales_py.TASP.values.reshape(-1,1) #Time Adjusted sales Price
DV = sales_py.iloc[:,3:].values
k = sales_py.shape[1]
#define coords
u = sales_py.Longitude
v = sales_py.Latitude
n = sales_py.shape[0]
coords = np.array(list(zip(u,v)))

#Bandwidth searching
opt_bw_adap = Sel_BW(coords,Y,DV,spherical=TF_py).search(verbose=True) #great circle distances
#Run the GWR Model
gwr_adapt_python = GWR(coords,Y,DV,opt_bw_adap,spherical=TF_py).fit() #great circle distances
#Model Summary
gwr_adapt_python.summary()
########Fit the model Results onto the population data#########
#define GWR model
#gwr_model = GWR(coords,Y,DV,opt_bw_adap,spherical=TF_py) #great circle distances
gwr_results_list = []
for i in range(len(pop_py_list)):
    gwr_results_list.append(i)
    #create population variables
    pop_py_l = pop_py_list[i]
    XPl = pop_py_l.iloc[:,2:].values
    upl = pop_py_l.Longitude
    vpl = pop_py_l.Latitude
    coordsl = np.array(list(zip(upl,vpl)))
    #Run the GWR predictions
    gwr_model = GWR(coords,Y,DV,opt_bw_adap,spherical=TF_py) #great circle distances
    gwr_results_list[i] = gwr_model.predict(coordsl,XPl)
