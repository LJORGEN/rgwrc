import math
import sys,os
import geopandas
import numpy as np
import pandas as pd
from mgwr.sel_bw import Sel_BW
from mgwr.gwr import GWR,GWRResults

sales_py = pd.read_csv('rgwrc/rgwrc_hold/sales_r.csv',sep=',')
TFdf_py = pd.read_csv('rgwrc/rgwrc_hold/TFdf_r.csv',sep=',')
TF_py = TFdf_py.iloc[0]["TF"]
#define indpendent and dependent variables
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
#opt_bw_adap = Sel_BW(coords,Y,DV,kernel='gaussian').search(verbose=True)
opt_bw_adap = Sel_BW(coords,Y,DV,spherical=TF_py).search(verbose=True) #GC
#opt_bw_adap = Sel_BW(coords,Y,DV,kernel='gaussian').search(criterion='CV')
#Fitting the model with optimal bandwidth
#gwr_adapt_python = GWR(coords,Y,DV,opt_bw_adap,kernel='gaussian').fit()
gwr_adapt_python = GWR(coords,Y,DV,opt_bw_adap,spherical=TF_py).fit() #GC
gwr_adapt_python.summary()
#gwr_bw = gwr_selector.search(verbose=True)
