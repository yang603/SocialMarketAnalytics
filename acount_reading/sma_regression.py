
# coding: utf-8

# In[512]:

import datetime
import numpy as np #fast routines for manipulating matrics and linear algebra operations
import pandas as pd #uses numpy to manipulate heterogenous datasets
import matplotlib as mpl #library for plotting
import matplotlib.pyplot as plt
from yahoo_finance import Share
from sklearn import cluster, covariance, manifold


# In[513]:

#ipython magic function to use matplotlib interactively
get_ipython().magic(u'matplotlib inline')


# In[526]:

df = pd.read_csv('C:/Users/Ethan/Documents/mydata/ticker.csv')
ref = pd.read_csv('C:/Users/Ethan/Documents/mydata/ticker_classification.csv')
account = pd.read_csv('C:/Users/Ethan/Documents/mydata/account.csv')


# In[516]:

output = pd.merge(df, ref, on='Ticker', suffixes=['_left', '_right'], how='left', sort=False)


# In[518]:

tickers = output.Ticker


# In[520]:

names = tickers[2000:2010]
rates = []
for ticker in names:
    print ticker
    df = pd.DataFrame(Share(ticker).get_historical('2014-01-01', '2015-01-01'))
    df["Rate"] = (df["Close"].astype(float)-df["Open"].astype(float))/df["Open"].astype(float)
    data = np.asmatrix(df["Rate"])
    if rates == []:
        rates = data
    else:
        rates = np.append(rates, data, axis=0)


# In[522]:

variation = rates


# In[523]:

###############################################################################
# Learn a graphical structure from the correlations
edge_model = covariance.GraphLassoCV()

# standardize the time series: using correlations rather than covariance
# is more efficient for structure recovery
X = variation.copy().T
X /= X.std(axis=0)
edge_model.fit(X)


# In[524]:

###############################################################################
# Cluster using affinity propagation

_, labels = cluster.affinity_propagation(edge_model.covariance_)
n_labels = labels.max()

for i in range(n_labels + 1):
    print('Cluster %i: %s' % ((i + 1), ', '.join(names[labels == i])))


# In[ ]:



