
# coding: utf-8

# In[1]:

import datetime
import numpy as np #fast routines for manipulating matrics and linear algebra operations
import pandas as pd #uses numpy to manipulate heterogenous datasets
import matplotlib as mpl #library for plotting
import matplotlib.pyplot as plt
from yahoo_finance import Share
from sklearn import cluster, covariance, manifold


# In[2]:

#ipython magic function to use matplotlib interactively
get_ipython().magic(u'matplotlib inline')


# In[3]:

account = pd.read_csv('C:/Users/Ethan/Documents/mydata/account.csv')


# In[4]:

from twython import Twython

# Authentication details. To  obtain these visit dev.twitter.com
consumer_key = "4f7OkjmXFgsttc6Ash8C14Y4l"
consumer_secret = "q1glCsbxpBaYTof3vvkkgo0LGl0vbl3ISUrdXXTeQm5Tv59Ot5"
access_token = "2932809258-l7K860sS1WODVadYfk1D2vBOskExsUMIb9MtM5W"
access_token_secret = "O1dRedUqAHYcs7d1ZClKeaTwjgt9ggiXZkHJI4I7l0dVe"

twitter = Twython(consumer_key,consumer_secret,access_token,access_token_secret)
# details = twitter.show_user(screen_name='DJMany')
# print (details['location']) #Prints profile image URL


# In[5]:

names=[]
for name in account.name:
    try:
        details = twitter.show_user(screen_name=name)
        loc = str(details['location'])
        if loc == '':
            names.append('null')
        else:
            names.append(loc)
    except Exception:
        names.append('null')


# In[ ]:



