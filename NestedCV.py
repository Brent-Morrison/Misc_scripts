# Import required libraries
import numpy as np
import pandas as pd
import datetime
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import StandardScaler
from sklearn import linear_model

# Read csv fle from github
url = 'https://raw.githubusercontent.com/Brent-Morrison/Misc_scripts/master/econ_fin_data.csv'
df_raw = pd.read_csv(url)

# Convert to date
df_raw['date'] = pd.to_datetime(df_raw['date'], format='%Y-%m-%d')

# Set date as index
df = df_raw.set_index('date')

# Creation of attributes
df = df.assign(
    CRED_SPRD = df.BAA - df.AAA
    ,YLD_SPRD = df.GS10 - df.FEDFUNDS
    ,LOAN_GROWTH = np.log(df.LOANS / df.LOANS.shift(6))
    )

# Inspect csv data - first and last records
df.iloc[np.r_[0:4, len(df) - 4:len(df)],]

# List variables, dependent variable followed by predictors/ independent variables 
df = df[['y1', 'CRED_SPRD', 'YLD_SPRD', 'LOAN_GROWTH', 'rtn_6m']]

# Drop na' when variables are not null / Nan
df = df.dropna(subset = ['y1', 'CRED_SPRD', 'YLD_SPRD', 'LOAN_GROWTH', 'rtn_6m'])

# Inspect csv data - first and last records
df.iloc[np.r_[0:4, len(df) - 4:len(df)],]

# Set training and testing ranges
# When train_length is set to 300 the sgd.fit function throws an error, 
# 'The number of classes has to be greater than one; got 1 class'.
# Inspecting the underlying data does not reveal any instances of singular 
# class over the sliding window.  train_length = 400 is ok.
train_length = 400
test_length = 3

# Empty array
y_pred_prob = np.array([(0, 0)])

# Training loop
for i in range(0, len(df) - train_length - test_length - 1, test_length):

    # Model data
    y_train_raw = np.array(df.iloc[i:i + train_length, 0])
    x_train_raw = np.array(df.iloc[i:i + train_length, 1:5])
    y_test_raw = np.array(df.iloc[i + train_length:i + train_length + test_length, 0])
    x_test_raw = np.array(df.iloc[i + train_length:i + train_length + test_length, 1:5])

    # Scale for model ingestion
    sc = StandardScaler()
    x_train = sc.fit_transform(x_train_raw)

    # Apply mean and standard deviation from transform applied to training data to test data
    x_test = sc.transform(x_test_raw)

    # Specifiy model
    sgd = linear_model.SGDClassifier(loss = 'log', penalty = 'elasticnet', shuffle = False ,max_iter = 1000, tol = 1e-3)

    # Train model
    sgd.fit(x_train, y_train_raw)

    # Predict on test data
    y_pred = sgd.predict_proba(x_test)
    y_pred_prob = np.concatenate((y_pred_prob, y_pred))
    #y_pred = sgd.predict(x_test)
    #conf_matrix = confusion_matrix(y_test_raw, y_pred)

# TO DO
# add date to output array to allow joining to the original Pandas dataframe
# add threshold to predicted probability and show various confusion matrices 

# Use the below to add index to results np.array
date_rng = pd.date_range(start = df.index[0], periods = 24, freq = 'M')

