# Import required libraries
import numpy as np
import pandas as pd
import datetime
import math
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
# https://stackoverflow.com/questions/34366618/transform-pandas-timestamp-to-end-of-current-month
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
train_length = 300
test_length = 4
loops = math.floor((len(df) - train_length) / test_length)
start = len(df) - (loops * test_length + train_length)
stop = math.floor((len(df) - train_length) / test_length) * test_length

# Empty object
y_pred_prob = None

# Training loop
for i in range(start, stop, test_length):

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
    sgd = linear_model.SGDClassifier(
        loss = 'log'
        ,penalty = 'elasticnet'
        ,max_iter = 2500
        ,n_iter_no_change = 500
        ,tol = 1e-3)

    # Train model
    sgd.fit(x_train, y_train_raw)

    # Predict on test data
    y_pred = sgd.predict_proba(x_test)
    if y_pred_prob is None:
        y_pred_prob = y_pred
    else:
        y_pred_prob = np.concatenate((y_pred_prob, y_pred))
    #y_pred = sgd.predict(x_test)
    #conf_matrix = confusion_matrix(y_test_raw, y_pred)

# TO DO
# add date to output array to allow joining to the original Pandas dataframe
# add threshold to predicted probability and show various confusion matrices 

# Use the below to add index to results np.array
# https://stackoverflow.com/questions/51992291/how-to-join-2-dataframe-on-year-and-month-in-pandas
date_rng = pd.date_range(start = df.index[train_length + start], periods = stop, freq = 'M')


