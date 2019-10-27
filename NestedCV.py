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

# Inspect csv data - shape
df.shape

# Inspect csv data - index
df.index

# Inspect csv data - first and last records
df.iloc[np.r_[0:4, len(df) - 4:len(df)],]

# List variables, dependent variable followed by predictors/ independent variables 
df = df[['y1', 'BAA', 'GS10', 'GS2', 'FEDFUNDS', 'rtn_6m']]

# Drop na' when variables are not null / Nan
df = df.dropna(subset = ['y1', 'BAA', 'GS10', 'GS2', 'FEDFUNDS', 'rtn_6m'])
df.shape

# Inspect csv data - first and last records
df.iloc[np.r_[0:4, len(df) - 4:len(df)],]

# Loop to start here
# for i in df

# Set training and testing ranges
train_length = 400
test_length = 3

# Model data
y_train_raw = np.array(df.iloc[0:train_length, 0])
x_train_raw = np.array(df.iloc[0:train_length, 1:5])
y_test_raw = np.array(df.iloc[train_length:train_length + test_length, 0])
x_test_raw = np.array(df.iloc[train_length:train_length + test_length, 1:5])

# Scale for model ingestion
sc = StandardScaler()
x_train = sc.fit_transform(x_train_raw)

# Apply mean and standard deviation from transform applied to training data to test data
x_test = sc.transform(x_test_raw)

# Specifiy model
sgd = linear_model.SGDClassifier(loss = 'log', penalty = 'elasticnet', max_iter = 1000, tol = 1e-3)

# Train model
sgd.fit(x_train, y_train_raw)

# Predict on test data
y_pred_prob = sgd.predict_proba(x_test)
y_pred = sgd.predict(x_test)

conf_matrix = confusion_matrix(y_test_raw, y_pred)