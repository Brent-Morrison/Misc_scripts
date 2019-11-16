# Import required libraries
import numpy as np
import pandas as pd
import datetime
import math
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from sklearn.metrics import confusion_matrix
import sklearn.metrics as metrics
from sklearn.preprocessing import StandardScaler
from sklearn import linear_model

# Read csv fle from github
url = 'https://raw.githubusercontent.com/Brent-Morrison/Misc_scripts/master/econ_fin_data.csv'
df_raw = pd.read_csv(url)

# Convert to daet and create month end date
df_raw['date'] = pd.to_datetime(df_raw['date'], format='%Y-%m-%d')
df_raw['me_date'] = pd.Index(df_raw['date']).to_period('M').to_timestamp('M')

# Set date as index in new df
df = df_raw.set_index('me_date')

# Creation of attributes
df = df.assign(
    CRED_SPRD = df.BAA - df.AAA
    ,YLD_SPRD = df.GS10 - df.FEDFUNDS
    ,LOAN_GROWTH = np.log(df.LOANS / df.LOANS.shift(6))
    )

# Inspect csv data - first and last records
df.iloc[np.r_[0:4, len(df) - 4:len(df)],]

# List variables, dependent variable followed by predictors/ independent variables 
# and variable to be used in the plot
vars = ['y1', 'CRED_SPRD', 'YLD_SPRD', 'LOAN_GROWTH', 'rtn_6m', 'close']
# List independent variable, this will be used to label the paramater time series array
ind_vars = ['CRED_SPRD', 'YLD_SPRD', 'LOAN_GROWTH', 'rtn_6m']
df = df[vars]

# Drop na' when variables are not null / Nan
df = df.dropna(subset = vars)

# Inspect csv data - first and last records
df.iloc[np.r_[0:4, len(df) - 4:len(df)],]

# Set training and testing ranges
train_length = 300
test_length = 4
loops = math.floor((len(df) - train_length) / test_length)
start = len(df) - (loops * test_length + train_length)
stop = math.floor((len(df) - train_length) / test_length) * test_length

# Empty objects
y_pred_prob = None
model_param = None

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

    # Specify model
    sgd = linear_model.SGDClassifier(
        loss = 'log'
        ,penalty = 'elasticnet'
        ,max_iter = 2500
        ,n_iter_no_change = 500
        ,tol = 1e-3)

    # Train model
    sgd.fit(x_train, y_train_raw)

    # Predict on test data and write to table
    y_pred = sgd.predict_proba(x_test)
    if y_pred_prob is None:
        y_pred_prob = y_pred
    else:
        y_pred_prob = np.concatenate((y_pred_prob, y_pred))
     
    param = np.repeat(
        np.append(
            sgd.intercept_[0], 
            sgd.coef_).reshape((1, -1)),
        test_length, 
        axis = 0
        )
    
    if model_param is None:
        model_param = param
    else:
        model_param = np.concatenate((model_param, param))

# Create predictions dataframe with date index
df_preds = pd.DataFrame(
    data = y_pred_prob
    ,index = pd.date_range(
        start = df.index[train_length + start]
        ,periods = stop
        ,freq = 'M')
    ,columns = [0, 1]
    )
# Theshold for hard prediction, to beused in confusion matrix
df_preds = df_preds.assign(pred = np.where(df_preds[1] > 0.25, 1, 0))

# Join predictions to df & rename prediction to pred_prob
df_preds = df_preds.join(df, how = 'inner')
df_preds = df_preds.rename(columns = {1:'pred_prob'}).drop(columns = 0)
df_preds.y1 = df_preds.y1.astype(int)
df_preds.close = np.log(df_preds['close'])

# Create parameters dataframe with date index
ind_vars.insert(0, 'Int')
df_model_param = pd.DataFrame(
    data = model_param
    ,index = pd.date_range(
        start = df.index[train_length + start]
        ,periods = stop
        ,freq = 'M')
    ,columns = ind_vars
    )

# Confusion matrix
cf_pred = np.array(df_preds['pred'])
cf_true = np.array(df_preds['y1'])
conf_matrix = confusion_matrix(cf_true, cf_pred)

# Plot ROC curve
roc_probs = np.array(df_preds['pred_prob'])
fpr, tpr, threshold = metrics.roc_curve(cf_true, roc_probs)
roc_auc = metrics.auc(fpr, tpr)
plt.title('Receiver Operating Characteristic')
plt.plot(fpr, tpr, 'b', label = 'AUC = %0.2f' % roc_auc)
plt.legend(loc = 'lower right')
plt.plot([0, 1], [0, 1],'r--')
plt.xlim([0, 1])
plt.ylim([0, 1])
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
plt.show()

# Plot timeseries of SP500, prediction %, and y label shading
import seaborn as sns
sns.set_style('white', {"xtick.major.size": 2, "ytick.major.size": 2})
flatui = ["#c5b4cc", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71","#f4cae4"]
sns.set_palette(sns.color_palette(flatui,7))

fig1, (ax1, ax2, ax3) = plt.subplots(nrows = 3)
ax1.plot(df_preds.index, df_preds['pred_prob'], 'k-', 
    color = sns.xkcd_rgb['grey'])
ax1.fill_between(
    df_preds.index, 
    df_preds['pred_prob'], 
    y2 = 0, 
    where = df_preds['y1']
    )
ax1.set_ylabel('Probability')

ax2.plot(df_preds.index, df_preds['CRED_SPRD'], 'k-', 
    color = sns.xkcd_rgb['grey'])
ax2.fill_between(
    df_preds.index, 
    df_preds['CRED_SPRD'], 
    y2 = 0, 
    where = df_preds['y1']
    )
ax2.set_ylabel('Credit spread')

ax3.plot(df_preds.index, df_preds['close'], 'k-',
    color = sns.xkcd_rgb['grey'])
ax3.fill_between(
    df_preds.index, 
    df_preds['close'], 
    y2 = 0, 
    where = df_preds['y1']
    )
ax3.set_ylim(bottom = 4.5)
ax3.set_ylabel('S&P500 (log scale)')
fig1.tight_layout()

# Plot parameters
# TO DO: fill will only work if 'y1' is in the dataframe
fig2, (ax1, ax2, ax3) = plt.subplots(nrows = 3)
ax1.plot(df_model_param.index, df_model_param['Int'], 'k-', 
    color = sns.xkcd_rgb['grey'])
#ax1.fill_between(
#    df_model_param.index, 
#    df_model_param['Int'], 
#    y2 = 0, 
#    where = df_model_param['y1']
#    )
ax1.set_ylabel('Int')

ax2.plot(df_model_param.index, df_model_param['CRED_SPRD'], 'k-', 
    color = sns.xkcd_rgb['grey'])
#ax2.fill_between(
#    df_model_param.index, 
#    df_model_param['CRED_SPRD'], 
#    y2 = 0, 
#    where = df_model_param['y1']
#    )
ax2.set_ylabel('Credit spread')

ax3.plot(df_model_param.index, df_model_param['YLD_SPRD'], 'k-',
    color = sns.xkcd_rgb['grey'])
#ax3.fill_between(
#    df_model_param.index, 
#    df_model_param['close'], 
#    y2 = 0, 
#    where = df_model_param['y1']
#    )
ax3.set_ylabel('Yield spread')
fig2.tight_layout()