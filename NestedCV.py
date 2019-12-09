# Import required libraries
import numpy as np
import pandas as pd
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
import datetime
import math
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
plt.rcParams["figure.figsize"] = (12, 8)
import sklearn.metrics as metrics
from sklearn import linear_model
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import StandardScaler

# Read csv fle from github
url = 'https://raw.githubusercontent.com/Brent-Morrison/Misc_scripts/master/econ_fin_data.csv'
df_raw = pd.read_csv(url)

# Convert date to datetime and create month end date
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

# List variables  
# Dependent variable
dep_var = ['y1']
# Predictors/ independent variables
ind_vars = ['CRED_SPRD', 'YLD_SPRD', 'LOAN_GROWTH', 'rtn_6m']
# Other variables to be used in the plot
oth_vars = ['close']
vars = dep_var + ind_vars + oth_vars
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
model_coef = None

# Training loop
for i in range(start, stop, test_length):

    # Model data
    y_train_raw = np.array(df.iloc[i:i + train_length, 0])
    x_train_raw = np.array(df.iloc[i:i + train_length, 1:len(vars) - 1])
    y_test_raw = np.array(df.iloc[i + train_length:i + train_length + test_length, 0])
    x_test_raw = np.array(df.iloc[i + train_length:i + train_length + test_length, 1:len(vars) - 1])

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

    # Predict on test data and write to array
    y_pred = sgd.predict_proba(x_test)
    if y_pred_prob is None:
        y_pred_prob = y_pred
    else:
        y_pred_prob = np.concatenate((y_pred_prob, y_pred))

    # Capture co-efficients and write to array
    coef = np.repeat(
        np.append(
            sgd.intercept_[0], 
            sgd.coef_).reshape((1, -1)),
        test_length, 
        axis = 0
        )
    
    if model_coef is None:
        model_coef = coef
    else:
        model_coef = np.concatenate((model_coef, coef))

# Create predictions dataframe with date index
df_preds = pd.DataFrame(
    data = y_pred_prob
    ,index = pd.date_range(
        start = df.index[train_length + start]
        ,periods = stop
        ,freq = 'M')
    ,columns = [0, 1]
    )
# Theshold for hard prediction, to populate confusion matrix
df_preds = df_preds.assign(pred = np.where(df_preds[1] > 0.25, 1, 0))

# Join predictions to df & rename prediction to pred_prob
df_preds = df_preds.join(df, how = 'inner')
df_preds = df_preds.rename(columns = {1:'pred_prob'}).drop(columns = 0)
df_preds.y1 = df_preds.y1.astype(int)
df_preds.close = np.log(df_preds['close'])

# Create co-efficients dataframe with date index
ind_vars.insert(0, 'Int')
ind_vars = [x + '_coef' for x in ind_vars]
df_model_coef = pd.DataFrame(
    data = model_coef
    ,index = pd.date_range(
        start = df.index[train_length + start]
        ,periods = stop
        ,freq = 'M')
    ,columns = ind_vars
    )

# Join predictions & co-efficients df's
df_preds_coefs = df_preds.join(df_model_coef, how = 'inner')

# Inspect dataframe of prediction probability
df_preds_coefs.iloc[np.r_[0:4, len(df_preds_coefs) - 4:len(df_preds_coefs)],]

# Set plot style
sns.set_style('white', {"xtick.major.size": 2, "ytick.major.size": 2})
flatui = ["#c5b4cc", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71","#f4cae4"]
sns.set_palette(sns.color_palette(flatui,7))

# Plot timeseries of SP500, prediction %, and y label shading
fig1, (ax1, ax2) = plt.subplots(nrows = 2)
fig1.suptitle('Prediction probability and S&P 500', size = 16).set_y(1.05)
fig1.subplots_adjust(top = 0.85)

ax1.plot(df_preds_coefs.index, df_preds_coefs['pred_prob'], 'k-', 
    color = sns.xkcd_rgb['grey'])
ax1.fill_between(
    df_preds_coefs.index, 
    df_preds_coefs['pred_prob'], 
    y2 = 0, 
    where = df_preds_coefs['y1']
    )
ax1.set_ylabel('Probability')

ax2.plot(df_preds_coefs.index, df_preds_coefs['close'], 'k-',
    color = sns.xkcd_rgb['grey'])
ax2.fill_between(
    df_preds_coefs.index, 
    df_preds_coefs['close'], 
    y2 = 0, 
    where = df_preds_coefs['y1']
    )
ax2.set_ylim(bottom = 4.5)
ax2.set_ylabel('S&P500 (log scale)')
fig1.tight_layout()


# Plot parameters
fig2, (ax1, ax2, ax3, ax4) = plt.subplots(nrows = 4)
fig2.suptitle('Rolling regression parameters', size = 16).set_y(1.05)
fig2.subplots_adjust(top = 0.85)

ax1.plot(df_preds_coefs.index, df_preds_coefs['Int_coef'], 'k-', 
    color = sns.xkcd_rgb['grey'])
ax1.fill_between(
    df_preds_coefs.index, 
    df_preds_coefs['Int_coef'], 
    y2 = df_preds_coefs['Int_coef'].min(), 
    where = df_preds_coefs['y1']
    )
ax1.set_ylabel('Intercept')

ax2.plot(df_preds_coefs.index, df_preds_coefs['CRED_SPRD_coef'], 'k-', 
    color = sns.xkcd_rgb['grey'])
ax2.fill_between(
    df_preds_coefs.index, 
    df_preds_coefs['CRED_SPRD_coef'], 
    y2 = df_preds_coefs['CRED_SPRD_coef'].min(),
    where = df_preds_coefs['y1']
    )
ax2.set_ylabel('Credit spread')

ax3.plot(df_preds_coefs.index, df_preds_coefs['YLD_SPRD_coef'], 'k-',
    color = sns.xkcd_rgb['grey'])
ax3.fill_between(
    df_preds_coefs.index, 
    df_preds_coefs['YLD_SPRD_coef'], 
    y2 = df_preds_coefs['YLD_SPRD_coef'].min(),
    where = df_preds_coefs['y1']
    )
ax3.set_ylabel('Yield spread')

ax4.plot(df_preds_coefs.index, df_preds_coefs['LOAN_GROWTH_coef'], 'k-',
    color = sns.xkcd_rgb['grey'])
ax4.fill_between(
    df_preds_coefs.index, 
    df_preds_coefs['LOAN_GROWTH_coef'], 
    y2 = df_preds_coefs['LOAN_GROWTH_coef'].min(),
    where = df_preds_coefs['y1']
    )
ax4.set_ylabel('Loan growth')
fig2.tight_layout()