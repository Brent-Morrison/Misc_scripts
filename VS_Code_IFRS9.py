import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
simfin = pd.read_csv("C:/Users/brent/Documents/R/R_import/output-comma-narrow.csv")
x = simfin.head()

plot_1 = simfin[(simfin['Indicator Name'] == 'Market Capitalisation')].assign(me_date = pd.to_datetime(simfin['publish date']).dt.to_period('M'))
plot_1.head()

plot_2 = plot_1[['me_date', 'Indicator Value']].groupby('me_date').aggregate(np.sum)
plot_2.head()

plot_2.plot()
