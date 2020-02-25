import pandas as pd
from time_series_multireg import TimeSeriesMultiReg
from time import time
from itertools import product
import statsmodels.api as sm
import sys
from sklearn.metrics import mean_squared_error

test_data = pd.read_csv('tests/test_data/rnb1015_2_All.csv')
model = TimeSeriesMultiReg()

X = model.format_df(test_data)

print(X.head())
print(X.dtypes)
# sys.exit(1)

p_values = range(0, 3)
d_values = range(0, 2)
q_values = range(0, 3)
P_values = range(0, 3)
D_values = range(0, 2)
Q_values = range(0, 3)
m = 7 #weekly seasonality

parameter_grid = list(product(
                p_values,
                d_values,
                q_values,
                P_values,
                D_values,
                Q_values,
                [m]
        ))

def evaluate_sarima_model(X, fcstdays, parameters):
        order = parameters[:3]
        sorder = parameters[3:]
        # make predictions
        predictions = list()
        model = sm.tsa.statespace.SARIMAX(X,order=order,
                                          seasonal_order=sorder,
                                          enforce_stationarity=True,
                                          enforce_invertibility=True)
        model_fit = model.fit(transparams=True)
        predictions = model_fit.predict(start=0, end=len(X)-1, dynamic=False)
        #yhat = model_fit.forecast(fcstdays)    
        # calculate out of sample error
        error = mean_squared_error(X, predictions)
        print('error = ', error)
        return error

results = []
for group, frame in X.groupby(['region', 'marketing']):
    series = frame['y']
    for parameters in parameter_grid:
        
        try:
            start = time()
            mse = evaluate_sarima_model(series, 365, parameters)
            stop = time()
        except:
            pass
        
        results.append({
            "mse": mse,
            "region": group[0],
            "marketing": group[1],
            "time": stop - start,
            **{
                x: p for x, p in zip(
                    ["p","d","q","P", "D", "Q", "w"],
                    parameters
                )
            }
        })

all_results = pd.DataFrame.from_records(
    results
).to_csv(
    'sarima_profile.csv',
    index=False
)