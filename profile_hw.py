import pandas as pd
from time_series_multireg import TimeSeriesMultiReg
from time import time
from itertools import product
import statsmodels.api as sm
import sys
from sklearn.metrics import mean_squared_error
from statsmodels.tsa.holtwinters import ExponentialSmoothing

test_data = pd.read_csv('test_data_6m.csv')
model = TimeSeriesMultiReg()

X = model.format_df(test_data)

print(X.head())
print(X.dtypes)
# sys.exit(1)

# evaluate parameters
t_params = ['add', 'mul', None]
d_params = [True, False]
s_params = ['add', 'mul', None]
b_params = [True, False]
r_params = [True, False]
p = 365

parameter_grid = list(product(
                t_params,
                d_params,
                s_params,
                b_params,
                r_params,
                [p]
        ))

def evaluate_hw_model(X, fcstdays, parameters):
        #order = parameters[:3]
        #sorder = parameters[3:]
        t = parameters[0]
        d = parameters[1]
        s = parameters[2]
        b = parameters[3]
        r = parameters[4]
        p = parameters[5]
        # make predictions
        predictions = list()
        model = ExponentialSmoothing(X, trend=t, damped=d, seasonal=s, seasonal_periods=p)
        model_fit = model.fit(optimized=True, use_boxcox=b, remove_bias=r)
        predictions = model_fit.fittedvalues
        #yhat = model_fit.forecast(fcstdays)    
        # calculate out of sample error
        error = mean_squared_error(X, predictions)
        print('error = ', error)
        return error

results = []
for group, frame in X.groupby('RLT Marketing Channel'):
    series = frame['y']
    series = series.astype('double')
    for parameters in parameter_grid:
        
        try:
            start = time()
            mse = evaluate_hw_model(series, 365, parameters)
            stop = time()
        except:
            pass
        
        results.append({
            "mse": mse,
            "dimension": group,
            "time": stop - start,
            **{
                x: p for x, p in zip(
                    ["t","d","s","b","r","p"],
                    parameters
                )
            }
        })

all_results = pd.DataFrame.from_records(
    results
).to_csv(
    'hw_profile.csv',
    index=False
)
