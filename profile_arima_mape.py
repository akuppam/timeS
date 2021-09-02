import pandas as pd
from time_series_multireg import TimeSeriesMultiReg
from time import time
from itertools import product
import statsmodels.api as sm
import sys
from sklearn.metrics import mean_squared_error
from statsmodels.tsa.arima_model import ARIMA
from sklearn.metrics import mean_absolute_error

test_data = pd.read_csv('rnb1015_2_All_seq.csv')
model = TimeSeriesMultiReg()

X = model.format_df(test_data)

print(X.head())
print(X.dtypes)
# sys.exit(1)

p_values = range(0, 6)
d_values = range(0, 2)
q_values = range(0, 6)

parameter_grid = list(product(
                p_values,
                d_values,
                q_values
        ))

def evaluate_arima_model(X, fcstdays, parameters):
        order = parameters[:3]
        # make predictions
        predictions = list()
        model = ARIMA(X,order=order)
        model_fit = model.fit(transparams=True)
        predictions = model_fit.predict(start=0, end=len(X)-1, dynamic=False)
        #yhat = model_fit.forecast(fcstdays)    
        # calculate MAPE
        #dff = abs((X-predictions)/X)
        #mape_arima = np.mean(dff)
        #print('mape_arima = ', mape_arima)
        #return mape_arima
        # calculate MA(P)E
        error = mean_absolute_error(X, predictions)
        print('error = ', error)
        return error

        
        

results = []
for group, frame in X.groupby('reg_mrkt'):
    series = frame['y']
    series = series.astype('double')
    for parameters in parameter_grid:
        
        try:
            start = time()
            mape = evaluate_arima_model(series, 365, parameters)
            stop = time()
        except:
            pass
        
        results.append({
            "mape": mape,
            "dimension": group,
            "time": stop - start,
            **{
                x: p for x, p in zip(
                    ["p","d","q"],
                    parameters
                )
            }
        })

all_results = pd.DataFrame.from_records(
    results
).to_csv(
    'arima_profile_mape.csv',
    index=False
)
