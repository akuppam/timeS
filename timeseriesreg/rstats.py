import pyper
import pandas as pd

from itertools import product
from loguru import logger
from datetime import timedelta

from time_series_multireg.utils import mape, split_actuals


def sarima(actuals, days):
    """ Executes a seasonal arima forecast using R's auto.arima function.


        Parameters
        ----------
        actuals : pd.Series
            A pandas series with a DateTimeIndex.

        days : int
            The number of days to predict.

        Returns
        -------
        pd.Series
            A pandas series indexed by Timestamp containing the in and out
            sample predictions.
    """
    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )

    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("rDF", actuals_frame)
    r.assign("h", days)
    r("library(forecast)")
    r(
        "ts_data <- ts("
        "rDF$value, "  # noqa
        f"c({start_year}, {start_dayofyear}), "
        "frequency=365"
        ")"
    )
    r("fit <- auto.arima(ts_data, D=1)")
    r("fcst <- forecast(fit, h=h)")
    r("df_sarima <- c(fit$fitted, fcst$mean)")
    results = r.get("df_sarima")

    if results is None:
        raise ValueError("Unable to produce forecast with R SARIMA.")
    else:
        return pd.Series(
            results,
            index=pd.date_range(
                actuals.index[0], actuals.index[-1] + timedelta(days=days)
            ),
        )


def arima(actuals, days):
    """ Executes a non-seasonal ARIMA forecast using R's auto.arima function.


        Parameters
        ----------
        actuals : pd.Series
            A pandas series with a DateTimeIndex.

        days : int
            The number of days to predict.


        Returns
        -------
        pd.Series
            A pandas series indexed by Timestamp containing the in and out
            sample predictions.
    """
    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )

    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("rDF", actuals_frame)
    r.assign("h", days)
    r("library(forecast)")
    r(
        "ts_data <- ts("
        "rDF$value, "  # noqa
        f"c({start_year}, {start_dayofyear}), "
        "frequency=365"
        ")"
    )
    r("fit <- auto.arima(ts_data, D=1, seasonal=FALSE)")
    r("fcst <- forecast(fit, h=h)")
    r("df_arima <- c(fit$fitted, fcst$mean)")
    results = r.get("df_arima")

    if results is None:
        raise ValueError("Unable to produce forecast with R ARIMA.")
    else:
        return pd.Series(
            results,
            index=pd.date_range(
                actuals.index[0], actuals.index[-1] + timedelta(days=days)
            ),
        )


def sarima_stlm(actuals, days):
    """ Executes a seasonal arima forecast using R's stlm function.


        Parameters
        ----------
        actuals : pd.Series
            A pandas series with a DateTimeIndex.

        days : int
            The number of days to predict.

        Returns
        -------
        pd.Series
            A pandas series indexed by Timestamp containing the in and out
            sample predictions.
    """
    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )
    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("rDF", actuals_frame)
    r.assign("h", days)
    r("library(forecast)")
    r(
        "ts_data <- ts("
        "rDF$value, "  # noqa
        f"c({start_year}, {start_dayofyear}), "
        "frequency=365"
        ")"
    )
    r("model_sorder <- auto.arima(" "ts_data, " "D=1" ")")  # noqa
    r("asorder <- model_sorder$arma[c(1, 6, 2, 3, 7, 4, 5)]")
    r("names(asorder) <- c('p', 'd', 'q', 'P', 'D', 'Q', 'Frequency')")
    r(
        "model_fit_s <- stlm("
        "ts_data, "  # noqa
        "modelfunction=Arima, "
        "order=c(asorder[1], asorder[2], asorder[3]), "
        "seasonal=list(order=c(asorder[4], asorder[5], asorder[6]))"
        ")"
    )
    r("fit_sarima_fc <- forecast(model_fit_s, h=h)")
    r("df_sarima <- c(fit_sarima_fc$fitted, fit_sarima_fc$mean)")
    results = r.get("df_sarima")

    if results is None:
        raise ValueError("Unable to produce forecast with R SARIMA-STLM.")
    else:
        return pd.Series(
            results,
            index=pd.date_range(
                actuals.index[0], actuals.index[-1] + timedelta(days=days)
            ),
        )


def sarima_x(actuals, days, external_regressors=[]):
    """ Executes a seasonal arima forecast with external regressors.

    Parameters
    ----------
    actuals : pd.Series
        A pandas series with a DateTimeIndex.
    days : int
        The number of days to predict.
    external_regressors : list[pd.Series]
        A list of external regressors. Each external regressor is a pandas
        series indexed by datetime, and includes both actual and forecasted
        values for each regressor.

    Returns
    -------
    pd.Series
        A pandas series with a DateTimeIndex containing the in and out sample
        predictions.
    """
    # Validate the external regressors.
    actuals_start_date = actuals.index[0]
    actuals_end_date = actuals.index[-1]
    prediction_start_date = actuals.index[-1] + timedelta(days=1)
    prediction_end_date = actuals.index[-1] + timedelta(days=days)

    for external_regressor in external_regressors:
        assert external_regressor.index[0] <= actuals_start_date
        assert external_regressor.index[-1] >= prediction_end_date

    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )
    xreg_actuals_frame = pd.DataFrame(
        data={
            f"xreg{ii}": external_regressors[ii][
                actuals_start_date:actuals_end_date
            ].values
            for ii in range(len(external_regressors))
        }
    )
    xreg_forecast_frame = pd.DataFrame(
        data={
            f"xreg_{ii}": external_regressors[ii][
                prediction_start_date:prediction_end_date
            ].values
            for ii in range(len(external_regressors))
        }
    )
    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("actualsDF", actuals_frame)
    # PypeR translates numpy arrays to matrices.
    r.assign("xregActuals", xreg_actuals_frame.values)
    r.assign("xregForecast", xreg_forecast_frame.values)
    r.assign("h", days)
    r("library(forecast)")
    r(
        f"""
        actuals_ts <- ts(
            actualsDF$value, c({start_year}, {start_dayofyear}), frequency=365
        )
        model_sorder <- auto.arima(actuals_ts, D=1)
        asorder <- model_sorder$arma[c(1, 6, 2, 3, 7, 4, 5)]
        names(asorder) <- c("p", "d", "q", "P", "D", "Q", "Frequency")

        model <- arima(
            actuals_ts,
            order=asorder[1:3],
            seasonal=list(order=asorder[4:6], period=365),
            xreg=xregActuals
        )

        fcst <- forecast(model, h=h, xreg=xregForecast)
        df_sarima <- c(fcst$fitted, fcst$mean)
        """
    )
    results = r.get("df_sarima")

    if results is None:
        raise ValueError(
            f"Unable to produce forecast with R SARIMA "
            f"and {len(external_regressors)} external regressors."
        )
    else:
        return pd.Series(
            results,
            index=pd.date_range(actuals_start_date, prediction_end_date),
        )


def arima_stlm(actuals, days):
    """ Executes a non-seasonal arima forecast using R's stlm function.


        Parameters
        ----------
        actuals : pd.Series
            A pandas series with a DateTimeIndex.

        days : int
            The number of days to predict.

        Returns
        -------
        pd.Series
            A pandas series indexed by Timestamp containing the in and out
            sample predictions.
    """
    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )
    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("rDF", actuals_frame)
    r.assign("h", days)
    r("library(forecast)")
    r(
        "ts_data <- ts("
        "rDF$value, "  # noqa
        f"c({start_year}, {start_dayofyear}), "
        "frequency=365"
        ")"
    )
    r("model_order <- auto.arima(ts_data, seasonal = FALSE)")
    r(
        "fit <- stlm("
        "ts_data, "  # noqa
        "modelfunction=Arima, "
        "order=arimaorder(model_order)"
        ")"
    )
    r("fit_arima_fc <- forecast(fit, h=h)")
    r("df_arima <- c(fit_arima_fc$fitted, fit_arima_fc$mean)")
    results = r.get("df_arima")

    if results is None:
        raise ValueError("Unable to produce forecast with R ARIMA / STLM.")
    else:
        return pd.Series(
            results,
            index=pd.date_range(
                actuals.index[0], actuals.index[-1] + timedelta(days)
            ),
        )


def arima_x(actuals, days, external_regressors=[]):
    """ Executes a non-seasonal arima forecast with external regressors.


    Parameters
    ----------
    actuals : pd.Series
        A pandas series with a DateTimeIndex.
    days : int
        The number of days to predict.
    external_regressors : list[pd.Series]
        A list of external regressors. Each external regressor is a pandas
        series indexed by datetime, and includes both actual and forecasted
        values for each regressor.

    Returns
    -------
    pd.Series
        A pandas series indexed by Timestamp containing the in and out
        sample predictions.
    """
    # Validate the external regressors.
    actuals_start_date = actuals.index[0]
    actuals_end_date = actuals.index[-1]
    prediction_start_date = actuals.index[-1] + timedelta(days=1)
    prediction_end_date = actuals.index[-1] + timedelta(days=days)

    for external_regressor in external_regressors:
        assert external_regressor.index[0] <= actuals_start_date
        assert external_regressor.index[-1] >= prediction_end_date

    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )
    xreg_actuals_frame = pd.DataFrame(
        data={
            f"xreg_{ii}": external_regressors[ii][
                actuals_start_date:actuals_end_date
            ].values
            for ii in range(len(external_regressors))
        }
    )
    xreg_forecast_frame = pd.DataFrame(
        data={
            f"xreg_{ii}": external_regressors[ii][
                prediction_start_date:prediction_end_date
            ].values
            for ii in range(len(external_regressors))
        }
    )
    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("actualsDF", actuals_frame)
    # PypeR translates numpy arrays to matrices.
    r.assign("xregActuals", xreg_actuals_frame.values)
    r.assign("xregForecast", xreg_forecast_frame.values)
    r.assign("h", days)
    r("library(forecast)")
    r(
        f"""
        actuals_ts <- ts(
            actualsDF$value, c({start_year}, {start_dayofyear}), frequency=365
        )
        """
    )
    r(
        """
        fcstFrame <- stlf(
            actuals_ts,
            h=h,
            method="arima",
            xreg=xregActuals,
            newxreg=xregForecast
        )
        """
    )
    r("fcst <- c(fcstFrame$fitted, fcstFrame$mean)")
    results = r.get("fcst")

    if results is None:
        raise ValueError(
            "Unable to produce forecast with R ARIMA STLF "
            "with external regressors."
        )
    else:
        return pd.Series(
            results,
            index=pd.date_range(
                actuals.index[0], actuals.index[-1] + timedelta(days=days)
            ),
        )


def holt_winters(actuals, days, seasonal="mult"):
    """ Executes a Holt-Winters forecast using R.

        Parameters
        ----------
        actuals : pd.Series
            A pandas series with a DateTimeIndex.

        days : int
            The number of days to predict.

        seasonal : str
            The method of seasonal smoothing. 'mult' or 'add'.

        Returns
        -------
        pd.Series
            A pandas series indexed by Timestamp containing the in and out
            sample predictions.
    """
    r = pyper.R()
    actuals_frame = pd.DataFrame(
        data={
            "value": actuals.values,
            "date": actuals.index.strftime("%Y-%m-%d"),
        }
    )

    start_year = actuals.index[0].year
    start_dayofyear = actuals.index[0].dayofyear

    r.assign("rDF", actuals_frame)
    r.assign("h", days)
    r("library(stats)")
    r("library(forecast)")
    r(
        "ts_data <- ts("
        "rDF$value, "  # noqa
        f"start=c({start_year}, {start_dayofyear}), "
        "freq=365"
        ")"
    )
    r(f"fit <- HoltWinters(ts_data, seasonal = '{seasonal}')")

    # This requires some explanation - the fitted values for the HoltWinters
    # object do not contain values for the first period. When running forecast
    # it _does_, but fills them with NA. However, if days == 0, forecast( ... )
    # won't work.
    r(f"fits <- c(rep(NA, 365), fit$fitted[,'xhat'])")
    if days == 0:
        r("fit_hw_fc <- list(mean=c())")
    else:
        r("fit_hw_fc <- forecast(fit, h=h)")

    r("df_hw <- c(fits, fit_hw_fc$mean)")

    results = r.get("df_hw")

    if results is None:
        raise ValueError("Unable to produce forecast with R HoltWinters.")
    else:
        return pd.Series(
            results,
            index=pd.date_range(
                actuals.index[0], actuals.index[-1] + timedelta(days)
            ),
            dtype=float,
        )


def holt_winters_grid(actuals, grid, holdout=0.0, ignore_exceptions=False):
    best_params = None
    best_mape = None
    best_forecast = None
    param_labels = ["seasonal"]

    actuals_in_sample, actuals_out_sample = split_actuals(actuals, holdout)
    days = len(actuals_out_sample)

    # Assert that the correct keys are in the input dict.
    for pl in param_labels:
        if pl not in grid:
            raise ValueError(f"{pl} is not in the grid.")

    # Blow out the grid into an ordered container.
    parameter_grid = [grid[pl] for pl in param_labels]
    for params in product(*parameter_grid):
        seasonal = params[0]
        try:
            forecast = holt_winters(actuals_in_sample, days, seasonal=seasonal)
        except Exception as e:
            logger.exception(
                f"Encountered exception for holt-winters params {params}."
            )
            if ignore_exceptions:
                continue
            else:
                raise e

        forecast_in_sample, forecast_out_sample = split_actuals(
            forecast, holdout
        )

        if days == 0.0:
            # If holdout is 0, compute the in-sample error.
            forecast_mape = mape(
                actuals_in_sample[365:], forecast_in_sample[365:]
            )
        else:
            forecast_mape = mape(actuals_out_sample, forecast_out_sample)

        if (best_mape is None) or (forecast_mape < best_mape):
            best_mape = forecast_mape
            # Put the parameters in a dictionary for straightforward retrieval.
            best_params = {pl: p for pl, p in zip(param_labels, params)}
            best_forecast = forecast

    assert best_mape is not None
    return best_params, best_mape, best_forecast
