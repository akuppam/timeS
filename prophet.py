import pandas as pd

from fbprophet import Prophet
from toolz import get
from datetime import timedelta

from time_series_multireg.utils import (
    HOLIDAYS_AMR,
    HOLIDAYS_UK,
    HOLIDAYS_CE,
    HOLIDAYS_FR,
    HOLIDAYS_SOEU,
)


REGION_TO_HOLIDAYS = {
    "AMR": HOLIDAYS_AMR,
    "UK": HOLIDAYS_UK,
    "FR": HOLIDAYS_FR,
    "CE": HOLIDAYS_CE,
    "SoEu": HOLIDAYS_SOEU,
    "None": None,
}


def prophet(actuals, days, region="None", season_mode="multiplicative"):
    """ Performs a Prophet forecast on the provided actuals.

        Parameters
        ----------
        actuals : pd.Series
            A pandas series indexed with DateTimes.

        days : int
            The number of days to forecast.

        region : str
            The region to perform the forecast for. This determines which
            holidays are used. Must be "None", "NA", "FR", "UK, "CE", or
            "SOEU".

        season_mode : str
            Seasonality mode must be either "multiplicative" or "additive".

        Returns
        -------
        pd.Series
            A pandas series with the in and out sample forecasts, indexed by
            datetime.
    """

    # Assign the correct holidays to the prophet object.
    prophet_model = Prophet(
        daily_seasonality=False,
        weekly_seasonality=True,
        yearly_seasonality=True,
        seasonality_mode=season_mode,
        holidays=get(region, REGION_TO_HOLIDAYS, None),
    )

    # Get the actuals into the right format.
    actuals_df = pd.DataFrame({"y": actuals.values, "ds": actuals.index})

    # Fit the model to the actuals.
    prophet_model.fit(actuals_df)

    # Get the in- and out- sample predictions.
    forecast = prophet_model.predict(
        prophet_model.make_future_dataframe(periods=days)
    )

    # Return the full time series.
    return pd.Series(forecast.yhat.values, index=forecast.ds)


def prophet_X(
    actuals,
    days,
    region="None",
    external_regressors=[],
    season_mode="multiplicative",
):
    """ Performs a Prophet forecast on the provided actuals.
        Parameters
        ----------
        actuals : pd.Series
            A pandas series indexed with DateTimes.
        days : int
            The number of days to forecast.
        region : str
            The region to perform the forecast for. This determines which
            holidays are used. Must be "None", "AMR", "FR", "UK, "CE", or
            "SOEU".
        external_regressors : List[Series]
            The external regressors. Each Series should be indexed with
            DateTimes identically to the actuals except that they should extend
            into the future by the number of days required by the forecast.
        season_mode : str
            Seasonality mode must be either "multiplicative" or "additive".

        Returns
        -------
        pd.Series
            A pandas series with the in and out sample forecasts, indexed by
            datetime.
    """
    # Assign the correct holidays to the prophet object.
    prophet_model = Prophet(
        daily_seasonality=False,
        weekly_seasonality=True,
        yearly_seasonality=True,
        seasonality_mode=season_mode,
        holidays=get(region, REGION_TO_HOLIDAYS, None),
    )

    # Validate that each xreg has the correct number of observations. We need
    # at least the same number of observations as actuals + days, but we can
    # have more (they just get ignored).
    actuals_start_date = actuals.index[0]
    actuals_end_date = actuals.index[-1]
    prediction_end_date = actuals.index[-1] + timedelta(days=days)

    for external_regressor in external_regressors:
        assert external_regressor.index[0] <= actuals_start_date
        assert external_regressor.index[-1] >= prediction_end_date
    for xreg in external_regressors:
        assert len(xreg) >= (len(actuals) + days)

    # Get the actuals into the right format.
    actuals_df = pd.DataFrame(
        {
            "y": actuals.values,
            "ds": actuals.index,
            # Adds the actuals for the extra regressors.
            **{
                # Only add the present actuals.
                str(ii): arg[actuals_start_date:actuals_end_date].values
                for ii, arg in enumerate(external_regressors)
            },
        }
    )

    # Fit the model to the actuals and xregs.
    for ii in range(len(external_regressors)):
        prophet_model.add_regressor(str(ii))

    prophet_model.fit(actuals_df)

    future_df = prophet_model.make_future_dataframe(periods=days)

    for ii, xreg in enumerate(external_regressors):
        future_df.loc[:, str(ii)] = xreg[
            actuals_start_date:prediction_end_date
        ].values

    # Get the in- and out- sample predictions.
    forecast = prophet_model.predict(future_df)

    # Return the full time series.
    return pd.Series(forecast.yhat.values, index=forecast.ds)
