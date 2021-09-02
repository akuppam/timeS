import pandas as pd
import math

from loguru import logger
from joblib import Parallel, delayed
from toolz import merge, curry

from time_series_multireg.forecasts import (
    holt_winters_grid,
    holt_winters,
    arima,
    sarima,
    arima_stlm,
    sarima_stlm,
    prophet,
    prophet_X,
    arima_x,
    sarima_x,
)
from time_series_multireg.utils import (
    mape,
    split_actuals,
    out_sample_mape,
    replace_fits_with_actuals,
)

# NOTE: It's possible to turn this into a dict mapping curried forecast
# function and make them kwargs, but that might be more painful to call than
# it's worth. Experiment with that later.
ALL_FORECASTS = {
    "holtwinters",
    "arima",
    "sarima",
    "prophet",
    "prophet_additive",
    "prophet_multiplicative",
    "arima_stlm",
    "sarima_stlm",
    "arima_X",
    "sarima_X",
    "prophet_X",
    "prophet_X_additive",
    "prophet_X_multiplicative",
}

XREG_FORECASTS = {
    "prophet_X",
    "arima_X",
    "sarima_X",
    "prophet_X_additive",
    "prophet_X_multiplicative",
}

# TODO: Is there an easy way to make the grids themselves configurable?


def _run_forecast(
    actuals,
    days,
    forecast_method,
    out_sample=None,
    grid_holdout=0.0,
    ignore_grid_exceptions=True,
    region="None",
    external_regressors=[],
):
    if forecast_method == "holtwinters":
        hw_param_grid = {"seasonal": ["add", "mult"]}
        hw_params, hw_mape, _ = holt_winters_grid(
            actuals,
            hw_param_grid,
            holdout=grid_holdout,
            ignore_exceptions=ignore_grid_exceptions,
        )

        forecast = holt_winters(actuals, days, seasonal=hw_params["seasonal"])

        # Now evaluate on the out-sample if possible. Replace the
        # MAPE with out sample value.
        if (out_sample is not None) and (len(out_sample) > 0):
            hw_mape = out_sample_mape(actuals, out_sample, forecast)

        return hw_mape, forecast

    elif forecast_method.startswith("arima"):
        if forecast_method.endswith("stlm"):
            forecast = arima_stlm(actuals, days)
        elif forecast_method.endswith("X"):
            forecast = arima_x(
                actuals, days, external_regressors=external_regressors
            )
        else:
            forecast = arima(actuals, days)

        if (out_sample is not None) and (len(out_sample) > 0):
            arima_mape = out_sample_mape(actuals, out_sample, forecast)
        else:
            arima_mape = mape(actuals, forecast)

        return arima_mape, forecast

    elif forecast_method.startswith("sarima"):
        if forecast_method.endswith("stlm"):
            forecast = sarima_stlm(actuals, days)
        elif forecast_method.endswith("X"):
            forecast = sarima_x(
                actuals, days, external_regressors=external_regressors
            )
        else:
            forecast = sarima(actuals, days)

        if (out_sample is not None) and (len(out_sample) > 0):
            sarima_mape = out_sample_mape(actuals, out_sample, forecast)
        else:
            sarima_mape = mape(actuals, forecast)
        return sarima_mape, forecast

    elif forecast_method.startswith("prophet"):
        # Default seasonality mode is multiplicative.
        seasonality_mode = "multiplicative"

        # Adjust seasonality mode.
        if "additive" in forecast_method:
            seasonality_mode = "additive"

        # Select prophet or prophet_X and forecast.
        if "X" in forecast_method:
            forecast = prophet_X(
                actuals,
                days,
                region=region,
                external_regressors=external_regressors,
                season_mode=seasonality_mode,
            )
        else:
            forecast = prophet(
                actuals, days, region=region, season_mode=seasonality_mode
            )

        prophet_mape = mape(actuals, forecast[: len(actuals)])

        # Now evaluate the out-sample if possible. Replace the MAPE with
        # out sample value.
        if (out_sample is not None) and (len(out_sample) > 0):
            prophet_mape = out_sample_mape(actuals, out_sample, forecast)

        return prophet_mape, forecast


def _select_forecast(
    actual_frame,
    group,
    days,
    holdout=0.0,
    forecasts={
        "holtwinters",
        "arima",
        "arima_stlm",
        "sarima",
        "sarima_stlm",
        "prophet",  # Default prophet seasonality is multiplicative.
        "prophet_additive",
    },
    date_col="ds",
    measure_col="y",
    region_col="region",
    external_regressors=[],  # A list of time series for external regressors.
    ignore_exceptions=True,
):
    # Validate that the external regressors have enough points.
    if external_regressors:
        for xreg in external_regressors:
            assert len(xreg) == (days + actual_frame.shape[0])

    # If there's only one element in the group by, pandas won't wrap it in
    # a tuple while iterating.
    if not isinstance(group, tuple):
        group = (group,)
    logger.debug(f"Determining best forecast for {', '.join(group)}.")

    # Generate the pandas time series.
    actuals = pd.Series(
        actual_frame.set_index(date_col)[measure_col].astype("float64"),
        index=pd.to_datetime(actual_frame[date_col]),
    )
    region = (
        "None"
        if not region_col
        else actual_frame.head(n=1)[region_col].values[0]
    )  # Thanks pandas.

    # Split the holdout. Out sample is empty if the holdout is zero.
    actuals_in_sample, actuals_out_sample = split_actuals(actuals, holdout)

    logger.debug(f"In sample actuals: {len(actuals_in_sample)}.")
    logger.debug(f"Out sample actuals: {len(actuals_out_sample)}.")

    # Loop over each forecast and calculate in- or out- sample errors.
    best_mape = math.inf
    best_forecast = None
    all_forecasts = {}
    best_forecasts = {}
    for forecast_method in forecasts:

        logger.debug(f"Performing {forecast_method} on {', '.join(group)}.")

        # We don't need the forecast itself during method selection, but
        # it should be added to all_forecasts for diagnostics.
        try:
            forecast_mape, forecast = _run_forecast(
                actuals_in_sample,
                days,
                forecast_method,
                out_sample=actuals_out_sample,
                ignore_grid_exceptions=ignore_exceptions,
                region=region,
                external_regressors=external_regressors,
            )
        except Exception as e:
            logger.exception(
                f"Encountered exception for {forecast_method}, "
                f"dimensions: {', '.join(group)}."
            )
            # Re-raise if ignore_exceptions is turned off.
            if ignore_exceptions:
                continue
            else:
                raise e

        logger.debug(f"{forecast_method} produced mape {forecast_mape:.3f}.")
        logger.debug(f"Best mape: {best_mape:.3f}.")

        all_forecasts[group + (forecast_method,)] = (forecast_mape, forecast)
        if forecast_mape < best_mape:
            best_mape = forecast_mape
            best_forecast = forecast_method

            logger.debug(f"New best mape: {best_mape:.3f}.")
            logger.debug(f"New best forecast: {best_forecast}.")

    if best_forecast is None:
        raise ValueError("Unable to produce a valid forecast.")

    logger.debug(f"Re-running {best_forecast} with a complete set of actuals.")
    # Select re-run the best forecast.
    forecast_mape, forecast = _run_forecast(
        actuals,
        days,
        best_forecast,
        region=region,
        external_regressors=external_regressors,
    )

    best_forecasts[group] = (best_forecast, best_mape, forecast)
    return best_forecasts, all_forecasts


def multireg_forecast(
    all_actuals,
    days,
    dimension_cols=None,
    xreg_cols=[],
    date_col="ds",
    measure_col="y",
    region_col="region",
    forecasts={
        "holtwinters",
        "arima",
        "arima_stlm",
        "sarima",
        "sarima_stlm",
        "prophet",
    },
    holdout=0.0,
    ignore_exceptions=True,
    n_jobs=4,
):
    # Validate the data frame.
    frame_cols = set(all_actuals.columns)
    if dimension_cols is not None:
        missing_cols = set(dimension_cols) - frame_cols
        if missing_cols:
            raise ValueError(
                f"Frame is missing the following expected dimensions: "
                f"{', '.join(missing_cols)}."
            )
    if date_col not in frame_cols:
        raise ValueError(f"Frame is missing expected date col: {date_col}.")
    if measure_col not in frame_cols:
        raise ValueError(
            f"Frame is missing expected measure col: {measure_col}."
        )
    if region_col and (region_col not in frame_cols):
        raise ValueError(
            f"Frame is missing expected region col: {region_col}."
        )

    if xreg_cols and (not (forecasts - XREG_FORECASTS)):
        raise ValueError(
            "Needs one method that doesn't require external regressors "
            "so xreg_cols can be forecasted."
        )

    if xreg_cols is not None:
        missing_cols = set(xreg_cols) - frame_cols
        if missing_cols:
            raise ValueError(
                "Frame is missing the following expected external regressors :"
                f"{', '.join(missing_cols)}."
            )

    # Validate the forecasts.
    invalid_forecasts = set(forecasts) - ALL_FORECASTS
    if invalid_forecasts:
        raise ValueError(
            f"Unable to recognize forecasts: {', '.join(invalid_forecasts)}."
        )

    # Determine if there are extra columns in the dataset that need to be
    # summed over. Start with all of the columns, less the measure and the
    # date.
    extra_cols = set(all_actuals.columns) - {date_col, measure_col, *xreg_cols}
    if dimension_cols:
        extra_cols -= set(dimension_cols)
    else:
        extra_cols = set()

    if region_col:
        extra_cols -= {region_col}

    # If there's anything left in that set, sum the measure over them.
    if extra_cols:
        group_cols = set(all_actuals.columns) - extra_cols - {measure_col}
        all_actuals = (
            all_actuals.groupby(list(group_cols))
            .agg({measure_col: "sum", **{c: "sum" for c in xreg_cols}})
            .reset_index()
        )

    # Now determine which dimensions will be regressed over.
    if dimension_cols:
        grouping_cols = set(dimension_cols)
    else:
        grouping_cols = set(all_actuals.columns) - {
            date_col,
            measure_col,
            *xreg_cols,
        }

    # This is in case dimension_cols is not empty, but doesn't have the region
    # col in it. If dimension_cols _is_ empty, then region_col is already in
    # the set, so this is a no-op in that case.
    if region_col:
        grouping_cols.add(region_col)

    # Select the best forecast for each external regressor, for all dimensions.
    # Each element of this is a dict mapping the dimension group to the best
    # forecast.
    xreg_forecasts = []
    logger.info(f"Forecasting {len(xreg_cols)} external regressors.")
    for xreg_col in xreg_cols:
        best_xreg_forecasts = {}
        select_xreg_forecast = curry(_select_forecast)(
            days=days,
            holdout=holdout,
            date_col=date_col,
            measure_col=xreg_col,  # <- forecast the xreg.
            region_col=region_col,
            # Skip the xreg forecasts when we're forecasting the external
            # regressors.
            forecasts=forecasts - XREG_FORECASTS,
            ignore_exceptions=ignore_exceptions,
        )
        forecast_jobs = []
        for group, actual_frame in all_actuals.groupby(list(grouping_cols)):
            # This insanity brought to you by pandas and its inconsistent
            # return types.
            logger.info(f"Forecasting {', '.join(group)} for {xreg_col}.")
            if len(grouping_cols) == 1:
                group = (group,)
            logger.info(f"Forecasting {','.join(group)}.")
            forecast_jobs.append(
                delayed(select_xreg_forecast)(
                    actual_frame.reset_index(), group
                )
            )
        forecast_results = Parallel(n_jobs=n_jobs)(forecast_jobs)
        best_xreg_forecasts = merge([fr[0] for fr in forecast_results])
        xreg_forecasts.append(best_xreg_forecasts)

    best_forecasts = {}
    all_forecasts = {}
    select_forecast = curry(_select_forecast)(
        days=days,
        holdout=holdout,
        date_col=date_col,
        measure_col=measure_col,
        region_col=region_col,
        forecasts=forecasts,
        ignore_exceptions=ignore_exceptions,
    )
    forecast_jobs = []
    logger.info(f"Running primary forecast.")
    for group, actual_frame in all_actuals.groupby(list(grouping_cols)):
        if len(grouping_cols) == 1:
            group = (group,)
        logger.info(f"Forecasting {','.join(group)}.")
        forecast_jobs.append(
            delayed(select_forecast)(
                actual_frame.reset_index(),
                group,
                # xregf is a dict mapping dimension group to
                # (method, mape, forecast). We just need the forecast.
                external_regressors=[
                    # Need to replace the fitted values with actuals for
                    # each external regressor.
                    replace_fits_with_actuals(
                        xregf[group][2], actual_frame[xreg_col]
                    )
                    for xregf, xreg_col in zip(xreg_forecasts, xreg_cols)
                ],
            )
        )
    forecast_results = Parallel(n_jobs=n_jobs)(forecast_jobs)

    best_forecasts = merge([fr[0] for fr in forecast_results])
    all_forecasts = merge([fr[1] for fr in forecast_results])

    return best_forecasts, all_forecasts


def multireg_forecast_to_df(multireg_forecast):
    """ Takes the multireg results and puts them in a hierarchically indexed
        data frame with one column per dimension group.
    """
    mreg_frame = pd.DataFrame(columns=multireg_forecast)
    for key in multireg_forecast:
        mreg_frame[key] = list(multireg_forecast[key][:-1]) + list(
            multireg_forecast[key][-1]
        )

    return mreg_frame
