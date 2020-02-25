import math

from time_series_multireg.utils import mape


def split_actuals(actuals, holdout_pct):
    """ Splits a time series of actuals into an in sample and out sample set.

    Holdouts are pulled from the end of the actuals.

    Parameters
    ----------
    actuals : `pd.Series`
        A pandas Series with a datetime index.
    holdout : float
        The percentage to hold out.

    Returns
    -------
    tuple[pd.Series, pd.Series]
        The in-sample actuals and the out-sample actuals.
    """
    
    if holdout_pct > 1:
        slice_point = len(actuals) - holdout_pct
    else:
        out_sample_length = math.floor(len(actuals) * holdout_pct)
        slice_point = len(actuals) - out_sample_length
        
    in_sample = actuals[:slice_point]
    out_sample = actuals[slice_point:]

    return in_sample, out_sample


def out_sample_mape(actuals, out_sample, forecast):
    """ Calculates the mape of the out sample.

    If the forecast has more points than out sample actuals, calculates the
    MAPE against the available out sample actuals.

    Parameters
    ----------
    actuals : `pd.Series`
        A pandas Series indexed by date time representing the in-sample
        actuals.
    out_sample : `pd.Series`
        A pandas Series indexed by date representing the out-sample actuals.
    forecast : `pd.Series`
        A pandas Series indexed by date representing the forecast (in and out
        sample).

    Returns
    -------
    The MAPE of the out sample forecast against the out sample actuals.
    """

    days = len(forecast) - len(actuals)
    evaluation_days = min(days, len(out_sample))

    return mape(
        out_sample[:evaluation_days].values,
        # Evaluate the forecast for all the days we have out_sample values, or
        # the days themselves, whichever is smaller.
        forecast[len(actuals) : (len(actuals) + evaluation_days)].values,
    )
