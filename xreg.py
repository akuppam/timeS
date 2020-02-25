from datetime import timedelta


def replace_fits_with_actuals(time_series, actuals):
    """ Replaces fits with actuals.

    Parameters
    ----------
    time_series : pd.Series
        The time series (fits + forecast).
    actuals : pd.Series
        The actuals.

    Returns
    -------
    A new time series with the values replaced with the actuals (from the
    front).
    """

    assert len(time_series) >= len(actuals)

    time_series_copy = time_series.copy()
    end_date = time_series.index[0] + timedelta(days=(len(actuals) - 1))
    time_series_copy[:end_date] = actuals
    return time_series_copy
