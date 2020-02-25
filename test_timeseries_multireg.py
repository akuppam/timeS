import pytest
import os
import pandas as pd

from time_series_multireg import TimeSeriesMultiReg


@pytest.fixture()
def test_data():
    """ Fixture for the test data.
    """
    current_dir = os.path.dirname(os.path.abspath(__file__))
    test_data_dir = os.path.join(current_dir, "test_data")

    return pd.read_csv(os.path.join(test_data_dir, "test_data_6m.csv"))


@pytest.fixture()
def model():
    """ Fixture for the model.
    """
    return TimeSeriesMultiReg()


def test_run_regressions(test_data, model):
    """ Tests that the _Run_Regressions function will execute on the test
        data.
    """
    answer = model._Run_Regressions(
        test_data,
        100,
        forecasts={"arima", "holtwinters", "prophet", "arima_r", "sarima_r"}
    )