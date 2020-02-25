import click
import logging
import pandas as pd
import warnings

from time import time

from time_series_multireg import TimeSeriesMultiReg

log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
logging.basicConfig(level=logging.INFO, format=log_fmt)
logger = logging.getLogger(__name__)


@click.command()
@click.option(
    "--input-data",
    type=click.File("r"),
    default="data/rnb1015_2_All.csv"
)
@click.option(
    "--method",
    type=click.Choice(["all", "holtwinters", "sarima", "arima", "prophet"]),
    default="all"
)
@click.option("--days", type=int, default=100)
def main(input_data, method, days):
    if method == "all":
        forecasts = {"arima", "prophet", "holtwinters"}
    else:
        forecasts = {method}

    model = TimeSeriesMultiReg()

    logger.info(f"Reading data from {input_data.name}.")
    data = pd.read_csv(input_data)
    logger.info("Done reading data.")

    logger.info(f"Running {', '.join(list(forecasts))} on test data.")
    start = time()
    with warnings.catch_warnings():
        model._Run_Regressions(
            data,
            days,
            forecasts=forecasts
        )
    stop = time()
    logger.info(f"Forecasts produced in {stop - start:.3f}s.")

    # TODO: figure out what the output should be?

if __name__ == "__main__":
    main()