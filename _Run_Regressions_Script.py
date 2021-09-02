import pandas as pd
import time
import click
import logging

from time import time

from time_series_multireg import TimeSeriesMultiReg

log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
logging.basicConfig(level=logging.INFO, format=log_fmt)
logger = logging.getLogger(__name__)


def run_regressions(input_file, output_file_location, forecastDays, region="None",
         forecasts={"holtwinters", "arima", "sarima", "prophet"}):

    model = TimeSeriesMultiReg()
    # returns a touple where the [0] entry is the best MAPES and the [1] is all MAPES
    start = time()
    logger.info("Running forecasts.")
    predicted_data = model._Run_Regressions(
            pd.read_csv(input_file),
            forecastDays,
            region,
            forecasts
        )
    logger.info(f"Done in {time() - start:.3f}.")
    
    bestOut = pd.DataFrame(columns=predicted_data[0])
    for key in predicted_data[0]:
        #bestOut[key[0]] = list(bestt[0][key][0:2])
        bestOut[key[0]] = list(predicted_data[0][key][0:2]) + list(predicted_data[0][key][2])
    
    allOut = pd.DataFrame(columns=predicted_data[1])
    for key in predicted_data[1]:
        #bestOut[key[0]] = list(bestt[0][key][0:2])
        allOut[key[0]] = list(predicted_data[1][key][0][0:2]) + list(predicted_data[1][key][0][2])
    
    best_output_file = output_file_location + 'bestMapes_' + time.strftime('%Y_%m_%d')
    all_output_file = output_file_location + 'allMapes_' + time.strftime('%Y_%m_%d')
    logger.info(f"Writing best models to {best_output_file}.")
    bestOut.to_csv(best_output_file)
    logger.info("Done.")
    logger.info(f"Writing all models to {all_output_file}.")
    allOut.to_csv(all_output_file)
    logger.info("Done.")
    
    #predicted_data.reset_index().to_parquet(output_file)


@click.command()
@click.argument('input_file', type=click.File('r'))
@click.argument('output_file_location', type=str)
@click.argument('forecast_days', type=int)
@click.option(
    '--region',
    type=click.Choice(['None', 'NA', 'UK', 'FR', 'CE', 'SoEu']),
    default='None'
)
@click.option(
    '--forecast', '-f',
    type=click.Choice(['holtwinters', 'arima', 'sarima', 'prophet']),
    multiple=True,
    default={"holtwinters", "arima", "prophet"}
)
def main(input_file, output_file_location, forecast_days, region, forecast):
    run_regressions(
        input_file,
        output_file_location,
        forecast_days,
        region=region,
        forecasts=set(forecast)
    )


if __name__ == "__main__":
    main()