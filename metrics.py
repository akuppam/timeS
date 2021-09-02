import numpy as np


def mape(actuals, predictions):
    """ Calculates the mean absolute percent error.

        Parameters
        ----------
        actuals : array-like
            A numpy array or pandas Series containing the actuals.

        predictions : array-like
            A numpy array or pandas Series containing the actuals.

        Returns
        -------
        `np.float64`
            The mean absolute percent error between the actuals and the
            predictions.
    """
    return np.mean(np.abs(predictions - actuals) / actuals)
