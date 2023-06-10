import pandas as pd
from datetime import datetime

class PreprocessingBTC:

    def __init__(self):
        """
        Description:
        - Class to resample btc price data into the desired frequency
        """

    def __load_data(self, path):
        """
        Description:
        - load the raw btc price data and filter the desired time frame
        Input:
        - path: path, where the raw btc data is located
        Output:
        - df of filtered btc price data
        """
        df = pd.read_csv(path,dtype=object, sep = ';')
        df['timestamp'] =  pd.to_datetime(df['timestamp'],errors='coerce')

        mask = (df['timestamp'].dt.date > datetime(2018, 11, 23).date()) & (df['timestamp'].dt.date <= datetime(2019, 3, 29).date())
        df = df.loc[mask]

        return df
    
    def __resample(self, df):
        """
        Description:
        - function to bring the btc price data in 6h frequency
        Input:
        - df with resampled btc price data
        Output:
        """
        frequency = '6H'

        df = df.resample(frequency, on='time').agg({'close':'last','volume':'sum'})
        df.rename(columns={'close': 'btc_price', 'volume': 'volume_sum'}, inplace=True)

        return df
    
    def trigger_btc_preprocessing(self, path):
        """
        Description:
        - trigger the previous functions
        Input:
        
        Output:
        """
        df = self.__load_data(path)
        df = self.__resample(df)

        return df