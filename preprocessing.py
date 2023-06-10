from preprocessing_tweets import PreprocessingTweets
from preprocessing_btc import PreprocessingBTC

import pandas as pd

class Preprocessing:

    def __init__(self):
        """
        Description:
        This class units the preprocessed twitter and btc data.
        """

        self.preprocessing_tweets = PreprocessingTweets()
        self.preprocessing_btc = PreprocessingBTC()

    def trigger_preprocessing(self, path_tweets, path_btc):
        """
        Description:
        - The funciton triggers the preprocessing of the twitter and btc data and concatinates the resulting dataframe.
        Input:
        - path_tweets: path where the twitter data is located
        - path_btc: path where the btc data is located
        Output:
        - processed multivariate time series
        """
        df_tweets = self.preprocessing_tweets.trigger_twitter_preprocessing(path=path_tweets)
        df_btc = self.preprocessing_btc.trigger_btc_preprocessing(path=path_btc)

        df = pd.concat([df_tweets, df_btc], axis=1)

        return df



