import pandas as pd
from datetime import datetime
from langdetect import detect

from transformers import pipeline
from transformers import AutoTokenizer, AutoModelForSequenceClassification

class PreprocessingTweets:

    def __init__(self):
        """
        Description: 
        Preprocessing of the twitter data to convert the raw tweets into time series variables
        """


    def __load_data(self, path):
        """
        Description:
        - Load and filter the relevant raw twitter data
        Input:
        -  path: path where the twitter data is located 
        Output:
        - df containing the raw twitter data
        """
        df = pd.read_csv(path,dtype=object, sep = ';')
        df['timestamp'] =  pd.to_datetime(df['timestamp'],errors='coerce')

        mask = (df['timestamp'].dt.date > datetime(2018, 11, 23).date()) & (df['timestamp'].dt.date <= datetime(2019, 3, 29).date())
        df = df.loc[mask]

        return df
    
    def __cleaning(self, df):
        """
        Description:
        - Function to filter spam tweets
        Input:
        - df: df containing raw twitter data
        Output:
        """
        df = df[df['text'].str.contains('Giveaway')==False]
        df = df[df['text'].str.contains('Cashback')==False]
        df = df[df['text'].str.contains('Airdrop')==False]
        df = df[df['text'].str.contains('nft')==False]
        df = df[df['text'].str.contains('giveaway')==False]
        df = df[df['text'].str.contains('giveaway')==False]
        return df
    
    def __preprocess_finbert(self, text):
        """
        Description:
        - Preprocess an individual tweet for the application of finBert
        Input:
        - text: string, which contains a raw tweet
        Output:
        - string which is preprocessed
        """
        text = text.str.lower()
        text = text.str.replace("@[A-Za-z0-9_]+","user")
        text = text.str.replace("#"," ")
        text = text.str.replace(r"http\S+", "website")
        text = text.str.replace(r"www.\S+", "website")
        text = text.str.replace('[()!?]', ' ')
        text = text.str.replace('\[.*?\]',' ')
        text = text.str.replace("[^a-z0-9]"," ")
        return text
    
    def __language_detection(df):
        """
        Description:
        - Function to detect the language in each twitter and filter for english tweets
        Input:
        - df: df containing preprocessed tweets
        Output:
        - df filtered for english and processed tweets
        """
        language = []

        for key, value in enumerate(df['preprocessed_text']):
            try:
                lang = detect(value)
                language.append(lang)
            except:
                language.append('no_language')

        df['language'] = language

        df = df[df['language'] == 'en']


        return df
    
    def analyze_sentiment_finBERT(self, tweet, nlp):
        """
        Description:
        - Assigns finBert score to a tweet
        Input:
        - tweet: string of an english and processed tweet
        - nlp: nlp-Pipeline
        Output:
        - finBert score 1, 0 or -1
        """
        sentiment_dict = nlp(tweet)
        if sentiment_dict[0]['label']=='positive':
            return 1
        elif sentiment_dict[0]['label']=='neutral':
            return 0
        else: return -1
    
    def finbert_senti(self, df):
        """
        Description:
        - Apply the function analyze_sentiment_finBERT to the whole df
        Input:
        - df: df containing processed and filtered twitter data
        Output:
        - df containing processed and filtered twitter data with finBert score column
        """
        df["BERT_Sentiment"] = df['preprocessed_text_finbert'].apply(self.analyze_sentiment_finBERT)
        return df
    
    def __resample(self, df):
        """
        Description:
        - resample the df conaining tweets to a df of time series
        Input:
        - df: df with tweets
        Output:
        - df containing multiple time series variables in 6h frquency
        """
        frequency = '6H'

        df = df.resample(frequency, on='timestamp').agg({'quantity':'sum','BERT_Sentiment':'mean','likes':'mean','retweets':'mean','replies':'mean'})
        df.rename(columns={'quantity': 'num_tweets', 'BERT_Sentiment': 'BERT_Sentiment_mean', 'likes': 'likes_mean', 'retweets':'retweets_mean', 'replies':'replies_mean'}, inplace=True)
        df.index.rename('time', inplace=True)  

        return df
    
    def trigger_twitter_preprocessing(self, path):
        """
        Description:
        - Use the previous function to turn the raw twitter data into multiple time series
        Input:
        - path: path where the raw twitter data is located
        Output:
        - df containing multiple time series
        """
        df = self.__load_data(path)
        df = self.__cleaning(df)
        df =self.__preprocess_finbert(df)
        
        df["preprocessed_text_finbert"] = df['text'].apply(self.preprocess_finbert)

        df = self.__language_detection(df)

        tokenizer = AutoTokenizer.from_pretrained("ProsusAI/finbert")
        model = AutoModelForSequenceClassification.from_pretrained("ProsusAI/finbert",num_labels=3)
        nlp = pipeline("sentiment-analysis",model=model,tokenizer=tokenizer)

        df['BERT_Sentiment'] = df['preprocessed_text_finbert'].apply(lambda tweet: self.analyze_sentiment_finBERT(tweet, nlp))

        df = self.__resample(df)

        return df







            
        

