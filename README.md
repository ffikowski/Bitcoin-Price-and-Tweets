# Bitcoin-Price-and-Tweets
- This project has the goal to predict the bicoin price by using tweets containing hash tags like #BTC or #Bitcoin
- First we generate to each tweet a sentiment score through the usage of the pretrained transformer model finBERT
- Then the mean sentiment score per day is calculated
- This mean sentiment score is used among other variables in a VAR time series model to predict the bitcoin price
- Although a VAR model is not the most suitable model class the project shows how language models and classic regression analysis tools can interact
