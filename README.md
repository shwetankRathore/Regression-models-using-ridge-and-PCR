This project aims to build linear models for predicting World Happiness Scores(Ladder Score) of countries. 
I considered six features to build these model-
  > LOGGED GDP PER CAPITA
  > SOCIAL SUPPORT
  > HEALTHY LIFE EXPECTANCY
  > FREEDOM TO MAKE LIFE CHOICES
  > GENEROSITY
Since there was multicollinearity in data, and all six features were importanrt so I used Ridge regression and Principal Component Regression(PCR) techniques to reduce multicolliniarity and build two different models.
I checked and resolved various other issues with data like outliers, heteroscedasticity etc.
And finally I compared efficiency of Ridge regression model and Principal Component regression(PCR) model.
