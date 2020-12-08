# COVID-19 Analytics
R code for COVID-19 predictive analytics.
These are a series of functions designed to process aggregated data made available by the NHS about COVID-19 triage.
The ETL process comprises cleaning the data and pulling information about the CCG from the ONS so that more information can be inferred about the various communities dealing with the pandemic with various degrees of acuteness. 
Data from the api of Goole Places is used to enrich the data. It is used by the notification system (beta) designed to inform local businesses of each community about surges or decreases of COVID-19 cases in the local area. 
A series of models are built using xgboost (2-fold cv and hyper-params tuned iteratively in the code; no test data set was used since the models are not intended to predict but for analytics only). The target variable in all of them is triagecount, but each model points at different aspects of the pandemic. The code wraps xgb.importance and ggplot of variables so that just running the code will produce these automatically and store them for you to have a look.

Before you run the code:
1) Download the most up-to-date data from nhs.digital.uk (pathways and digital).
2) Make sure filepaths in the code point the right files
3) You will need a Google Places api key to run the notification system. Make sure the "apikey" argument references your own key.

To run the pipeline simply run the calls.R script.
