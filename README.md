# covid-19 analytics
R code for COVID-19 predictive analytics.
This is a series of functions designed to process aggregated data made available by the NHS about COVID-19 triage.
The ETL process comprises cleaning the data and pulling information about the ccg from the ONS so that more information can be inferred abuot the communities dealing with the pandemic more or less accutely. Data from the api of Goole Places is used to enrich the data. The code contains the rudimentary notification system designed to inform local businesses of each community about surges or decreases of COVID-19 cases in the local area. 

Before you run the code:
1) Download the data from nhs.digital.uk
2) Make sure filepaths in the code reference the right files
3) You will need a Google Places api key to run the notification system. Make sure the "apikey" argument references your own key.

To run the code simply run the calls.R script.
