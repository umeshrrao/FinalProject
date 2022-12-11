# FinalProject
This app provides 4 tabs

* About

* Data Exploration

* Modeling 

* Data


## Data Exploration Tab:- 

Create numerical and graphical summaries. Change the type of plot shown and type of summary reported.Change the variables and filter the rows to change the data used in the plots/summaries.

## Modeling Tab:-

The user has an option to fit three supervised learning models. Depending on their response they will fit a multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model. 

This page has three tabs to it:

### Modeling Info tab:

Explains these three modeling approaches, the benefits of each,and the drawbacks of each. 

### Model Fitting tab:

Gives the ability to split the data into a training and test set by being able to choose the proportion of data used in each.The user can choose the model settings for each model. For all models, they can select the variables used as predictors.The user can press a button and fit all three models on the training data.· Fit statistics (such as RMSE) on the training data is reported for each model on the mainpanel along with summary of the fit. Please note the default model selects all predictor variables, the user can subset the variables to generate statistics. In order to do custom prediction it is mandatory to select color as this is to demonstrate the prediction of specific value of color

### Prediction tab:

The user can choose any one of the models for prediction. They can select the values of the predictors(color) and obtain a prediction for the response. Here we are using only the MLR model for demonstration purpose


## Data Tab:- 

The user can Scroll through the data set.Subset this data set (rows and columns).Save the data as a csv file

# Packages used

library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(rio)
library(DT)
library(stargazer)
library(randomForest)
    
# Use the following code to run the app

shiny::runGitHub('umeshrrao/FinalProject')
