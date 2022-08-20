library(ggplot2)
library(shiny)
library(shinythemes)
library(plotly)
library(stargazer)
library(compare)
library(prediction)
library(survival)

#######################################################
#### Before publishing your dynamic nomogram:
####
#### - You may need to edit the following lines if
#### data or model objects are not defined correctly
#### - You could modify ui.R or server.R for
#### making any required changes to your app
#######################################################

load('data.RData')
source('functions.R')

m.summary <- 'raw'
covariate <- 'slider'
clevel <- 0.95

### Please cite the package if used in publication. Use:
# Amirhossein Jalali, Davood Roshan, Alberto Alvarez-Iglesias and John Newell (2019). DynNom: Visualising statistical models using dynamic nomograms.
# R package version 5.0. https://CRAN.R-project.org/package=DynNom

