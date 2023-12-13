# Shiny_App
The purpose of this app is to allow users to explore the employment data of the fictional 'abc' company. Overall, the user should be able to examine what factors affect the longevity of employees at the company. Employee retention cannot only save on training expenses and resources, but also foster better company culture and talent. Within the app, users are able to control various options for data exploration, model fitting, and prediction. 

Packages to run the app:

library(shiny)  

library(shinydashboard)  

library(tidyverse)  

library(ggplot2)  

library(randomForest)  

library(mathjaxr)  

library(Metrics)

Use this line of code to install all these packages on local server:

`install.packages(c("shiny","shinydashboard","tidyverse","ggplot2","randomForest","mathjaxr","Metrics"))`
 
Copy and paste this into RStudio to run my app: 

`shiny::runGitHub('Shiny_App', username = "jgally", subdir = "Final")`
