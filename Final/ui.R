#Jasmine Gallaway
#ST 558 Final Project  
#12/12/23  
#
#Libraries to read in  
library(shiny)
library(shinydashboard)  
library(tidyverse)  
#
#Read in dataset file  
#read.csv("abc-insurance-hr-data", sep = ",")

# Define UI using shiny dashboard  
shinyUI(fluidPage(
  dashboardPage(
  dashboardHeader(title = "Company Retention"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", 
               tabName = "about", 
               icon = icon("circle-info")),
      menuItem("Data Exploration",
               tabName = "data",
               icon = icon("route")),
      menuItem("Modeling",
               tabName = "models",
               icon = icon("chart-simple"),
               #Beginning the subtabs  
               startExpanded = TRUE,
               menuSubItem("Model Info",
                           tabName = "info"),
               menuSubItem("Model Fitting",
                           tabName = "fitting"),
               menuSubItem("Prediction",
                           tabName = "predictions"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("about",
              h3("Thi")),
      tabItem("data",
              fluidRow(
                box(
                  title = "Numerical Summaries Inputs",
                  "Below you can choose how you would like to explore the data. Some variables have been set to their numerical values rather than factoral values in order to have meaningful summaries.",
                  #Choices will be set to their original numerical values only here in order to produce meaningful numerical summaries  
                  selectizeInput("num_var", "Choose a Variable to Summarize",
                                 choices = c("Age", "Evaluation", "Salary", "Job Satisfaction", "Intent to Quit"),
                                 selected = "Age"),
                  #Put only the four numerical summary types, min and max do not provide much value except for the Age variable  
                  selectizeInput("num_type", "Choose a Type of Numerical Summary to Observe",
                                 choices = c("Mean", "Median", "Minimum", "Maximum"),
                                 selected = "Mean"),
                  radioButtons("num_subset", "Subset the Data?",
                               c("Yes", "No, use all data"),
                               selected = "No, use all data"),
                  #This is the conditional panel to allow the user to select which rows of the data they would like to use in the numerical summaries (how they would like to subest the data)  
                  conditionalPanel(condition = "input.num_subset == 'Yes'",
                                   checkboxGroupInput("num_sub_options", "Pick Which Variables to Subset the Data by (Pick one or more)",
                                                      choices = c("Ethnicity", "Gender", "Job Role"))),  
                  #The next conditional panels are based on the selections of the checkbox in the above conditional panel --Starting with with the options for ethnicity  
                  
                  conditionalPanel(condition = "input.num_sub_options.indexOf('Ethnicity') != -1",
                                   wellPanel(checkboxGroupInput("num_sub_ethnicity", "Choose One or More", choices = c("White", "Black", "Asian", "Latino", "Other")
                                   )
                  )
                  ),
                  #Here are the options if gender is selected  
                  conditionalPanel(condition = "input.num_sub_options.indexOf('Gender') != -1",
                                   wellPanel(radioButtons("num_sub_gender", "Choose One", c("Female", "Male"))
                                             )
                                   ),
                  #Here are the options if job role is selected  
                  conditionalPanel(condition = "input.num_sub_options.indexOf('Job Role') != -1",
                                   wellPanel(checkboxGroupInput("num_sub_roles", "Choose One or More", choices = c("Administrative", "Customer Service", "Coordinator", "Junior Account Manager", "Senior Account Manager", "Assistant Branch Manager", "Branch Manager", "Executive"))
                                             )
                  )
                )
              )
              )
    )
  )
)
))
