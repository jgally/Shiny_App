#Jasmine Gallaway
#ST 558 Final Project  
#12/12/23  
#
#Libraries to read in  
library(shiny)
library(shinydashboard)  
library(tidyverse)  
#
#
#reading in data file  
raw <- read.csv("abc-insurance-hr-data.csv", header = TRUE, sep = ",")  

#Saving a dataset specifically to use for numeric summaries  
num_data <- raw  

#changing variables into meaningful factors  
num_data$Ethnicity <- num_data$Ethnicity %>% 
  factor(levels = c(0,1,2,3,4),
         labels = c("White", "Black", "Asian", "Latino", "Other"))  

num_data$Gender <- num_data$Gender %>% 
  factor(levels = c(1,2),
         labels = c("Female", "Male"))  

num_data$JobRole <- num_data$JobRole %>% 
  factor(levels = c(0,1,2,3,4,5,6,7),
         labels = c("Administrative", "Customer Service", "Coordinator", "Jr Acct Mgr", "Sr Acct Mgr", "Asst Branch Mgr", "Branch Mgr", "Executive"))  




# Define UI using shiny dashboard  
shinyUI(fluidPage(
  #Using a bootswatch theme to change up the look  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
  ),
  #Official content of the UI page
  dashboardPage(
  dashboardHeader(title = "Company Retention"),
  #Here is my sidebar content
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
  #Here is all the body content; there's a lot to go through!
  dashboardBody(
    tabItems(
      tabItem("about",
              h3("Thi")),
      tabItem("data",
              fluidRow(
                #This first box is to describe the meaning of the numeric summaries; to give a key for what should be factor values  
                box(
                  title = "Key for Understanding",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  paste0("Remember: Variables such as evaluation, salary, job satisfaction, and intent to quit are ratings on a scale of 1 to 5 with 1 being a poorer score and 5 being a great score. See the About tab to determine specific meaning behind these scores.")
                ),
                #This box to hold all my numeric summary choices
                box(
                  title = "Numerical Summary Inputs",
                  status = "success",
                  solidHeader = TRUE, 
                  "Below you can choose how you would like to explore numerical statistics for the data. Some variables have been set to their numerical values rather than factoral values in order to have meaningful summaries.",
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
                  conditionalPanel(condition = "input.num_subset != 'No, use all data'",
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
                                   wellPanel(checkboxGroupInput("num_sub_roles", "Choose One or More", choices = c("Administrative", "Customer Service", "Coordinator", "Junior Account Manager" = "Jr Acct Mgr", "Senior Account Manager" = "Sr Acct Mgr", "Assistant Branch Manager" = "Asst Branch Mgr", "Branch Manager" = "Branch Mgr", "Executive"))
                                             )
                  ),
                  #This action button is the submit for once all choices by the user are made  
                  actionButton("num_button", "Generate Numeric Summaries", icon = icon("calculator"))
                ),
                
                #Here is the numerical summaries box to go along with all the choices  
                box(
                  title = "Numerical Summaries",
                  status = "success",
                  solidHeader = TRUE,
                  "New numeric summaries will generate here after the 'Generate Numeric Summaries' button has been clicked.",
                  textOutput("sum_stat")
                )
              ),
              br(),
              fluidRow(
                #This box is for user inputs on graphical summaries
                box(
                  title = "Graphical Summary Inputs",
                  status = "info",
                  solidHeader = TRUE,
                  "Below you can choose how you would like to explore graphical summaries for the data. Variables have been set to their factoral values for meaningful axis labels.",
                  radioButtons("graph", "Pick a Graph Type",
                               choices = c("Jitter", "Barplot", "Density", "Count"),
                               selected = "Jitter"),
                  selectizeInput("x", "Pick the X-Axis Variable",
                                 choices = c("Evaluation", "Salary", "Job Satisfaction", "Job Role", "Gender", "Ethnicity", "Age")),
                  selectizeInput("y", "Pick the Y-Axis Variable", 
                                 choices = c("Intent to Quit", "Job Satisfaction", "Salary", "Evaluation")),
                  radioButtons("more", "Would you like to graph by a third variable?",
                               choices = c("Yes", "No"),
                               selected = "No"),
                  conditionalPanel(condition = "input.third_var != 'No'",
                                   wellPanel(
                                     radioButtons("how", "How would you like to show the third variable?",
                                                  choices = c("Color", "Side-by-side Graphing", "Subsetting Data"))
                                   )),
                  conditionalPanel(condition = "input.third_var != 'No'",
                                   wellPanel(
                                     selectizeInput("third_var", "What should be the third variable?",
                                                    choices = c("Ethnicity", "Job Role", "Gender"))
                                     
                                   )),
                  conditionalPanel(condition = "input.how == 'Subsetting Data'",
                                   wellPanel(
                                     
                                   ))
                ),
                #This box is for the rendering of the plots after the user input
                box(
                  title = "Graphical Summary",
                  status = "info",
                  solidHeader = TRUE,
                  "The new selected graph will generate here once the 'Graph It' button is pushed."
                )
              )
              )
    )
  )
)
))
