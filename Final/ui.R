#Jasmine Gallaway
#ST 558 Final Project  
#12/12/23  
#
#Libraries to read in  
library(shiny)
library(shinydashboard)  
library(tidyverse)  
library(ggplot2)  
library(randomForest)  
library(mathjaxr)  
library(Metrics)
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
              box(
                title = "Information about the Dataset",
                width = 12,
                paste("The purpose of this app is to allow users to explore the employment data of the fictional 'abc' company. Overall, the user should be able to examine what factors affect the longevity of employees at the company. Employee retention cannot only save on training expenses and resources, but also foster better company culture and talent",
"The data used in this application comes from the Sage Research Methods Datasets. These datasets are provided for reasearch, teachers, and students alike to utilize while learning data analysis techniques. More information about this dataset and others can be found at the link below."),
                uiOutput("link"),
                paste("This data is the records of a fictional company. Employee attributes such as gender, ethnicity, tenure, performance evaluation, age, employee job satisfaction, job role, and salary are contained within this data set. The original data provides all numeric responses, but is intended to be facotral variables all except for age and tenure. For example, ethnicity is recorded in responses of 0, 1, 2, 3, 4. These numbers are not supposed to be taken numerically, but to be recognized with factoral levels as 'white', 'black', 'asian', 'latino', and 'other'. Job roles is recorded as 1-7, but the responses correlate to administrative, customer service, coordinator, junior account manager, senior account manager, assistant branch manager, branch manager, and executive; respectively. Gender is recorded as 1 or 2 with 1 meaning female and 2 meaning male. Salary is recording the salary grade of the employee which supplies that a range for which the employee's salary fits into. Salary grade is broken into a scale of 1-5 with 1 being 20 to 40 thousand dollars, 2 is 40 to 60 thousand, 3 is 60 to 80 thousand, 4 is 80 to 100 thousand, and 5 is 100 thousand and more. Job satisfaction, performance evaluation, and intention to quit are all rated on a scale of 1-5. 1 is treated as the lower end of the scale such as 'poor' for evaluation or 'extremely unlikely' for intent to quit. 5 is treated as the higher end of the scale meaning 'exceeded expectations' for evaluation or 'extremely likely' intend to quit. Tenure is the underlying focus of this data examination, not only for its importance within a company, but additionally due to its numeric structure which provides an easier examination of modeling success."
                       ,"Each tab within this application serves a different purpose for the user. This About tab is meant to explain the idea of the application and the data within it. The Data Exploration tab allows the user to change and choose variables used in numerical and graphical summaries. Data Exploration is important in allowing the user to become familiar with the data and possible relationships between different variables or subsets of the data. The Modeling tab is broken into three smaller tabs. The Modeling Info subtab goes into explaining the two models used within this app: linear regression and random forests. Model Fitting subtab is where the user can select variables and tuning parameters for the models to evaluate the data. Within this tab fit statistics for the models such as root mean square deviation will be compared to determine how well the models are performing.The Prediction subtab should allow the user to use linear regression and random forest modeling to test out specified variable values. Users will be able to select the values and gain predictions for just how long an employee's tenure might be.",
                       sep = "\n"),
imageOutput("biz_image")
                      
              )
              ),
      tabItem("data",
              fluidRow(
                #This first box is to describe the meaning of the numeric summaries; to give a key for what should be factor values  
                box(
                  title = "Key for Understanding",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  paste("Remember: Variables such as evaluation, salary, job satisfaction, and intent to quit are ratings on a scale of 1 to 5 with 1 being a poorer score and 5 being a great score. See the About tab to determine specific meaning behind these scores.")
                ),
                #This box to hold all my numeric summary choices
                box(
                  title = "Numerical Summary Inputs",
                  status = "success",
                  solidHeader = TRUE, 
                  "Below you can choose how you would like to explore numerical statistics for the data. Some variables have been set to their numerical values rather than factoral values in order to have meaningful summaries.",
                  #Choices will be set to their original numerical values only here in order to produce meaningful numerical summaries  
                  selectizeInput("num_var", "Choose a Variable to Summarize",
                                 choices = c("Age", "Evaluation", "Salary", "Job Satisfaction", "Tenure", "Intent to Quit"),
                                 selected = "Age"),
                  #Put only the four numerical summary types, min and max do not provide much value except for the Age and Tenure variables  
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
              #
              #
              fluidRow(
                #This box is for user inputs on graphical summaries
                box(
                  title = "Graphical Summary Inputs",
                  status = "info",
                  solidHeader = TRUE,
                  "Below you can choose how you would like to explore graphical summaries for the data. Variables have been set to their factoral values for meaningful axis labels.",
                  #Allowing the user to pick the graph type  
                  radioButtons("graph", "Pick a Graph Type",
                               choices = c("Jitter", "Column Plot", "Regression", "Count"),
                               selected = "Jitter"),
                  #User can pick the variable for the x axis here
                  selectizeInput("x", "Pick the X-Axis Variable",
                                 choices = c("Evaluation", "Salary", "Job Satisfaction", "Job Role", "Gender", "Ethnicity", "Tenure", "Age")),
                  #User can pick the variable for the y axis here
                  selectizeInput("y", "Pick the Y-Axis Variable", 
                                 choices = c("Intent to Quit", "Job Satisfaction", "Salary", "Evaluation")),
                  #Awkward wording, but here is where the user can choose if they want to see a multivariate relationship
                  radioButtons("more", "Would you like to graph by a third variable?",
                               choices = c("Yes", "No"),
                               selected = "No"),
                  #Options for how to visualize the multivariate relationship
                  conditionalPanel(condition = "input.more != 'No'",
                                   wellPanel(
                                     radioButtons("how_third", "How would you like to show the third variable?",
                                                  choices = c("Color", "Side-by-side Graphing", "Subsetting Data"))
                                   )),
                  #If the user does not want to subset they have to pick only one variable here
                  conditionalPanel(condition = "input.more != 'No' && input.how_third != 'Subsetting Data'",
                                   wellPanel(
                                     selectizeInput("third_var", "What should be the third variable?",
                                                    choices = c("Ethnicity", "Job Role", "Gender"))
                                     
                                   )),
                  #Here comes in all the subsetting options that was in the numerical summaries section
                  conditionalPanel(condition = "input.how_third == 'Subsetting Data'",
                                   wellPanel(
                                     checkboxGroupInput("sub_options", "Pick Which Variables to Subset the Data by (Pick one or more)",
                                                        choices = c("Ethnicity", "Gender", "Job Role"
                                   ))
                                   )),  
                  #The next conditional panels are based on the selections of the checkbox in the above conditional panel --Starting with with the options for ethnicity  
                  
                  conditionalPanel(condition = "input.sub_options.indexOf('Ethnicity') != -1",
                                   wellPanel(checkboxGroupInput("sub_ethnicity", "Choose One or More", choices = c("White", "Black", "Asian", "Latino", "Other")
                                   )
                                   )
                  ),
                  #Here are the options if gender is selected  
                  conditionalPanel(condition = "input.sub_options.indexOf('Gender') != -1",
                                   wellPanel(radioButtons("sub_gender", "Choose One", c("Female", "Male"))
                                   )
                  ),
                  #Here are the options if job role is selected  
                  conditionalPanel(condition = "input.sub_options.indexOf('Job Role') != -1",
                                   wellPanel(checkboxGroupInput("sub_roles", "Choose One or More", choices = c("Administrative", "Customer Service", "Coordinator", "Junior Account Manager" = "Jr Acct Mgr", "Senior Account Manager" = "Sr Acct Mgr", "Assistant Branch Manager" = "Asst Branch Mgr", "Branch Manager" = "Branch Mgr", "Executive"))
                                   )
                  ),
                  #Here is the action button to submit all of the users inputs for graphing. 
                  actionButton("graph_button", "Graph It!", icon = icon("chart-column"))
                ),
                #This box is for the rendering of the plots after the user input
                box(
                  title = "Graphical Summary",
                  status = "info",
                  solidHeader = TRUE,
                  "The new selected graph will generate here once the 'Graph It!' button is pushed.",
                  plotOutput("graph_plot")
                )
              )
              ),
      #This is the next tab to tackle  
      tabItem("info",
              box(
                title = "In Depth Modeling Knowledge",
                status = "info",
                solidHeader = TRUE,
                width = 12, 
                paste0("Linear regression models are a way to conduct predictive analysis. Typically, it is easiest to envision linear regression as the lines typically overlayed on graphs to show general trends. Linear regression models follow a couple of general assumptions. The response variable being modeled must be a continuous response, not a discrete variable such as factoral variables. The statistical errors should assume a normal distribution. Constant variance should also be assumed. If these assumptions are not met then data scientists can used generalized linear regression models to fit their data."),
                br(),
                paste0("Random forests are a type of ensemble method of modeling. Random forest take what is known as bootstrapping samples from the data in order to evaluate the relationships between predictor variables and the target variable. A bootstrap sample is a sample of a specific size take from the data and then the data can be replaced into the data. Bootstrapping tends to lead to an out of bag amount of data that has not been sampled once during the process. This can be used as a testing set in some situations. Random forests build multiple trees (could be regression or classification) which should decrease the variance of predictions compared to a single modeling tree. A lower variance is preferred in situations involving prediction as it could mean the predictions will be more accurate. Random forests that are used for predicting a classification variable will take the class majority predicted by all of its sampling. If it is predicting the values for regression the random forest will take the average of the results produced. Random forests are different from other ensemble methods such as bagging because random forests will consider only a subset of the predictor variables when building trees. Bagging methods use all of the predictor variables to evaluate the relationships between the predictors and the focus variable. Subsetting the variables with random forests can increase the bias, but hopefully the decreased variance will balance these pros and cons in order to provide a better model fitting.")
              )
      ),
      tabItem("fitting",
              box(
                title = "Model Fitting Inputs",
                status = "info",
                solidHeader = TRUE,
                "Below you can choose how to model the data.",
                #User can change the split percentage; I gave a reasonable range for splits  
                sliderInput("split", "Select the Training/Test Split Percentage",
                            min = 0.5, max = 0.95, value = 0.7, step = 0.05),
                #Allowing the user to choose predictors for the lm  
                checkboxGroupInput("lm_predictors", "Choose predictor(s) to model the lm for Tenure",
                                   choices = c("Age", "Gender", "Intent to Quit", "Ethnicity", "Job Role", "Job Satisfaction", "Salary", "Evaluation"),
                                   selected = "Age"),
                #Allowing the user to choose predictors for the random forest  
                checkboxGroupInput("rf_predictors", "Choose predictor(s) to model the rf for Tenure",
                                   choices = c("Age", "Gender", "Intent to Quit", "Ethnicity", "Job Role", "Job Satisfaction", "Salary", "Evaluation"),
                                   selected = "Age"),
                #Tuning parameter grid options for the user to choose from  
                numericInput("tune_grid", "Tune the Random Forest Model (Pick a number that is equal to, or less than the total number of predictors selected)",
                             value = 1, min = 1, max = 8, step = 1),
                #User options to changing the CV settings 
                numericInput("alter_cv", "Change CV Setting",
                             value = 1, min = 1, max= 5, step = 1),
                #Action button to model both at the same time  
                actionButton("model", "Model It!", icon = icon("code-branch"))
              ),
              box(
                title = "Model Fit",
                status = "info",
                solidHeader = TRUE,
                "These are the fit statistics of each model."#,
                #Here is the RMSE  
                #uiOutput("comparisons"),
                #Here is the summary stats of the lm and the tree plot of the rf  
                #uiOutput("fits")
              )
      ),
      tabItem("predictions",
              box(
                title = "Predictions of Company Retention",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                "This is where users should have been able to select values for different predictor variables"
              )
              )
    )
  )
)
))
