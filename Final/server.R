#Jasmine Gallaway
#ST 558 Final Project  
#12/12/23  
#
#
#Reading in libraries  
library(shiny)  
library(shinydashboard)  
library(tidyverse)  
library(ggplot2)  
library(Metrics)

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

#saving the num_data as a new object to turn everything that needs to be into a factor  
fact_data <- num_data  

#Here are all the factor variables that were kept numeric for the numerical summaries tab  
fact_data$Salary.grade <- fact_data$Salary.grade %>% 
  factor(levels = c(1,2,3,4,5),
         labels = c("20-40", "40-60", "60-80", "80-100", "100+"))  

fact_data$Evaluation <- fact_data$Evaluation %>% 
  factor(levels = c(1,2,3,4,5),
         labels = c("Poor", "Slightly Underperformed", "Met Expectations", "Slightly Exceeded", "Excellent"))  

fact_data$Intention.to.Quit <- fact_data$Intention.to.Quit %>% 
  factor(levels = c(1,2,3,4,5),
         labels = c("Extremely Unlikely", "Somewhat Likely", "Not Sure", "Somewhat Unlikely", "Extremely Likely"))  

fact_data$Job.Satisfaction <- fact_data$Job.Satisfaction %>% 
  factor(levels = c(1,2,3,4,5),
         labels = c("Extremely Unsatisfied", "Somewhat Unsatisfied", "Neutral", "Somewhat Satisfied", "Extremely Satisfied"))  

# Define server logic required to draw a histogram
function(input, output, session){
  #Here is the link for the About page  
  url <- a("SAGE research methods", href = "http://srmo.sagepub.com/datasets")  
  #Output for it  
  output$link <- renderUI({
    tagList("URL Link:", url)
  })
  
  #Starting with the numeric summaries choices, the action button requires an observeEvent() to make sure it runs only when it is submitted  
  observeEvent(input$num_button, {
    #Saving user selections of which variable to summarize for easy tracking  
    if("Age" %in% input$num_var){
      focus <- paste0("Age")
    }
    if("Evaluation" %in% input$num_var){
      focus <- paste0("Evaluation")
    }
    if("Salary" %in% input$num_var){
      focus <- paste0("Salary.grade")
    }
    if("Job Satisfaction" %in% input$num_var){
      focus <- paste0("Job.Satisfaction")
    }
    if("Tenure" %in% input$num_var){
      focus <- paste0("Tenure")
    }
    if("Intent to Quit" %in% input$num_var){
      focus <- paste0("Intention.to.Quit")
    }
    
    
    #Saving type of summary  
    type <- input$num_type  
    
    #Saving the numeric dataset as another object so as to not over write it  
    user_num <- num_data  
    
    #Making a conditional argument to subset the data so the summary can be calculated  
    if(input$num_subset == "Yes"){
      if("Ethnicity" %in% input$num_sub_options){
        user_num <- user_num[user_num$Ethnicity %in% input$num_sub_ethnicity, ]
      }
      if("Gender" %in% input$num_sub_options){
        user_num <- user_num[user_num$Gender %in% input$num_sub_gender, ]
      }
      if("Job Role" %in% input$num_sub_options){
        user_num <- user_num[user_num$JobRole %in% input$num_sub_roles, ]
      }
    }
    
    #Using if/else conditional arguments for the summary stat selection because only one option can be chosen at a time  
    if(type == "Mean"){
      calc <- round(mean(user_num[[focus]]), 3)
    } else if(type == "Median"){
      calc <- median(user_num[[focus]])
    } else if(type == "Minimum"){
      calc <- min(user_num[[focus]])
    } else{
      calc <- max(user_num[[focus]])
    }
    
    #This here pastes the summary stat result in a sentence for the user  
    output$sum_stat <- renderText({
      paste0("The ", type, " for ", input$num_var, " based on your subsetting choices is ", calc, ".")
    })
  })
  
  #Here is the observe event for the graphical summaries button  
  observeEvent(input$graph_button, {
    #Saving the graph selection as an object to put into a renderPlot later  
    if(input$graph == "Jitter"){
      graphing <- geom_jitter()
    } else if(input$graph == "Column Plot"){
      graphing <- geom_col()
    } else if(input$graph == "Regression"){
      graphing <- geom_smooth()
    } else {
      graphing <- geom_count()
    }
    
    #Saving the x axis selection as an object, the if else statement can be used because the user can only select one at a time  
    if(input$x == "Evaluation"){
      xaxis <- paste0("Evaluation")
    } else if(input$x == "Salary"){
      xaxis <- paste0("Salary.grade")
    } else if(input$x == "Job Satisfaction"){
      xaxis <- paste0("Job.Satisfaction")
    } else if(input$x == "Job Role"){
      xaxis <- paste0("JobRole")
    } else if(input$x == "Gender"){
      xaxis <- paste0("Gender")
    } else if(input$x == "Ethnicity"){
      xaxis <- paste0("Ethnicity")
    } else {
      xaxis <- paste0("Age")
    }
    
    #Saving the y axis selection as an object, the if else statement can be used because the user can only select one at a time  
    if(input$y == "Evaluation"){
      yaxis <- paste0("Evaluation")
    } else if(input$y == "Salary"){
      yaxis <- paste0("Salary.grade")
    } else if(input$y == "Job Satisfaction"){
      yaxis <- paste0("Job.Satisfaction")
    } else if(input$y == "Tenure"){
      yaxis <- paste0("Tenure")
    } else {
      yaxis <- paste0("Intention.to.Quit")
    }
    
    #Saving the user input as an object for easier input later into a renderPlot function  
    if(input$third_var == "Ethnicity"){
      third <- paste0("Ethnicity")
    } else if(input$third_var == "Job Role"){
      third <- paste0("JobRole")
    } else {
      third <- paste0("Gender")
    }
    
    #Saving the numeric dataset as another object so as to not over write it with the subsetting option  
    user_graph <- num_data  
    
    #Addressing the conditional argument to subset the data in the graphing section, this is practically the same code from the numeric part jsut with minor adjustments  
    if(input$more == "Yes"){
      if("Ethnicity" %in% input$sub_options){
        user_graph <- user_graph[user_graph$Ethnicity %in% input$sub_ethnicity, ]
      }
      if("Gender" %in% input$sub_options){
        user_graph <- user_graph[user_graph$Gender %in% input$sub_gender, ]
      }
      if("Job Role" %in% input$sub_options){
        user_graph <- user_graph[user_graph$JobRole %in% input$sub_roles, ]
      }
    }
    
    #Here is where every input of the user gets put into a graph! Super important. In order to conserve code the axis labels are with the data set labels and not the selection options that the UI shows the user. This is because these inputs were already previously assigned to a value, but in order to have the labels be dynamic with the user selection. Only the first one need to be defined by two variables becuase otherwise the other options will not be selected.
    output$graph_plot <- renderPlot({
      if(input$how_third == "Color" && input$more == "Yes"){
        ggplot(num_data, aes_string(x = xaxis, 
                                    y = yaxis, 
                                    color = third)) + 
                 graphing + 
                 xlab(xaxis) + 
                 ylab(yaxis)
      } else if(input$how_third == "Side-by-side Graphing"){
        ggplot(num_data, aes_string(x = xaxis, 
                                    y = yaxis)) + 
          graphing + 
          xlab(xaxis) + 
          ylab(yaxis) + 
          facet_grid(cols = vars(third))
      } else if(input$more == "Yes"){
        ggplot(user_graph, aes_string(x = xaxis, 
                                      y = yaxis)) + 
          graphing + 
          xlab(xaxis) + 
          ylab(yaxis)
      } else {
        ggplot(num_data, aes_string(x = xaxis, 
                                      y = yaxis)) + 
          graphing + 
          xlab(xaxis) + 
          ylab(yaxis)
      }
    })
  })
  
  #This is the observe event to fit both models at the same time. Like before, most options will be save first before a grand finale function to combine them all.  
  observeEvent(input$model, {
    #input$split is just the number doesn't need saving  
    
    #This code is somewhat similar to other checkboxGroupInputs before but the formula requires that a + is between each so here the predictors will be saved in a vector and then linked together in a formula afterwards  
    lm_p <- c()  
    
    if("Evaluation" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Evaluation")
    }
    if("Salary" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Salary.grade")
    }
    if("Job Satisfaction" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Job.Satisfaction")
    }
    if("Job Role" %in% input$lm_predictors){
      lm_p <- c(lm_p, "JobRole")
    }
    if("Gender" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Gender")
    }
    if("Ethnicity" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Ethnicity")
    }
    if("Intent to Quit" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Intention.to.Quit")
    }
    if("Age" %in% input$lm_predictors){
      lm_p <- c(lm_p, "Age")
    }
    
    #Now that all the possible selections can be added into the glm_p object they can be linked with a + in between  
    lm_string <- paste(lm_p, collapse = " + ")  
    
    #The same process above can be done for the random forest predictors  
    rf_p <- c()  
    
    if("Evaluation" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Evaluation")
    } if("Salary" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Salary.grade")
    } if("Job Satisfaction" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Job.Satisfaction")
    } if("Job Role" %in% input$rf_predictors){
      rf_p <- c(rf_p, "JobRole")
    } if("Gender" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Gender")
    } if("Ethnicity" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Ethnicity")
    } if("Intent to Quit" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Intention.to.Quit")
    } if("Age" %in% input$rf_predictors){
      rf_p <- c(rf_p, "Age")
    }
    
    #Now that all the possible selections can be added into the glm_p object they can be linked with a + in between  
    rf_string <- paste(rf_p, collapse = " + ")  
      
    
    #input$tune_grid is just a number, also doesn't need saving  
    
    #input$cv is also a number  
    
    #Now that all variables have been assigned, time to split the data  
    #Set seed for reproducibility  
    set.seed(333)  
    
    #Splitting the data  
    intrain <- createDataPartition(y = fact_data$Tenure, p = input$split, list = FALSE)  
    
    #Saving the splits  
    training <-fact_data[intrain,]  
    testing <-fact_data[-intrain,]  
    
    #Setting the trainControl for the glm() this did not have to be an option for the user to alter as per the instructions  
    lm_ctrl <- trainControl(method = "cv", number = 3)  
    
    #Fitting the lm model  
    lm_model <-train(Tenure ~ lm_string, data = training,
                      method = "lm",
                      trControl = lm_ctrl)  
    
    #Use predict() on training model so that the rmse can be calculated  
    training_lmpred <- predict(lm_model, newdata = training)  
    
    #With the Metrics library rmse can easily be calculated  
    training_lmrmse <- rmse(training$Tenure, training_lmpred)  
    
    #Then the same process is repeated for the testing predictions  
    testing_lmpred <- predict(lm_model, newdata = testing)  
    
    #Calculating rmse for testing model  
    testing_lmrmse <- rmse(testing$Tenure, testing_lmpred)  
    
    
    #The same process over for the random forest model  
    rf_ctrl <- trainControl(method = "cv", number = input$alter_cv)  
    #Fitting the rf model  
    rf_model <- train(Tenure ~ rf_string, data = training,
                      method = "rf",
                      trControl = rf_ctrl,
                      tuneGrid = expand.grid(mtry = tune_grid))  
    #Use predict() on training model so that the rmse can be calculated  
    training_rfpred <- predict(rf_model, newdata = training)  
    
    #With the Metrics library rmse can easily be calculated  
    training_rfrmse <- rmse(training$Tenure, training_rfpred)  
    
    #Then the same process is repeated for the testing predictions  
    testing_rfpred <- predict(rf_model, newdata = testing)  
    
    #Calculating rmse for testing model  
    testing_rfrmse <- rmse(testing$Tenure, testing_rfpred)
    
    #Putting results in a table to compare  
    renderUI(
      c(lm = testing_lmrmse, rf = testing_rfrmse)
    )
  })
}
