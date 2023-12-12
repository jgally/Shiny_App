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
    } else if(input$graph == "Density"){
      graphing <- geom_density()
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
    } else if(input$x == "Tenure"){
      xaxis <- paste0("Tenure")
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
}
