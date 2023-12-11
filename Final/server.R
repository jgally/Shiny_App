#Jasmine Gallaway
#ST 558 Final Project  
#12/12/23  
#
#
#Reading in libraries  
library(shiny)  
library(shinydashboard)  
library(tidyverse)  

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
}
