#Jasmine Gallaway
#ST 558 Final Project  
#12/12/23  
#
#Libraries to read in  
library(shiny)
library(shinydashboard)  

#Read in dataset file  


# Define UI using shiny dashboard  
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
              h3("This is my about page"))
    )
  )
)
