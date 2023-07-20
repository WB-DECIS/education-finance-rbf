library(shinydashboard)
library(DT)
library(shiny)
library(readxl)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library(formattable)
library(shinythemes)


# Load Data
rbf_data <- readRDS("data/rbf_data.rds")
dli_data <- readRDS("data/dli_data.rds")
temp = factor(unique(as.character(rbf_data$income_name)), 
              levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))

ui <-  shinyUI(
  
  
        navbarPage("Results-Based Financing in Education",
              
               theme = shinytheme("united"),
               tags$script(HTML('
           $( document ).on("shiny:sessioninitialized", function(event) {
                $(\'span[data-toggle="tooltip"]\').tooltip({
                    html: true
                });      
           });'
               )),
               
       tabPanel("Introduction",
                fluidPage(
                              includeHTML("www/About.html")
                )),
       tabPanel(span("RBF Projects", 
                     title = "Search or filter education RBF projects funded by the World Bank and approved between 2015 and 2021.",
                     `data-toggle`="tooltip",  `data-placement` ="bottom"),
                  fluidRow(  
                  column(width = 4,
                         pickerInput("region_rbf", "Region:",
                                     choices = c(sort(unique(as.character(rbf_data$region_name)))),
                                     selected = c(sort(unique(as.character(rbf_data$region_name)))),
                                     options = list(`actions-box` = TRUE), 
                                     multiple = TRUE)
                  ),
                  column(width = 4,
                         pickerInput("income_rbf", "Income Level:",
                                     choices = levels(factor(temp)),
                                     selected = levels(factor(temp)),
                                     options = list(`actions-box` = TRUE), 
                                     multiple = TRUE)
                  ),
                  
                  column(4,
                         pickerInput(inputId="country_rbf",
                                     label="Country:", 
                                     choices = c(sort(unique(rbf_data$Country))),
                                     multiple = TRUE,
                                     options = list(`actions-box` = TRUE)
                         )
                  )
                  ),
                  
                div(
                fluidRow(

                  column(width = 3,
                             pickerInput("lending_rbf", "Lending Instrument:",
                                         choices = levels(rbf_data$`Lending Instrument`),
                                         selected = levels(rbf_data$`Lending Instrument`),
                                         multiple = TRUE,
                                         options = list(`actions-box` = TRUE))),
                  
                  column(width = 1,
                    uiOutput("log_details2")),
                  
                  column(width = 3,
                         pickerInput("fiscal_rbf", "Fiscal Year of Approval:",
                                     choices = c(sort(unique(as.character(rbf_data$`Fiscal Year`)))),
                                     selected = c(sort(unique(as.character(rbf_data$`Fiscal Year`)))),
                                     multiple = TRUE,
                                     options = list(`actions-box` = TRUE))),
                  
                  column(width = 1,
                         shiny::uiOutput("log_details0")),
                  
                  tags$style(type='text/css', "#log_details2 { margin-top: 12px; margin-left: -25px;}"),
                  tags$style(type='text/css', "#log_details0 { margin-top: 25px; margin-left: -50px;}")

                )),
                
                fluidRow(
                         column(width = 12,
                                dataTableOutput("table_rbf")),
                         includeHTML("www/wb_logo.html")
                ),

                
       ),
               
        tabPanel(
                 span("Disbursement-linked Indicators", 
                      title = "Disbursement-linked Indicators (DLI) are the indicators that must be achieved for rewards or incentives to be disbursed in RBF projects.",
                      `data-toggle`="tooltip",  `data-placement` ="bottom"),
            fluidRow(

            column(width = 4,
                   pickerInput("region_dli", "Region:",
                               choices = c(sort(unique(as.character(dli_data$region_name)))),
                               selected = c(sort(unique(as.character(dli_data$region_name)))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE)
            ),
            
            column(width = 4,
                   pickerInput("income_dli", "Income:",
                               choices = levels(dli_data$income_name),
                               selected = levels(dli_data$income_name),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE)
            ),
            column(width = 4,
                   pickerInput("country_dli", "Country:",
                               choices =  c(sort(unique(as.character(dli_data$Country)))),
                               selected = c(sort(unique(as.character(dli_data$Country)))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE),
            ),
            column(width = 4,
                   pickerInput("fiscal_dli", "Fiscal Year of Approval:",
                               choices = c(sort(unique(dli_data$`Fiscal Year of Approval`))),
                               selected = c(sort(unique(as.character(dli_data$`Fiscal Year of Approval`)))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE),
            ),
            
            column(4,
                   pickerInput(inputId="edu_dli",
                               label="Level of Education:",
                               choices = levels(dli_data$`Level of Education`),
                               selected = levels(dli_data$`Level of Education`),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE
                   )
            ),
            
            column(4,
                   pickerInput(inputId="focus_dli",
                               label="Focus Area:",
                               choices = levels(dli_data$`Focus area`),
                               selected = levels(dli_data$`Focus area`),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE
                   )
            ),
          
            column(4,
                   pickerInput(inputId="topic_dli",
                               label="Topic:", 
                               choices = c(sort(unique(dli_data$Topic))),
                               multiple = TRUE,
                              options = list(`actions-box` = TRUE), 
                               )
            ),
        ),
            
            
            fluidRow(
                     column(width = 12,
                            dataTableOutput("table_dli")),
                     includeHTML("www/tab3.html")
            ),
        
        ),
       

      
       tabPanel("Definitions",
                fluidPage(
                    includeHTML("www/Definitions.html")
                )),

       
        )
    )
