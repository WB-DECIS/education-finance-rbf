library(shinydashboard)
#library(formattable)
library(DT)
library(shiny)
library(readxl)
library(tidyverse)
library(shinycssloaders)
#library(highcharter)
library(shinyWidgets)
library("formattable")


rbf_data <- readRDS("data/rbf_data.rds")
dli_data <- readRDS("data/dli_data.rds")
dli_data$`Focus area` <- gsub("_", " ", dli_data$`Focus area`)
dli_data$`Level of Education` <- factor(dli_data$`Level of Education`, levels=c('Early Child Development', 'Primary Education', 
                                                                    'Secondary Education', 'Primary and Secondary Education',
                                                                    'Tertiary Education','Vocational Education and Training',
                                                                    'Lifelong Learning'))

# set the order of income groups
temp = factor(unique(as.character(rbf_data$income_name)), 
              levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))

#rbf_data$`Fiscal Year` <- sub("^", "FY", rbf_data$`Fiscal Year`) # current dataset has no "FY", this line can be removed after the data update

ui <-  shinyUI(
    
        navbarPage("Results-Based Financing in Education",
               #theme = "R-shiny-style-cwon-tim.css",
               theme = "custom_hd.css",
               # add tooltips
               tags$script(HTML('
           $( document ).on("shiny:sessioninitialized", function(event) {
                $(\'span[data-toggle="tooltip"]\').tooltip({
                    html: true
                });      
           });'
               )),
               
       tabPanel("Introduction",
                fluidPage(
                      #    mainPanel(fluidRow(                    
                              includeHTML("www/About.html")
                         # )
                         # )
                )),
       tabPanel(span("RBF Projects", 
                     title = "Search or filter education RBF projects funded by the World Bank and approved between 2015 and 2021.",
                     `data-toggle`="tooltip",  `data-placement` ="bottom"),
                  fluidRow(  
                  #includeHTML("www/tab2.html"),
                  #style = "border-right: 2px dotted #e68534; padding-left: 50px;",
                  column(width = 4,
                         pickerInput("region_rbf", "Region:",
                                     choices = c(sort(unique(as.character(rbf_data$region_name)))),
                                     # selected = c("Sub-Saharan Africa"
                                     #               ),
                                     selected = c(sort(unique(as.character(rbf_data$region_name)))),
                                     options = list(`actions-box` = TRUE), 
                                     multiple = TRUE)
                  ),
                  column(width = 4,
                         pickerInput("income_rbf", "Income:",
                                     choices = levels(factor(temp)),
                                     #selected = c('Lower middle income', "Low income", "middle income"),
                                     selected = c(sort(unique(as.character(rbf_data$income_name)))),
                                     options = list(`actions-box` = TRUE), 
                                     multiple = TRUE)
                  ),
                  
                  column(4,
                         pickerInput(inputId="country_rbf",
                                     label="Country:", 
                                     choices = c(sort(unique(rbf_data$Country))),
                                     multiple = TRUE,
                                     #selected = c(sort(unique(as.character(rbf_data$Country)))),
                                     options = list(`actions-box` = TRUE)
                                     # selected = c("Afghanistan", "Argentina", "Bangladesh", "Brazil", 
                                     #              "Chile",  "Honduras", "India",
                                     #              "Nepal"
                                     #              
                                     # )
                         )
                  ),
                  
                  column(width = 4,
                         pickerInput("fiscal_rbf", "Fiscal Year of Approval:",
                                     choices = c(sort(unique(as.character(rbf_data$`Fiscal Year`)))),
                                     # selected = c("2016", "2017", "2018", "2019", "2020", "2021"),
                                     selected = c(sort(unique(as.character(rbf_data$`Fiscal Year`)))),
                                     multiple = TRUE,
                                     options = list(`actions-box` = TRUE)
                         ),
                        # align = "right",
                        shiny::uiOutput(("log_details0"))
                  ),
                  # shiny::column(1, align = "right",
                  #               shiny::uiOutput(("log_details0"))
                  # ),
                column(width = 4,
                       pickerInput(inputId="edu_rbf",
                              label="Level of Education:", 
                              choices = levels(dli_data$`Level of Education`),
                              selected = levels(dli_data$`Level of Education`),
                              # selected = c("Early Child Development", "Primary and Secondary Education", 
                              #              "Tertiary Education"),
                              # selected = c(sort(unique(rbf_data$`Level of Education`))),
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)
                  )
                ),
                  column(width = 4, 
                         pickerInput("lending_rbf", "Lending Instrument:",
                                     choices = c(sort(unique(rbf_data$`Lending Instrument`))),
                                     # selected = c('IPF/DLIs', "IPF/PBC", "PforR"),
                                     selected = c(sort(unique(rbf_data$`Lending Instrument`))),
                                     multiple = TRUE,
                                     options = list(`actions-box` = TRUE)
                                     ),
                         shiny::uiOutput(("log_details2"))
                  ),
                ),
                fluidRow(
                         column(width = 12,
                                dataTableOutput("table_rbf")),
                                #formattableOutput("table_rbf"))
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
                               #selected = c(sort(unique(as.character(dli_data$region_name)))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE)
            ),
            
            column(width = 4,
                   pickerInput("income_dli", "Income:",
                               choices = c(sort(unique(as.character(dli_data$income_name)))),
                               selected = c(sort(unique(as.character(dli_data$income_name)))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE)
            ),
            column(width = 4,
                   pickerInput("country_dli", "Country:",
                               choices =  c(sort(unique(as.character(dli_data$Country)))),
                               selected = c(sort(unique(as.character(dli_data$Country)))),
                               # selected = c("Afghanistan", "Argentina", "Bangladesh", "Brazil",
                               #              "Chile",  "Honduras", "India",
                               #              "Nepal"
                               # 
                               # ),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE),
            ),
            column(width = 4,
                   pickerInput("fiscal_dli", "Fiscal Year of Approval:",
                               choices = c(sort(unique(dli_data$`Fiscal Year`))),
                               selected = c(sort(unique(as.character(dli_data$`Fiscal Year`)))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE),
            ),
            
            column(4,
                   pickerInput(inputId="edu_dli",
                               label="Level of Education:",
                               choices = levels(dli_data$`Level of Education`),
                               # choices = c(sort(unique(dli_data$`Level of Education`))),
                               selected = levels(dli_data$`Level of Education`),
                               # selected = c(sort(unique(dli_data$`Level of Education`))),
                               # selected = c("Early Child Development", "Primary and Secondary Education", 
                               #              "Tertiary Education"),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE
                   )
            ),
            
            column(4,
                   pickerInput(inputId="focus_dli",
                               label="Focus Area:",
                               choices = c(sort(unique(dli_data$`Focus area`))),
                               selected = c(sort(unique(dli_data$`Focus area`))),
                               options = list(`actions-box` = TRUE), 
                               multiple = TRUE
                   )
            ),
          
            column(4,
                   pickerInput(inputId="topic_dli",
                               label="Topic:", 
                               choices = c(sort(unique(dli_data$Topic))),
                              #selected = "Teacher Performance Assessment",
                               multiple = TRUE,
                              options = list(`actions-box` = TRUE), 
                               )
            ),
        ),
            
            
            fluidRow(
              #column( width = 12, highchartOutput('bar')),
                     #column( width = 12, tableOutput("data")),
                     column(width = 12,
                            dataTableOutput("table_dli")),
                            #formattableOutput("table"))
                     includeHTML("www/tab3.html")
            ),
        
        ),
       

      
       tabPanel("Definitions",
                fluidPage(
                 # mainPanel(fluidRow(                    
                    includeHTML("www/Definitions.html")
                 # )
                #  )
                )),

       
        )
    )
