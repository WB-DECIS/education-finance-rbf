library(shinydashboard)
library(shiny)
library(readxl)
library(DT)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library("formattable")
library(dplyr)


# Load Data
rbf_data <- readRDS("data/rbf_data.rds")
dli_data <- readRDS("data/dli_data.rds")
temp = factor(unique(as.character(rbf_data$income_name)), 
              levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))
    

shinyServer = function(input, output, session) {
  observe({
        country_list_rbf <- as.character(sort(unique(rbf_data[rbf_data$region_name %in% input$region_rbf, "Country"])))
        updatePickerInput(session, "country_rbf",
                          choices = country_list_rbf,# update choices,
                          selected = country_list_rbf

        )
    })
  
  observe({
    country_list_dli <- as.character(sort(unique(dli_data[dli_data$region_name %in% input$region_dli, "Country"])))
    updatePickerInput(session, "country_dli",
                      choices = country_list_dli,# update choices,
                      selected = country_list_dli
                      
    )
  })
  
  observe({
    topic_list_dli <- as.character(sort(unique(dli_data[dli_data$`Focus area` %in% input$focus_dli, "Topic"])))
    updatePickerInput(session, "topic_dli",
                      choices = topic_list_dli,# update choices,
                      selected = topic_list_dli
                      
    )
  })

  output$table_rbf <- renderDT({
    
    rbf_data_table <- rbf_data %>%
      filter(region_name %in% input$region_rbf) %>%
      filter(income_name %in% input$income_rbf) %>%
      filter(Country %in% input$country_rbf) %>% 
      filter(`Fiscal Year of Approval` %in% input$fiscal_rbf) %>% 
      filter(`Lending Instrument` %in% input$lending_rbf) %>% 
      select(`Project Title`, 
             `Project ID`,
             Country_original,
             region_name, 
             income_name,
             `Lending Instrument`, 
             `Fiscal Year of Approval`, 
             `Closing date`,
             `Lending Instrument`,
             PDO,
             `Total commitment`,
             `Total RBF commitment`,
             `% of Project RBF`, 
             `IBRD/IDA Commit Amt`, 
             `RBF Amt IBRD/IDA`, 
             `% of IBR/IDA Commitment as RBF`,
             `TF Amt`, 
             `RBF Amt TF`, 
             `% of Trust Fund Commitment as RBF`
             ) %>%
      rename("Region" = region_name, 
             "Income Level" = income_name,
             "Country" = Country_original, 
             "Project Name" =`Project Title`, 
             "Project Development Objective (PDO)" = PDO,
             "IBRD/IDA Commitment" = `IBRD/IDA Commit Amt`,
             "RBF Commitment from IBRD/IDA" = `RBF Amt IBRD/IDA`,
             "Trust Fund Commitment" = `TF Amt`,
             "RBF Commitment from Trust Fund" =  `RBF Amt TF`,
             "% of Total Commitment as RBF" = `% of Project RBF`,
             "% of IBRD/IDA Commitment as RBF" = `% of IBR/IDA Commitment as RBF`
             )
    
    
  
     DT::datatable(rbf_data_table,
             
             class='cell-border stripe',
             escape = FALSE,
             extensions = c('Buttons', 'FixedColumns', "FixedHeader"), 
             rownames = F,
             caption = "All committed amounts are expressed in millions of US dollars and dated as of April 30, 2021.",
             options=list(
               autoWidth = TRUE,
               dom = 't',
               paging = TRUE,
               pageLength = 20,
               buttons = c('copy', 'csv', 'excel'),
               fixedHeader = TRUE,
               fixedColumns = list(leftColumns = 1),
               scroller = TRUE,
               columnDefs = list(
                list(className = 'dt-center', targets = '_all'))
             )
    ) %>%
       formatPercentage(c("% of Total Commitment as RBF", "% of IBRD/IDA Commitment as RBF", "% of Trust Fund Commitment as RBF"), 2)
  })
  
  output$log_details0 <- shiny::renderUI({
    
      shiny::actionButton(("log_details"), "", icon = shiny::icon("info"))

  })
  shiny::observeEvent(input$log_details, {
    shiny::showModal(shiny::modalDialog(
      title = span("", style = "font-weight: bold; color:black; font-size:20px"),
      p(HTML(paste0('The fiscal year for the World Bank Group runs from July 1 of a given calendar year to June 30 of the following calendar year. 
                    For example, FY21 runs from July 1, 2020 to June 30, 2021.'))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$log_details2 <- shiny::renderUI({
    
    shiny::actionButton(("log_details2"), "", icon = shiny::icon("info"))
    
  })
  shiny::observeEvent(input$log_details2, {
    shiny::showModal(shiny::modalDialog(
      title = span("", style = "font-weight: bold; color:black; font-size:20px"),
      p(HTML(paste0('<ul><b>Investment Project Financing with Performance-Based Conditions (IPF with PBCs) </b> is a form of IPF in which all or part of the disbursements are conditional on the achievement of PBCs. For projects approved before January 2020, PBCs were referred to as Disbursement Linked Indicators (DLIs).  </ul>
                    <ul><b>Investment Project Financing with Disbursement-Linked Indicators (IPF with DLIs). </b> The DLIs are specific, measurable, and verifiable indicators related to and/or derived from project development objectives and the results framework. DLIs may be expressed as outcomes, outputs, intermediate outcomes or outputs, process indicators, or financing indicators. DLIs may also be defined as actions or process results deemed critical for strengthening performance. Since 2020, IPFs with DLIs have been called IPFs with PBCs. </ul>
                    <ul><b> Program-for-Results Financing (PforR) </b> aims to help partner countries to improve the design and implementation of their development programs and achieve lasting results by strengthening institutions and building capacity. PforR Financing proceeds are disbursed upon the achievement of verified results specified as disbursement-linked indicators. </ul>
                    <ul> <b> Sources: </b>
                    <ul> Guidance: <a href="https://www.worldbank.org/en/what-we-do/products-and-services/financing-instruments/investment-project-financing" target="blank"> Investment Project Financing with Performance-Based Conditions </a> </ul>
                    <ul> <a href ="https://policies.worldbank.org/en/policies/all/ppfdetail/dad74959-885e-4d01-ab19-1e77f70adbe0" target="blank"> Bank directive: Program-for-results financing. </a>  Revised on March 8, 2022 </ul>'))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
    
    
    output$table_dli <- renderDataTable({
      dli_data_table <- dli_data %>%
        mutate(`Value of DLI (in mln)` = round(`Value of DLI (in mln)`,2)) %>%
        mutate(`Value of DLR (mln)` = round(`Value of DLR (mln)`,2)) %>%
        filter(region_name %in% input$region_dli) %>%
        filter(income_name %in% input$income_dli) %>%
        filter(Country %in% input$country_dli) %>%
        filter(`Fiscal Year of Approval` %in% input$fiscal_dli) %>% 
        filter(`Level of Education`%in% input$edu_dli)%>% 
        filter(`Focus area` %in% input$focus_dli)%>% 
        filter(Topic %in% input$topic_dli)%>%
          select(`Project Title`,
                `Project ID`,
                 Country_original, 
                 region_name, 
                 income_name, 
                 `Lending Instrument`,
                 `Fiscal Year of Approval`,
                 `Level of Education`, 
                 `Focus area`, 
                 Topic, 
                 DLI, 
                 `DLI/DLR`,
                 `Value of DLI (in mln)`,
                 `Value of DLR (mln)`, 
                 `Share of total RBF for DLI`,
                 Scalability,
                 `Chain Position`, 
                 `Formula/Calculation`, 
                 `Verification agency type`,
                 `Verification source`,
                 Restructuring) %>%
      rename("Project Name" =`Project Title`, 
            "Region" = region_name, 
             "Income Level" = income_name,
             "Country" = Country_original,
             "Disbursement-linked Indicators (DLIs)" = DLI,
             "Value of DLI (in million USD)" = `Value of DLI (in mln)`,
             "Value of DLR (in million USD)" = `Value of DLR (mln)`, 
             "Share of total RBF for this DLI" = `Share of total RBF for DLI`
             )
      
      
      DT::datatable(
        dli_data_table,
        class = 'cell-border stripe hover',
        rownames = FALSE,
        callback = JS(paste0("
          var tips = ", jsonlite::toJSON(colnames(dli_data_table)), ",
              header = table.columns().header();
          for (var i = 0; i < tips.length; i++) {
            $(header[i]).attr('title', tips[i]);
          $(header[11]).attr('title', 'A Disbursement Linked Result (DLR) is a target or milestone of a DLI that must be achieved in order to receive disbursements.');
          }
        ")),
        escape = FALSE,
        extensions = c('Buttons', 'FixedHeader', 'FixedColumns'),
        options = list(
          dom = 't',
          paging = TRUE,
          pageLength = 20,
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1),
          buttons = c('copy', 'csv', 'excel'),
          scroller = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = '200px', targets = 9),  # Adjusted to target the 10th column (0-indexed)
            list(className = 'dt-center', targets = '_all')
          )
        )
      )
      
       })
 
}
