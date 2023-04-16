library(shinydashboard)
library(shiny)
library(readxl)
library(DT)
library(tidyverse)
#library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
library("formattable")
library(dplyr)

#library(flextable)
#library(plotly)

rbf_data <- readRDS("data/rbf_data.rds")
dli_data <- readRDS("data/dli_data.rds")

dli_data$`Focus area` <- gsub("_", " ", dli_data$`Focus area`)
dli_data$`Level of Education` <- factor(dli_data$`Level of Education`, levels=c('Early Child Development', 'Primary Education', 
                                                                                'Secondary Education', 'Primary and Secondary Education',
                                                                                'Tertiary Education','Vocational Education and Training',
                                                                                'Lifelong Learning'))

# Reformat year
rbf_data$`Fiscal Year` <- sub("^", "FY", rbf_data$`Fiscal Year` %% 100)
dli_data$`Fiscal Year` <- sub("^", "FY", dli_data$`Fiscal Year` %% 100)

# Restructure the Lending Instrument Variable
rbf_data$`Lending Instrument` <- ifelse(rbf_data$`Lending Instrument` == "IPF/DLIs", "IBF with DLIs",
                                        ifelse(rbf_data$`Lending Instrument` == "IPF/PBC", "IBF with PBCs","The Same")) 
rbf_data$`Lending Instrument` <- factor(rbf_data$`Lending Instrument`,levels=c("IBF with PBCs","IBF with DLIs","PforR"))

# Reorder income level
rbf_data$income_name <- factor(rbf_data$income_name, levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))
dli_data$income_name <- factor(dli_data$income_name, levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))

# Reorder Focus Area
dli_data$`Focus area` <- factor(dli_data$`Focus area`, levels = c("Education Access and Equity", "Education Facilities",
                                                                  "Education Financing", 
                                                                  "Education Governance School Based Management",
                                                                  "Private Sector Delivery of Education", 
                                                                  "Science and Technology",
                                                                  "Skills Development", "Standards Curriculum and Textbooks",
                                                                  "Student Assessment","Teachers", "Other"))

# Clean up rbf_data
rbf_data$`Project ID` = paste0("<a href='",rbf_data$Link,"'target=\'_blank\'>",rbf_data$`Project ID`,"</a>")
rbf_data$`% of IBR/IDA Commitment as RBF` <- rbf_data$`RBF Amt IBRD/IDA`/rbf_data$`IBRD/IDA Commit Amt`
rbf_data$`% of Trust Fund Commitment as RBF` <- rbf_data$`RBF Amt TF`/rbf_data$`TF Amt`
    

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
    # observe({
    #     
    #     focuslist <- unique(df[df$Country %in% input$country,
    #                            # & 
    #                            #     df$`Level of Education` %in% input$edulist,
    #                            "Focus area"])
    #     updateSelectInput(session, "focus_area",
    #                       choices = focuslist, # update choices
    #                       #selected = focuslist[1]
    #                       ) 
    # })
    # 
    # observe({
    #     topiclist <- unique(df[df$`Focus area` %in% input$focus_area,
    #                            # & 
    #                            #   df$`Level of Education` %in% input$edulist & 
    #                            #     df$Country %in% input$country, 
    #                            "Topic"])
    #     updateSelectInput(session, "topic",
    #                       choices = topiclist, # update choices
    #                       #selected = topiclist[1] 
    #     ) 
    # })
  
  output$table_rbf <- renderDataTable({
    
    rbf_data_table <- rbf_data %>%
      filter(region_name %in% input$region_rbf) %>%
      filter(income_name %in% input$income_rbf) %>%
      filter(Country %in% input$country_rbf) %>% 
      filter(`Fiscal Year` %in% input$fiscal_rbf) %>% 
      filter(`Level of Education` %in% input$edu_rbf) %>% 
      filter(`Lending Instrument` %in% input$lending_rbf) %>% 
      select(`Project Title`, 
             `Project ID`,
             Country_original,
             region_name, 
             income_name,
             `Lending Instrument`, 
             `Fiscal Year`, 
             `Closing date`,
             `Level of Education`,
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
             "Fiscal Year of Approval"= `Fiscal Year`, 
             "Project Development Objective (PDO)" = PDO,
             "IBRD/IDA Commitment" = `IBRD/IDA Commit Amt`,
             "RBF Commitment from IBRD/IDA" = `RBF Amt IBRD/IDA`,
             "Trust Fund Commitment" = `TF Amt`,
             "RBF Commitment from Trust Fund" =  `RBF Amt TF`,
             "% of Total Commitment as RBF" = `% of Project RBF`
             )
    
  
     DT::datatable(rbf_data_table,
             
             class='cell-border stripe',
             escape = FALSE,
             extensions = c('Buttons', 'FixedColumns', "FixedHeader"), 
             rownames = F,
             caption = "All committed amounts are expressed in millions of US dollars and dated as of April 30, 2021.",
             options=list(
               autoWidth = FALSE,
               dom = 'Bfrtip',
               paging = FALSE,
               pageLength = 20,
               buttons = c('copy', 'csv', 'excel'),
               fixedHeader = TRUE,
               fixedColumns = list(leftColumns = 1),
               scroller = TRUE,
               columnDefs = list(
                list(className = 'dt-center', targets = '_all'),
                list(width = '400px', targets = 10))
             )
    ) %>%
       formatPercentage(c("% of Total Commitment as RBF", "% of IBR/IDA Commitment as RBF", "% of Trust Fund Commitment as RBF"), 2)
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
                    <ul><b> Program-for-Results Financing (PforR) </b> aims to help partner countries to improve the design and implementation of their development programs and achieve lasting results by strengthening institutions and building capacity. PforR Financing proceeds are disbursed upon the achievement of verified results specified as disbursement-linked indicators. </ul>'))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
    
    
    # output$bar <- renderHighchart({
    #     #dat <- df %>% filter(Country %in% input$country)%>% 
    #     # if (!("All" %in% input$country)) {
    #     #     df1 <- df %>%
    #     #         filter((Country %in% input$country)) 
    #     # }  else {
    #     #     df1 <- df
    #     # }
    #     
    #   dat <- rbf_data %>%
    #     filter(Region %in% input$region_d) %>%
    #     filter(Country %in% input$country_d) %>%
    #     filter(`Level of Education`%in% input$edu)%>% 
    #     filter(`Focus area` %in% input$focus_area)%>% 
    #     filter(Topic %in% input$topic)
    #     
    #     bar <- hchart(dat, 
    #         type = "bar",
    #         hcaes(x = DLI, y = `Value of DLI (in mln)`)
    #         )
    #         
    #     
    # })
    
    
    output$table_dli <- renderDataTable({
      dli_data_table <- dli_data %>%
        mutate(`Value of DLI (in mln)` = round(`Value of DLI (in mln)`,2)) %>%
        mutate(`Value of DLR (mln)` = round(`Value of DLR (mln)`,2)) %>%
        filter(region_name %in% input$region_dli) %>%
        filter(income_name %in% input$income_dli) %>%
        filter(Country %in% input$country_dli) %>%
        filter(`Fiscal Year` %in% input$fiscal_dli) %>% 
        filter(`Level of Education`%in% input$edu_dli)%>% 
        filter(`Focus area` %in% input$focus_dli)%>% 
        filter(Topic %in% input$topic_dli)%>%
          select(`Project ID`,
                 Country_original, 
                 region_name, 
                 income_name, 
                 `Lending Instrument`,
                 `Fiscal Year`,
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
      rename("Region" = region_name, 
             "Income Level" = income_name,
             "Country" = Country_original,
             "Disbursement-linked Indicators (DLIs)" = DLI,
             "Value of DLI (in million USD)" = `Value of DLI (in mln)`,
             "Value of DLR (in million USD)" = `Value of DLR (mln)`, 
             "Share of total RBF for this DLI" = `Share of total RBF for DLI`
             )
      
        # unit.scale = function(x) (x - 0) / (1 - 0)
        # formattable(dli_data_table, align=c("l","l","l","l","l","l","l","l", "r", "r", "r", "r"),
        #             list(
        #                 `Share of total RBF for DLI` = percent,
        #                 `Share of total RBF for DLI`= color_bar("lightblue", fun = unit.scale)
        #                 
        #             ))
      
      DT::datatable(dli_data_table,
                    class='cell-border stripe',
                    escape = FALSE,
                    extensions = c ('Buttons', 'FixedHeader', 'FixedColumns'), 
                    rownames = F,
                    options=list(
                      dom = 'Bfrtip',
                      paging = FALSE,
                      fixedHeader=TRUE,
                      fixedColumns = list(leftColumns = 1),
                      buttons = c('copy', 'csv', 'excel'),
                      columnDefs = list(
                        list(className = 'dt-center', targets = '_all'))
                  ) 
                      ) %>% 
        formatPercentage(c("Share of total RBF for this DLI"), 2)
 
       })
 
}
