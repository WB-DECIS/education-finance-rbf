library(shinydashboard)
library(shiny)
library(readxl)
library(DT)
library(tidyverse)
#library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
library("formattable")

#library(flextable)
#library(plotly)

rbf_data <- readRDS("data/rbf_data.rds")
dli_data <- readRDS("data/dli_data.rds")

dli_data$`Focus area` <- gsub("_", " ", dli_data$`Focus area`)

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
    #renderFormattable({
    rbf_data$`Project ID` = paste0("<a href='",rbf_data$Link,"'target=\'_blank\'>",rbf_data$`Project ID`,"</a>")
    rbf_data$`% of IBR/IDA Commitment as RBF` <- rbf_data$`RBF Amt IBRD/IDA`/rbf_data$`IBRD/IDA Commit Amt`
    rbf_data$`% of Trust Fund Commitment as RBF` <- rbf_data$`RBF Amt TF`/rbf_data$`TF Amt`
    
    rbf_data_table <- rbf_data %>%
      filter(region_name %in% input$region_rbf) %>%
      filter(income_name %in% input$income_rbf) %>%
      filter(Country %in% input$country_rbf) %>% 
      filter(`Fiscal Year` %in% input$fiscal_rbf) %>% 
      filter(`Level of Education` %in% input$edu_rbf) %>% 
      filter(`Lending Instrument` %in% input$lending_rbf) %>% 
      select(`Project Title`, `Project ID`,Country_original,
             region_name, income_name,
             `Lending Instrument`, 
             `Fiscal Year`, `Level of Education`,
             `Total commitment`,`Total RBF commitment`,`% of Project RBF`, 
             `IBRD/IDA Commit Amt`, `RBF Amt IBRD/IDA`, `% of IBR/IDA Commitment as RBF`,
             `TF Amt`, `RBF Amt TF`, `% of Trust Fund Commitment as RBF`,
             PDO) %>%
      rename("Region" = region_name, "Income" = income_name,
             "Country" = Country_original, "Project Name" =`Project Title`, 
             "Fiscal Year of Approval"= `Fiscal Year`, 
             "Project Development Objective (PDO)" = PDO,
             "IBRD/IDA Commitment" = `IBRD/IDA Commit Amt`,
             "RBF Commitment from IBRD/IDA" = `RBF Amt IBRD/IDA`,
             "Trust Fund Commitment" = `TF Amt`,
             "RBF Commitment from Trust Fund" =  `RBF Amt TF`)
  
  
    #formattable(rbf_data_table)
     DT::datatable(rbf_data_table,
                   class='cell-border stripe',
                   escape = FALSE,
                   extensions = c('Buttons', 'FixedColumns', "FixedHeader"), 
                   rownames = F,
                 
                   options=list(
                     dom = 'Bfrtip',
                     paging = TRUE,
                     pageLength = 20,
                     buttons = c('copy', 'csv', 'excel'),
                     fixedHeader = TRUE,
                     fixedColumns = list(leftColumns = 1),
                     scroller = TRUE
                     #scrollX = T
                     #scrollY = T
                   ) 
    
                  #,
                  #   callback=JS("table.on( 'order.dt search.dt', function () {
                  #                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                  #                                      cell.innerHTML = i+1;});}).draw();")
    )%>%formatPercentage(c("% of Project RBF", "% of IBR/IDA Commitment as RBF", "% of Trust Fund Commitment as RBF"), 2)
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
      p(HTML(paste0('<ul><b>Investment Project Financing</b> with Performance-Based Conditions (IPF-PBC) is a form of IPF in which all or part of the disbursements are conditional on the achievement of PBCs. For projects approved before January 2020, PBCs were referred to as Disbursement Linked Indicators. </ul>
                    <ul><b>Program-for-Results Financing (PforR)</b> uses partner countries and helps to improve the design and implementation of their development programs and achieve lasting results by strengthening institutions and building capacity.  PforR Financing proceeds are disbursed upon the achievement of verified results specified as disbursement-linked indicators.</ul>
                    <ul><b>Disbursement-Linked Indicators.</b> The DLIs are specific, measurable, and verifiable indicators related to and/or derived from the PforR Program development objectives and the results framework. DLIs may be expressed as outcomes, outputs, intermediate outcomes or outputs, process indicators, or financing indicators. DLIs may also be defined as actions or process results deemed critical for strengthening performance under the PforR Program (this could include actions for improving fiduciary, social and environmental issues and/or monitoring and evaluation), or as indicators of key institutional changes.</ul>'))),
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
        filter(region_name %in% input$region_dli) %>%
        filter(income_name %in% input$income_dli) %>%
        filter(Country %in% input$country_dli) %>%
        filter(`Fiscal Year` %in% input$fiscal_dli) %>% 
        filter(`Level of Education`%in% input$edu_dli)%>% 
        filter(`Focus area` %in% input$focus_dli)%>% 
        filter(Topic %in% input$topic_dli)%>%
          select(Country_original, region_name, income_name, 
                 `Level of Education`, `Lending Instrument`,
                 `Focus area`, Topic, DLI, `DLI/DLR`,
                 `Value of DLI (in mln)`,
                 `Value of DLR (mln)`, `Share of total RBF for DLI`,
                 `Chain Position`, `Formula/Calculation`, `Verification agency type`)%>%
      rename("Region" = region_name, "Income" = income_name,
             "Country" = Country_original)
      
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
                      buttons = c('copy', 'csv', 'excel')
                      #,
                      # scroller = T,
                      # scrollX = F,
                      # scrollY = T
                  ) 
                      )%>%formatPercentage(c("Share of total RBF for DLI"), 2)
    })
    

    
}
