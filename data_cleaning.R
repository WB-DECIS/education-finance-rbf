
library(readxl)
library(tidyverse)
library(dplyr)

# Load data
rbf_data <- readRDS("data/rbf_data.rds") %>%
  rename(`Fiscal Year of Approval` = `Fiscal Year`)
dli_data <- readRDS("data/dli_data.rds") %>%
  rename(`Fiscal Year of Approval` = `Fiscal Year`)

# Fix closing date format
rbf_data$`Closing date` <- format(rbf_data$`Closing date`, "%Y-%m-%d")

  # Clean Focus Area
  dli_data$`Focus area` <- gsub("_", " ", dli_data$`Focus area`)
  dli_data$`Focus area` <- factor(dli_data$`Focus area`, levels = c("Education Access and Equity", "Education Facilities",
                                                                    "Education Financing", 
                                                                    "Education Governance School Based Management",
                                                                    "Private Sector Delivery of Education", 
                                                                    "Science and Technology",
                                                                    "Skills Development", "Standards Curriculum and Textbooks",
                                                                    "Student Assessment","Teachers", "Other"))
  
  # Clean Education Levels
  dli_data$`Level of Education` <- ifelse(dli_data$`Level of Education` == 'Early Child Development', 
                                          'Early Childhood Development', dli_data$`Level of Education`)
  dli_data$`Level of Education` <- factor(dli_data$`Level of Education`, levels=c('Early Childhood Development', 
                                                                                  'Primary Education', 
                                                                                  'Secondary Education', 
                                                                                  'Primary and Secondary Education',
                                                                                  'Tertiary Education',
                                                                                  'Vocational Education and Training',
                                                                                  'Lifelong Learning'))
  
  # Reformat year
  rbf_data$`Fiscal Year of Approval` <- sub("^", "FY", rbf_data$`Fiscal Year` %% 100) 
  dli_data$`Fiscal Year of Approval` <- sub("^", "FY", dli_data$`Fiscal Year` %% 100)
  
  # Restructure the Lending Instrument Variable
  rbf_data$`Lending Instrument` <- ifelse(rbf_data$`Lending Instrument` == "IPF/DLIs", "IPF with DLIs",
                                          ifelse(rbf_data$`Lending Instrument` == "IPF/PBC", "IPF with PBCs","The Same")) 
  rbf_data$`Lending Instrument` <- factor(rbf_data$`Lending Instrument`,levels=c("IPF with PBCs","IPF with DLIs","PforR"))
  dli_data$`Lending Instrument` <- ifelse(dli_data$`Lending Instrument` == "IPF/DLIs", "IPF with DLIs",
                                          ifelse(dli_data$`Lending Instrument` == "IPF/PBC", "IPF with PBCs","The Same")) 
  dli_data$`Lending Instrument` <- factor(dli_data$`Lending Instrument`,levels=c("IPF with PBCs","IPF with DLIs","PforR"))
  
  # Reorder income level
  temp = factor(unique(as.character(rbf_data$income_name)), 
                levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))
  rbf_data$income_name <- factor(rbf_data$income_name, levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))
  dli_data$income_name <- factor(dli_data$income_name, levels = c("Low income", "Lower middle income",  "Upper middle income", "High income"))
  
  
  # Edit Link for Project P173314
  rbf_data$Link = ifelse(rbf_data$`Project ID` == "P173314",
                         "https://documents1.worldbank.org/curated/en/506841602499970579/pdf/Project-Information-Document-Integrated-Safeguards-Data-Sheet-EQRA-Additional-Financing-P173314.pdf",
                         rbf_data$`Link`)
  
  dli_data$Link = ifelse(dli_data$`Project ID` == "P173314",
                         "https://documents1.worldbank.org/curated/en/506841602499970579/pdf/Project-Information-Document-Integrated-Safeguards-Data-Sheet-EQRA-Additional-Financing-P173314.pdf",
                         dli_data$`Link`)
  
  # Combine Link w/ Project ID
  rbf_data$`Project ID` = ifelse(is.na(rbf_data$Link),  rbf_data$`Project ID`, 
                                 paste0("<a href='",rbf_data$Link,"' target = _blank>",rbf_data$`Project ID`,"</a>"))
  dli_data$`Project ID` = ifelse(is.na(dli_data$Link),  dli_data$`Project ID`, 
                                 paste0("<a href='",dli_data$Link,"' target= _blank>",dli_data$`Project ID`,"</a>"))

  # Clean up rbf_data
  rbf_data$`% of IBR/IDA Commitment as RBF` <- rbf_data$`RBF Amt IBRD/IDA`/rbf_data$`IBRD/IDA Commit Amt`
  rbf_data$`% of Trust Fund Commitment as RBF` <- rbf_data$`RBF Amt TF`/rbf_data$`TF Amt`
  
  # In DLI table, make Share of Total RBF for DLI "Not Applicable" if not DLI
  dli_data$`Share of total RBF for DLI` <- ifelse(dli_data$`DLI/DLR` != "DLI", "Not Applicable",  
                                                  paste(round((dli_data$`Share of total RBF for DLI`*100),2),"%"))
  
  dli_data <- dli_data %>%
    dplyr::mutate(region_name = ifelse(Country %in% c("Angola","Botswana","Burundi","Comoros","Congo, Democratic Republic of","Eritrea","Eswatini",
                                                 "Ethiopia","Kenya","Lesotho","Madagascar","Malawi","Mauritius","Mozambique","Namibia","Rwanda",
                                                 "Sao Tome and Principe","Seychelles","Somalia","South Africa","South Sudan","Sudan","Tanzania",
                                                 "Uganda","Zambia","Zimbabwe","Congo, Dem. Rep."), "Africa Eastern and Southern",
                                  ifelse(Country %in% c("Benin","Burkina Faso","Cabo Verde","Cameroon","Central African Republic","Chad",
                                                        "Congo, Republic of","Cote d'Ivoire","Equatorial Guinea","Gabon","Gambia, The","Ghana",
                                                        "Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal",
                                                        "Sierra Leone","Togo","Congo, Rep."),"Africa Western and Central", region_name)))
  
  rbf_data <- rbf_data %>%
    dplyr::mutate(region_name = ifelse(Country %in% c("Angola","Botswana","Burundi","Comoros","Congo, Democratic Republic of","Eritrea","Eswatini",
                                                 "Ethiopia","Kenya","Lesotho","Madagascar","Malawi","Mauritius","Mozambique","Namibia","Rwanda",
                                                 "Sao Tome and Principe","Seychelles","Somalia","South Africa","South Sudan","Sudan","Tanzania",
                                                 "Uganda","Zambia","Zimbabwe","Congo, Dem. Rep."), "Africa Eastern and Southern",
                                  ifelse(Country %in% c("Benin","Burkina Faso","Cabo Verde","Cameroon","Central African Republic","Chad",
                                                        "Congo, Republic of","Cote d'Ivoire","Equatorial Guinea","Gabon","Gambia, The","Ghana",
                                                        "Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria","Senegal",
                                                        "Sierra Leone","Togo","Congo, Rep."),"Africa Western and Central", region_name)))


  
  # Remove quotes from PDO string
  rbf_data <- rbf_data %>% mutate(PDO = gsub('"','',PDO))

saveRDS(rbf_data, "data/rbf_data.rds")
saveRDS(dli_data, "data/dli_data.rds")


