
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
  rbf_data$`Level of Education` <- ifelse(rbf_data$`Level of Education` == 'Early Child Development', 
                                          'Early Childhood Development', rbf_data$`Level of Education`)
  rbf_data$`Level of Education` <- factor(rbf_data$`Level of Education`, levels=c('Early Childhood Development', 
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
  
  # Clean up rbf_data
  rbf_data$`Project ID` = paste0("<a href='",rbf_data$Link,"'target=\'_blank\'>",rbf_data$`Project ID`,"</a>")
  rbf_data$`% of IBR/IDA Commitment as RBF` <- rbf_data$`RBF Amt IBRD/IDA`/rbf_data$`IBRD/IDA Commit Amt`
  rbf_data$`% of Trust Fund Commitment as RBF` <- rbf_data$`RBF Amt TF`/rbf_data$`TF Amt`


saveRDS(rbf_data, "data/rbf_data.rds")
saveRDS(dli_data, "data/dli_data.rds")


