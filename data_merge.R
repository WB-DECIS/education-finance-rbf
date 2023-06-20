library(readxl)
library(tidyverse)
library(fuzzyjoin)

df_rbf <- read_excel("data/2_DLI database updated FY2021_04082022 ext only.xlsx",
                     sheet = "RBF Projects") %>%
  select(1:22) # keep first 22 columns


df_dli <- read_excel("data/2_DLI database updated FY2021_04082022 ext only.xlsx",
                 sheet = "DLIs&DLRs") %>% 
  drop_na(`Project ID`)

dat <- read.csv('data/country_list.csv')
dat <- rename(dat, Country = country_name)

df_rbf_new <- merge(df_rbf, dat, by='country_id', all.x = TRUE) %>%
  select( -X, -Country.y) %>%
  rename(Country = Country.x)


id_fiscal <- distinct(df_rbf_new[, c("Project Title", "Project ID", "Fiscal Year", "country_id", "Country", "Link")])

df_dli_new <- merge(x = df_dli, y = id_fiscal, by = "Project ID", all.x = TRUE)


df_dli_new <- merge(df_dli_new, dat, by='country_id', all.x = TRUE) %>%
  select( -X, -Country.y) %>%
  rename(Country = Country.x)

write.csv(df_dli_new, 'data/dli_data.csv')
write.csv(df_rbf_new, "data/rbf_data.csv")

saveRDS(df_rbf_new, "data/rbf_data.rds")
saveRDS(df_dli_new, "data/dli_data.rds")
