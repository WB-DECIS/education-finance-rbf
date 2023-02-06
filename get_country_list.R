daturl = paste("https://api.worldbank.org/v2/country?per_page=300&format=json")
dat_raw = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]
dat = lapply(dat_raw, function(j) 
  cbind(j$id, j$name, j$region['id'], j$region['value'],
        j$incomeLevel['id'], j$incomeLevel['value']
        )) # can add source note here
dat = data.frame(do.call('rbind', dat), stringsAsFactors = FALSE)
colnames(dat) = c('country_id', 'country_name', 
                  'region_id', 'region_name',
                  'income_id', 'income_name')

rownames(dat) <- NULL
dat$income_name <- trimws(dat$income_name)
dat$region_name <- trimws(dat$region_name)


write.csv(dat, 'data/country_list.csv')



