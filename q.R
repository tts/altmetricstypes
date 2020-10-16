library(readxl)
library(writexl)
library(rAltmetric)
library(tidyverse)

key <- Sys.getenv("altmetricKey")
options("altmetricKey"=key)

rawdata <- read_xlsx("PLUMX_Aalto sustis tagatty Pure 2016-2019.xlsx")

ids <- as.list(rawdata$DI)

# https://gist.github.com/karthik/78016fc78d52156561f5f543defb7ec0
safe_altmetrics <- purrr::safely(altmetrics, otherwise = NULL)
alm <- function(x)  safe_altmetrics(doi = x)
requests <- map(ids, alm) 

results <- requests %>%  
  map("result") %>% 
  compact(.) %>% 
  modify_depth(1, altmetric_data)

data <- bind_rows(results) %>% 
  filter(doi != 'na') %>% 
  select(doi, contains("cited"), contains("readers")) 

data[,2:19] <- sapply(data[,2:19], as.integer)

write.csv(data, "aalto_sust_altm.csv", row.names = F)

# Grouping by altmetrics class, modified from 
# https://www.niso.org/sites/default/files/stories/2017-08/IP_Lin_Fenner_PLOS_altmetrics_isqv25no2.pdf 
#
# Altmetric documentation by field
# https://docs.google.com/spreadsheets/d/1ndVY8Q2LOaZO_P_HDmSQulagjeUrS250mAL2N5V8GvY/edit#gid=0
#
# Classes
# -------
# Downloaded: Pure download count
# Saved: readers_count (Mendeley)
# Discussed: cited_by_* (except policies, patents, and cited_by_account_count. 
#                       Note that contrary to the documentation, cited_by_account is not sum of all cited_*)
# Recommended/Having impact: cited_by_policy_* and cited_by_patents_*

stats <- data %>% 
  mutate(Altmetric_saved = rowSums(select(., readers_count)),
         Altmetric_discussed = rowSums(select(., starts_with("cited_by") & 
                                                !starts_with("cited_by_accounts") &
                                                !starts_with("cited_by_policies") &
                                                !starts_with("cited_by_patents")), na.rm = T),
         Altmetric_impact = rowSums(select(., starts_with("cited_by_patents") | starts_with("cited_by_policies")), na.rm = T)) %>% 
  select(doi, starts_with("Altmetric"))

data_with_altmetrics <- stats %>% 
  left_join(rawdata, ., by=c("DI"="doi")) %>% 
  select(-starts_with("PLUM")) 

data_with_altmetrics %>% 
  relocate(Altmetric_saved, Altmetric_discussed, Altmetric_impact, .after = `Downloads\r\n(Pure)`) %>% 
  write_xlsx(path = "Altmetric_Aalto sustis tagatty Pure 2016-2019.xlsx")




