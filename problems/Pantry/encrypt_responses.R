#
# Anonymize survey responses.
#

library(dplyr)
library(assert)

filename = "Weekly Grocery Bag Request Survey_November 8, 2020_12.47.xlsx"
filepath = "problems/Community-Pantry"
data_raw = readxl::read_xlsx(file.path(filepath, filename))

fields = c(date = "RecordedDate",
          name = "Q2",
          email = "Q3",
          phone = "Q4",
          how = "Q5",
          n_household = "Q9",
          babybag = "Q10",
          degree = "Q30",
          school = "Q31",
          year = "Q32",
          n_children = "Q34")

md5_key = as.character(openssl::rand_num())
data = data_raw %>%
  select(!!!fields) %>%
  mutate(date = openxlsx::convertToDate(date)) %>%
  filter(date >= "2020-06-24", date <= "2020-11-04") %>%
  mutate(name = openssl::md5(tolower(name), key=md5_key),
         email = openssl::md5(tolower(email), key=md5_key),
         phone = openssl::md5(gsub("[^0-9.-]", "", phone), key=md5_key))

saveRDS(data, file=file.path(filepath, "order_data.rds"))


