# Workforce
Workforce data for Female Pelvic Medicine and Reconstructive Surgeons


I am working with Rui and Elena to look at visits with FPMRS physicians.  My role is bringing in the list of physicians who are FPMRS.  

## Urology FPMRS
I was able to find data on Urology board-certified FPMRS at the AUA patient-facing site and the "Is Your Doctor Board Certified?" with ABU:

* [Urology Care Foundation, The Official Foundation of the American Urological Association](https://www.urologyhealth.org/find-a-urologist) - This provides a list of providers and their subspecialty.  The year of certification is listed for their primary board cert in Urology.  
![ABU Image](https://www.dropbox.com/s/4m00ycj9ch73yfw/AUA_find.png?raw=1)

* [ABU List of Board-Certified Urologists with their FPMRS certification](https://www.abu.org/diplomatesearch) - I searched state-by-state to find the data.  In states with over 250 providers we then searched by city and state.  
![ABU Image](https://github.com/mufflyt/Workforce/blob/master/images/ABU.png)
![ABU Image](https://www.dropbox.com/s/6kttw8bvmc5e7yg/ABU_Search.png?raw=1)

* [Wikipedia cities and states](https://en.wikipedia.org/wiki/List_of_largest_cities_of_U.S._states_and_territories_by_population) - The five largest cities in each state were searched from this list.  

We changed the data from long to wide with one physician per row/observation and matched with the NPI number:  
```r
# Set libPaths.
.libPaths("/Users/tylermuffly/.exploratory/R/4.0")

# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(exploratory)

# Steps to produce Batch_4145753_batch_results
`Batch_4145753_batch_results` <- exploratory::read_delim_file("/Users/tylermuffly/Dropbox (Personal)/workforce/Rui_Project/Urology/From_mturk/Batch_4145753_batch_results (completed).csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/New_York", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  select(-HITId, -HITTypeId, -Title, -Description, -Keywords, -Reward, -CreationTime, -MaxAssignments, -RequesterAnnotation, -AssignmentDurationInSeconds, -AutoApprovalDelayInSeconds, -Expiration, -NumberOfSimilarHITs, -LifetimeInSeconds, -AssignmentId, -WorkerId, -AssignmentStatus, -AcceptTime, -SubmitTime, -AutoApprovalTime, -ApprovalTime, -RejectionTime, -RequesterFeedback, -WorkTimeInSeconds, -LifetimeApprovalRate, -Last30DaysApprovalRate, -Last7DaysApprovalRate) %>%
  select(-Answer.Taxonomy, -Answer.comments, -Answer.physician_name, -Approve, -Reject) %>%
  distinct(Input.Name, .keep_all = TRUE) %>%
  drop_na(Answer.NPI) %>%
  filter(Answer.NPI %nin% c("n/a", "no matching records")) %>%
  mutate(Input.city = str_to_title(Input.city))

# Steps to produce city_by_city_urologists
`city_by_city_urologists` <- exploratory::read_excel_file( "/Users/tylermuffly/Dropbox (Personal)/workforce/Rui_Project/Urology/city-by-city-urologists.xlsx", sheet = "Sheet1", na = c('','NA'), skip=0, col_names=TRUE, trim_ws=TRUE, tzone='America/New_York') %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  rename(...1 = `Name:`) %>%
  (function(x){as.data.frame(matrix(x$...1[!is.na(x$...1)], ncol=14, byrow=TRUE))})

# Steps to produce the output
exploratory::select_columns(exploratory::clean_data_frame(exploratory::read_excel_file( "/Users/tylermuffly/Desktop/urology without suffixes.xlsx", sheet = "Sheet1", na = c('','NA'), skip=0, col_names=FALSE, trim_ws=TRUE, tzone='America/Los_Angeles')),"...1") %>%
  readr::type_convert() %>%
  (function(x){as.data.frame(matrix(x$...1[!is.na(x$...1)], ncol=14, byrow=TRUE))}) %>%
  bind_rows(city_by_city_urologists, id_column_name = "ID", current_df_name = "urology_1", force_data_type = TRUE) %>%
  select(V2, V4, V6, V8, V10, V12, V14) %>%
  rename(`Original Certification Year` = V6, Name = V2, Location = V4, `Certification Type` = V8, `SubSpecialty Certification` = V10, `Certificate Expiration Year` = V12, `Perticipation in LLL` = V14) %>%
  filter(`SubSpecialty Certification` == "Female Pelvic Medicine and Reconstructive Surgery") %>%
  arrange(Name) %>%
  filter((is.na(`Certification Type`) | `Certification Type` != "Urology — Retired")) %>%
  filter(`Perticipation in LLL` == "Yes") %>%
  distinct(Name, .keep_all = TRUE) %>%
  drop_na(Name) %>%
  select(-`Certificate Expiration Year`) %>%
  select(-`Perticipation in LLL`) %>%
  separate(Name, into = c("full_name", "suffix"), sep = "\\s*\\,\\s*", remove = FALSE, convert = TRUE) %>%
  mutate(first_name = word(full_name, 1, sep = "\\s+"), last_name = word(full_name, -1, sep = "\\s+")) %>%
  separate(Location, into = c("city", "state"), sep = "\\s*\\,\\s*", remove = FALSE, convert = TRUE) %>%
  mutate(state = statecode(state, output_type="name")) %>%
  mutate_at(vars(suffix, state, `Original Certification Year`, `Certification Type`, `SubSpecialty Certification`), funs(factor)) %>%
  left_join(Batch_4145753_batch_results, by = c("Name" = "Input.Name"), ignorecase=TRUE) %>%
  select(-full_name, -suffix, -Location, -first_name, -last_name, -Input.full_name, -Input.suffix, -Input.Location, -Input.city, -Input.state, -Input.Original_Certification_Year, -Input.Certification_Type, -Input.SubSpecialty_Certification, -Input.first_name, -Input.last_name, -Answer.State) %>%
  rename(NPI = Answer.NPI) %>%
  mutate_at(vars(city, state), funs(str_to_title)) %>%
  mutate_at(vars(everything()), funs(factor)) %>%
  filter(!is.na(NPI)) %>%
  
  # The values were for their urology cert not their FPMRS cert.  So anyone before 2013 I said was grandfathered in during 2013.  
  mutate(`Original Certification Year` = recode(`Original Certification Year`, "2004" = "2013", "2011" = "2013", "2012" = "2013", "2018" = "2018", "2007" = "2013", "1997" = "2013", "2002" = "2013", "2005" = "2013", "2006" = "2013", "1994" = "2013", "2010" = "2013", "1998" = "2013", "2008" = "2013", "1987" = "2013", "1999" = "2013", "1984" = "2013", "2009" = "2013", "2001" = "2013", "2003" = "2013", "1990" = "2013", "1993" = "2013", "2013" = "2013", "2000" = "2013", "1995" = "2013", "1992" = "2013", "1988" = "2013", "1982" = "2013", "1996" = "2013", "1991" = "2013"))
```

