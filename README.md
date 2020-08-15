# Workforce information for Rui/Elena Project
Workforce data for Female Pelvic Medicine and Reconstructive Surgeons.  Please see a walk through video of how the data set was created.  
https://youtu.be/07RiuBlL7UA

## Where did the data come from?
![Flow chart](https://www.dropbox.com/s/paajyi6rkihev3x/flowsheet.jpeg?raw=1)

## Publicly-Available ABMS
* [ABMS Certification Data](https://www.dropbox.com/s/8bdf0z7eyd2yea7/abms-board-certification-report-2018-2019.pdf?raw=1). The ABMS provides a publicly-accessible guide regarding certificates issued for all specialties and subspecialties including FPMRS.  They use information provided annually by ABOG.  Table 3B: New Subspecialty Certificates Issued by ABMS Member Boards 2009–2018.  Look for **both** OBGYN (top) and urology (bottom) numbers:
![Table 3B: New Subspecialty Certificates Issued by ABMS Member Boards 2009–2018, ABMS Image](https://www.dropbox.com/s/100lktrzhj0d99x/abms.png?raw=1)
![Table 3B: New Subspecialty Certificates Issued by ABMS Member Boards 2009–2018, ABMS Image](https://www.dropbox.com/s/6ewvkwi0r4hwvft/abms_urology.png?raw=1)

Table 3C: ABMS Board Certified Physicians by Member Board and State.  Look for **both** OBGYN and urology numbers:
![Table 3C: New Subspecialty Certificates Issued by ABMS Member Boards 2009–2018, ABMS Image](https://www.dropbox.com/s/tof4ohpq3527vg4/ABMS%20by%20state.png?raw=1)
![Table 3C: ABMS Member Boards 2009–2018, ABMS Image](https://www.dropbox.com/s/1xench4g6z6qtel/ABMS%20by%20state%20urology.png?raw=1)

## I started by Searching Googing for the term "Female Pelvic Medicine and Reconstructive Surgery"
This was a top hit and a start of my names to search:  https://www.healthgrades.com/female-pelvic-medicine-and-reconstructive-surgery-directory.  

## I started by Searching Google for Names in the NPI Database
The code searched for physicians who list Female Pelvic Medicine and Reconstructive Surgery in a web site.  This was the start for a list of possible board-certified OBGYNs and Urologists.  I then cross-referenced that list with the NPI database by first and last name.  This was done by a hand search.  
```r
########################################################################################
# Who practices urogynecology in the United States of America?
###  https://medium.com/@curiositybits/automating-the-google-search-for-the-web-presence-of-8000-organizations-54775e9f6097

install.packages("rvest")
install.packages("urltools")
require("rvest")
require("urltools")
library("memoise")

fc <- cache_filesystem(file.path(".cache"))
GET_m <- memoise(httr::GET, cache = fc)
gc()

########################################################################################
###Build list of residents based on NPPES taxonomy codes
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
library(tidyverse)

# NPPES scrape using the API.  So this should be really up to date for the NPPES data.  
NPI_names <- exploratory::read_excel_file( "/Users/tylermuffly/Dropbox (Personal)/workforce/Workforce/NPPES web search hand scrape.xlsx", sheet = "Sheet1", na = c('','NA'), skip=0, col_names=TRUE, trim_ws=TRUE, tzone='America/Denver') %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  filter(!is.na(NPI)) %>%
  select(-...5, -Address) %>%
  rename(Certified = ...3) %>%
  mutate(name = stringr::str_to_title(name)) %>%
  filter(`Taxonomy code` %nin% c("Nurse Practitioner", "Physician Assistant", "Student in an Organized Health Care Education/Training Program")) %>%
  rename(Address = ...4) %>%
  tidyr::separate(Address, into = c("Address_1", "Address_2"), sep = "\\s*\\,\\s*", remove = FALSE, convert = TRUE, extra = "merge") %>%
  tidyr::separate(Address_2, into = c("Address_2_1", "Address_2_2"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  mutate(Address_2_2 = str_remove_all(Address_2_2, "[^a-zA-Z]+")) %>%
  rename(State = Address_2_2, City = Address_2_1, Street = Address_1) %>%
  select(-Certified, -Street, -Phone) %>%
  mutate(first_name = word(name, 1, sep = "\\s+"), last_name = word(name, -1, sep = "\\s+"))
  #distinct(name, .keep_all = TRUE)

#Applicants dataframe is called "d"
d <- NPI_names
colnames(d)
dim(d)

d$Website1 <- "NA"   #creates to empty row in the d dataframe where the website can go.
#d$Website2 <- "NA"   #creates to empty row in the d dataframe where the website can go.

for (i in 1:dim(d)[1]) {       #for (i in 1:10) {  #This was for testing.
  
  print(paste0("Search for the urogyn name and the google url for:",d$name[i]))
  Sys.sleep(60.0) 
  
  url1 = GET_m(utils::URLencode(paste("https://www.google.com/search?q=female+pelvic+medicine+and+reconstructive+surgery",d$name[i])))  
 
  page1 <- xml2::read_html(url1)    #Reads HTML of the Google results page

  nodes <- rvest::html_nodes(page1, "a")   #Finds the HTML node "a" that is the name of the first link.
  links <- rvest::html_attr(nodes,"href")  # extract first link of the Google search results
  
  link <- links[startsWith(links, "/url?q=")]    # clean the link
  
  link <- sub("^/url\\?q\\=(.*?)\\&sa.*$","\\1", link)     # clean the link

 result1 <- as.character(link)
 d$Website1[i] <- result1[1]
}

dim(d)
str(d)
write.csv(d, "~/Desktop/test.csv")
write.csv(d, gsub(":", "-" , paste0("~/Desktop/Google_FPMRS_name_and_term_Doximity (", Sys.time(), ").csv")), row.names = FALSE)

View(d)
```
I reviewed each hit to determine if the person was a FPMRS and if they listed board-certification as was described in the NPI database.  
  
## Publicly-Available Urology and OBGYN board-certified
Data on the web can be scraped based on the "terms of service" and the robots.txt file on the site.  All sites are able to be copied and pasted.  None of the sites have a terms of service therefore we examined the robots.txt files.  ABOG has a vague terms of service but an unrestrictive robots.txt file.  
```r
#Provides functions to download and parse ‘robots.txt’ files. Ultimately the package makes it easy to check if bots (spiders, crawler, scrapers, …) are allowed to access specific resources on a domain.  (e.g. https://wikipedia.org/robots.txt, https://google.com/robots.txt) 

# paths_allowed("http://google.com/")
# ## [1] TRUE
# paths_allowed("http://google.com/search")
# ## [1] FALSE

install.packages("robotstxt")
library(robotstxt)
options(robotstxt_warn = FALSE)
paths_allowed(
  paths  = c("/api/rest_v1/?doc", "/w/"),
  domain = "https://www.urologyhealth.org",
  bot    = "*"
)
#[1] TRUE TRUE

paths_allowed(
  paths  = c("/api/rest_v1/?doc", "/w/"),
  domain = "https://www.abu.org",
  bot    = "*"
)
#[1] TRUE TRUE

paths_allowed(
  domain = "https://en.wikipedia.org/",
  bot    = "*"
)
#[1] TRUE

paths_allowed(
  paths  = c("/api/rest_v1/?doc", "/w/"),
  domain = "https://www.voicesforpfd.org/",
  bot    = "*"
)
#[1] TRUE TRUE

paths_allowed(
  paths  = c("/api/rest_v1/?doc", "/w/"),
  domain = "https://www.medicare.gov/physiciancompare/",
  bot    = "*"
)
#[1] TRUE TRUE
```
I was able to find data on Urology board-certified FPMRS at the AUA patient-facing site and the "Is Your Doctor Board Certified?" with ABU:

* [Urology Care Foundation, The Official Foundation of the American Urological Association](https://www.urologyhealth.org/find-a-urologist) - This provides a list of providers and their subspecialty.  The year of certification is listed for their primary board cert in Urology.  
![ABU Image](https://www.dropbox.com/s/4m00ycj9ch73yfw/AUA_find.png?raw=1)

* [ABU List of Board-Certified Urologists with their FPMRS certification](https://www.abu.org/diplomatesearch) - I searched state-by-state to find the data.  In states with over 250 providers we then searched by city and state.  Data were stored in file: `urology without suffixes.xlsx`.
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

# Publicly-Available National Provider Plan and Enumeration System National Provider Index
* [NPPES NPI Registry Search](https://npiregistry.cms.hhs.gov/registry/?) - I searched for the text string `female pelvic medicine` in the USA for individuals and not offices/hospitals.  The downside of the NPI database is that taxonomy code/subspecialty code is self-described and about half the people described themselves with an FPMRS taxonomy code were not FPMRS (`missing_names_from_npi_filter_27.xlsx`).  This NPI data is available as a downloaded file that is HUGE so it breaks a one core system like R.  There were 982 results of physicians with FPMRS.  [Hand scrape of NPPES NPI online search](https://www.dropbox.com/s/lxz0azg5rakkz73/only_docs_npidata_pfile_20050523-20200510_no_filter.txt?raw=1)

This is the raw output of the NPI data file from NPPES and is updated monthly.  I outputed the data from JMP as a txt file where I filtered.  The taxonomy codes are set by the National Uniform Claim Committee (https://www.dropbox.com/s/2vmbbk6s6hxxiv9/nucc_taxonomy_191.csv?raw=1).The NUCC is updated yearly.  

![NPPES Search Image](https://www.dropbox.com/s/j6p3dtb83tw12aa/NPPES_search.png?raw=1)
![NPPES Search Results Image](https://www.dropbox.com/s/g7axl25cmp5uwne/NPPES%20data%20dissemination%20page.png?raw=1)
![NPPES Search Results Image](https://www.dropbox.com/s/ff6u1464yppm8fh/NPPES_search_results.png?raw=1)
We were able to find FPMRS board certification from the NPI database. 
[Data file of NPPES numbers](https://www.dropbox.com/s/lxz0azg5rakkz73/only_docs_npidata_pfile_20050523-20200510_no_filter.txt?raw=1) - 
only_docs_npidata_pfile_20050523-20200510_no_filter.txt from Muffly.  The code is below:

```r
# Set libPaths.
.libPaths("/Users/tylermuffly/.exploratory/R/4.0")

# Load required packages.
library(keras)
library(humaniformat)
library(e1071)
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

# Steps to produce the output

  # This is the raw output of the NPI data file from NPPES.  I outputed the data from JMP as a txt file where I filtered.  
  exploratory::read_delim_file("/Users/tylermuffly/Dropbox/Nomogram/nomogram/data/NPPES/slimmed_down_nppes_to_docs_only/only_docs_npidata_pfile_20050523-20200510_no_filter.txt" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Denver", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  filter(!is.na(NPI)) %>%
  distinct(NPI, .keep_all = TRUE) %>%
  mutate(letter_last_name = str_sub(`Provider Last Name (Legal Name)`, "1","1"))
```
# Publicly-Available AUGS List of FPMRS Physicians
* [AUGS List of Board-Certified with FPMRS](https://www.voicesforpfd.org/find-a-provider/) - This is a list of board-certified FPMRS from the patient facing site for AUGS and we were able to confirm the NPI data this way as well.  
![Voices web site search](https://www.dropbox.com/s/37xjuzoafvuxomu/voices_for_pfd.png?raw=1)

# Publicly-Available National Physician Compare List of FPMRS Physicians
* [National Physician Compare List of Board-Certified with FPMRS](https://www.medicare.gov/physiciancompare/) - This is a public list of physicians who see Medicare and it lists their board-certification status.  This has the date of medical school graduation as well.  The entire 2017 data is also able to be downloaded at https://data.medicare.gov/data/physician-compare but that is THREE YEARS AGO.  But because the 2017 data is too old we will use an API to download the information.  

Physician Compare Search Page:
![National Physician Compare web site search](https://www.dropbox.com/s/s87vrikk6bibgu8/PhysicianCompare_search.png?raw=1)

Physician Compare results showing board certifications:
![National Physician Compare web site search](https://www.dropbox.com/s/q7b0wusft6l0gw2/Physician_compare_results.png?raw=1)

Physician Compare results showing board certifications:
![National Physician Compare API](https://dev.socrata.com/foundry/data.medicare.gov/mj5m-pzi6).  Please see the code below that I used:  
```r
## https://dev.socrata.com/foundry/data.medicare.gov/mj5m-pzi6

##  How do we get the most up to date information from Physician Compare instead of 2017 data that is free for download?
## Install the required package with:
## install.packages("RSocrata")

library("RSocrata")
library(tidyverse)
library(magrittr)
library(Hmisc)
library(exploratory)

#Excellent trick to test huge files with read.csv , nrows = 10000
PCND1 <- 
  # read.csv("/Volumes/Projects/Pharma_Influence/Data/Physician_Compare/Physician_Compare_National_Downloadable_File.csv") %>%
  read.socrata(
    "https://data.medicare.gov/resource/mj5m-pzi6.json",
    app_token = "**************",
    email     = "*****************",
    password  = "*************"
  ) 

PCND2 <- PCND1 %>%
  distinct(npi, .keep_all = TRUE) %>%
  tidyr::drop_na(npi) %>%
  dplyr::select(npi, lst_nm, frst_nm, mid_nm, gndr, med_sch, grd_yr, pri_spec, sec_spec_1, sec_spec_2, sec_spec_3, sec_spec_4, adr_ln_1, cty, st, zip) %>%
  dplyr::rename(State = st, City = cty, Zip.Code = zip, First.Name = frst_nm, Last.Name = lst_nm, Middle.Name = mid_nm, Primary.specialty = pri_spec, Secondary.specialty.1 = sec_spec_1, Secondary.specialty.2 = sec_spec_2, Secondary.specialty.3 = sec_spec_3, Secondary.specialty.4 = sec_spec_4, Line.1.Street.Address = adr_ln_1, Gender = gndr, Medical.School.Name = med_sch, Graduation.year = grd_yr) %>%
  dplyr::filter(State %nin% c("AP","AE", "AS", "FM", "GU", "MH","MP", "PR","PW","UM","VI", "ZZ")) %>%
  dplyr::filter(Primary.specialty %in% c("GYNECOLOGICAL ONCOLOGY", "OBSTETRICS/GYNECOLOGY") | (Secondary.specialty.1 %in% c("GYNECOLOGICAL ONCOLOGY", "OBSTETRICS/GYNECOLOGY")) | (Secondary.specialty.2 %in% c("GYNECOLOGICAL ONCOLOGY", "OBSTETRICS/GYNECOLOGY")) | (Secondary.specialty.3 %in% c("GYNECOLOGICAL ONCOLOGY", "OBSTETRICS/GYNECOLOGY")) | (Secondary.specialty.4 %in% c("GYNECOLOGICAL ONCOLOGY", "OBSTETRICS/GYNECOLOGY"))) %>%
  #substr(x = Zip.Code, start =1, stop =5) %>%
  dplyr::arrange (Last.Name) %>%
  #tolower(c(Last.Name, First.Name, Middle.Name, Line.1.Street.Address, City, State)) %>%
  distinct(npi, .keep_all = TRUE) %>%
  dplyr::mutate_at(vars(Last.Name, First.Name, Middle.Name, Line.1.Street.Address), funs(str_to_title)) %>%
  dplyr::mutate(Middle.Name = exploratory::impute_na(Middle.Name, type = "value", val = "")) %>%
  dplyr::mutate_at(vars(Last.Name, First.Name, Middle.Name), funs(str_clean)) %>%
  tidyr::unite(full.name.1, First.Name, Middle.Name, Last.Name, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  tidyr::unite(full.name.2, First.Name, Middle.Name, Last.Name, State, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  tidyr::unite(full.name.3, Line.1.Street.Address, State, Last.Name, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  dplyr::mutate(Line.1.Street.Address = str_to_title(Line.1.Street.Address)) %>%
  tidyr::unite(full.name.5, Line.1.Street.Address, State, Last.Name, sep = " ", remove = FALSE, na.rm = FALSE) %>%
  readr::write_csv("Physician_Compare_National_Downloadable_File2.csv")  
```
# AMA Physician Masterfile
* [AMA Physician Masterfile](https://www.dropbox.com/s/0sbkw7i49rddwxc/2020%20FPMRS%20Redi-Data%20Quote%20state%20by%20state%20and%20total%20numbers.xls?raw=1) - Current count from AMA is 1,245 physicians.  

STATE	
  ### AK	ALASKA	 1 
  ### AL	ALABAMA	 20 
  ### AR	ARKANSAS	 5 
	AZ	ARIZONA	 22 
	CA	CALIFORNIA	 172 
	CO	COLORADO	 25 
	CT	CONNECTICUT	 19 
	DC	DISTRICT OF COLUMBIA	 6 
	FL	FLORIDA	 84 
	GA	GEORGIA	 35 
	HI	HAWAII	 6 
	IA	IOWA	 10 
	ID	IDAHO	 3 
	IL	ILLINOIS	 44 
	IN	INDIANA	 19 
	KS	KANSAS	 7 
	KY	KENTUCKY	 11 
	LA	LOUISIANA	 15 
	MA	MASSACHUSETTS	 36 
	MD	MARYLAND	 33 
	ME	MAINE	 6 
	MI	MICHIGAN	 39 
	MN	MINNESOTA	 20 
	MO	MISSOURI	 26 
	MS	MISSISSIPPI	 8 
	MT	MONTANA	 1 
	NC	NORTH CAROLINA	 39 
	NE	NEBRASKA	 9 
	NH	NEW HAMPSHIRE	 8 
	NJ	NEW JERSEY	 43 
	NM	NEW MEXICO	 8 
	NV	NEVADA	 11 
	NY	NEW YORK	 85 
	OH	OHIO	 50 
	OK	OKLAHOMA	 10 
	OR	OREGON	 19 
	PA	PENNSYLVANIA	 45 
	RI	RHODE ISLAND	 9 
	SC	SOUTH CAROLINA	 11 
	SD	SOUTH DAKOTA	 3 
	TN	TENNESSEE	 30 
	TX	TEXAS	 85 
	UT	UTAH	 4 
	VA	VIRGINIA	 38 
	VT	VERMONT	 4 
	WA	WASHINGTON	 33 
	WI	WISCONSIN	 22 
	WV	WEST VIRGINIA	 6 
	DE	DELAWARE	 -   
	ND	NORTH DAKOTA	 -   
	WY	WYOMING	 -   

ABMSSUBCERTIFICATION1	5144	Female Pelvic Medicine and Reconstructive Surgery 	 956 
	5145	Female Pelvic Medicine and Reconstructive Surgery 	 284 
ABMSSUBCERTIFICATION2	5144	Female Pelvic Medicine and Reconstructive Surgery 	 5 
	5145	Female Pelvic Medicine and Reconstructive Surgery 	 -   
ABMSSUBCERTIFICATION3	5144	Female Pelvic Medicine and Reconstructive Surgery 	 -   
	5145	Female Pelvic Medicine and Reconstructive Surgery 	 -   
ABMSSUBCERTIFICATION4	5144	Female Pelvic Medicine and Reconstructive Surgery 	 -   
	5145	Female Pelvic Medicine and Reconstructive Surgery 	 -   


### The final data set is available for the journal to review.  
https://www.dropbox.com/s/ds4cogzjst9hhr9/all_a_dataframes_distinct_48.csv?raw=1


# Accessory Code I used:
* Hand searching remains the best for matching names to NPI numbers.  I tried the `RecordLinkage` package as well.  
```r
##https://rpubs.com/ahmademad/RecordLinkage
# Set libPaths.
.libPaths("/Users/tylermuffly/.exploratory/R/4.0")

# Load required packages.
if(!require(pacman))install.packages("pacman")
pacman::p_load('janitor', 'lubridate', 'hms', 'tidyr', 'devtools', 'purrr', 'readr', 'ggplot2', 'dplyr', 'forcats', 'RcppRoll', 'lubridate', 'hms', 'tidyr', 'stringr', "bit64", "remotes", "tidylog","inspectdf", "DataExplorer", "arsenal", "RCurl", "RSQLite", "DBI", "sqldf", "qdapRegex", "dplyr", "dbplyr", "RPostgreSQL", "data.table")
set.seed(123456)

# Install github packages
p_install_gh("cran/doMC")
library("doMC")
registerDoMC(cores = detectCores()-1)

library(RecordLinkage)
library(readr)
npi <- read_csv("Downloads/filtered_down_to_obgyns_filter_14.csv")
#Physicians_total_arrange_39 <- read_csv("Downloads/Residents_Only_reorder_cols_59.csv")

Physicians_total_arrange_39 <- read_csv("Downloads/Physicians_total_arrange_39a.csv")

npi <- npi[,c("Provider First Name", "Provider Last Name (Legal Name)", "Provider Business Mailing Address City Name", "Provider Business Mailing Address State Name", "Provider Credential Text")]
Physicians_total_arrange_39 <- Physicians_total_arrange_39[, c("firstname", "Last_name", "city", "state", "suffix")]

names(Physicians_total_arrange_39) <- names(npi)  

npi[,-1] <- as.data.frame(sapply(npi[,-1], toupper))
Physicians_total_arrange_39[,-1] <- as.data.frame(sapply(Physicians_total_arrange_39[,-1], toupper))

npi[,-1] <- as.data.frame(sapply(npi[,-1], function(x) gsub("[[:punct:]]", "", x)))
Physicians_total_arrange_39[,-1] <- as.data.frame(sapply(Physicians_total_arrange_39 [,-1], function(x) gsub("[[:punct:]]", "", x)))
npi[,-1] <- as.data.frame(sapply(npi[,-1], function(x) gsub(" ", "", x)))

npi[,-1] <- as.data.frame(sapply(npi[,-1], function(x) gsub(" ", "", x)))
Physicians_total_arrange_39[,-1] <- as.data.frame(sapply(Physicians_total_arrange_39[,-1], function(x) gsub(" ", "", x)))


npi$flast <- paste(substring(npi$'Provider First Name',1,1), npi$'Provider Last Name (Legal Name)', sep = '')
Physicians_total_arrange_39$flast <- paste(substring(Physicians_total_arrange_39$'Provider First Name',1,1), Physicians_total_arrange_39$'Provider Last Name (Legal Name)', sep = '')

a <- compare.linkage(npi, Physicians_total_arrange_39, 
                     blockfld = c("Provider Credential Text", 
                                  "Provider Last Name (Legal Name)", 
                                  "Provider Business Mailing Address State Name"),  #Add gender
                     strcmp = T, 
                     exclude=c(1))
print(head(a$pairs))
a

a <- epiWeights(a)
result <- epiClassify(a, 0.7)

b <- emWeights(a, cutoff = 0.8)
summary(b)

allPairs <- getPairs(b)
head(allPairs)

finalPairs <- getPairs(b, max.weight = 14, min.weight = 0, single.rows=TRUE, show = "all")
head(finalPairs)

write.csv(finalPairs, "~/Dropbox/workforce/finalPairs.csv")
```

I also tried to access the NPI application programming information (API) and it was painfully slow.  The code is listed below:
```r
#Searches names in the NPPES API.   


#use library so R can make an API call
require(httr)

#create function trim that removes white spaces before and after value. One of the standard procedures in data preparation
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# optional. set working directory as Desktop where the output file will be saved. for Mac
setwd("~/Dropbox/workforce//")

# provide the full path or URL to the input file
#input_file_path = "https://www.dropbox.com/s/bdxfcw0iq9etp77/Physicians_total_left_join_27.csv?raw=1"
input_file_path = "https://www.dropbox.com/s/3myst0596aqn96e/Physicians_total_drop_na_29.csv?raw=1"

# if you provide the full path to file, no need to set working directory above
output <- "output.csv"

# read the input CSV file into R's data frame object
input = read.csv(input_file_path) #This is a list of all OBGYNs
dim(input)

# ONLY FOR QUICK TESTING. ONLY TAKES FIRST 100 ROWS FOR QUICK RESULT. 
# YOU CAN DELETE THIS LINE SO THE SCRIPT WILL CHECK ENTIRE INPUT
input<- head(input, 1000000L)

# Loop through each row (from 1 to the total number of rows) in the dataframe with users
for(i in 1:nrow(input)){
  
  # grab firstname, lastname, state from each row/person
  # write grabbed values into variables to be passed onto API call
  first_name <- input$firstname[i]
  middle_name <- input$middlename[i]
  last_name <- input$lastname[i]
  state <- input$state[i]
  
  # make API call for each row by dynamically populating API's querry with firstname, lastname, state for each row/person
  api_call <-  GET("https://npiregistry.cms.hhs.gov/api/", query = list(enumeration_type = "NPI-1", first_name=first_name, last_name=last_name,state=state, version=2.1))
  # convert API call resulting object into R's list object
  api_content <-  content(api_call)
  # grab the value of the total return result per search and write into variable
  api_content_count <-  api_content$result_count
  
  
  # if API call returns more than 0 results, add columns to the output:
  if(api_content_count>0){
    # reset the iteratorso we don't go out of bounds when selecting elements later 
    n = 1  
    # if we get more than 1 result per search
    if (api_content_count > 1){
      #loop though all results per search
      for (j in 1:length(api_content$results)){
        # if middle name returned in any result per search AND it is equal to the middle name in the input, PULL the NPI info for that user with matching middlename, otherwise pick the first result
        # functions around are just for more precise results like making sure to handle NA's, case sensitivity, no white spaces
        if(!is.null(api_content$results[[j]]$basic$middle_name) && trim(tolower(api_content$results[[j]]$basic$middle_name)) == trim(tolower(ifelse(is.na(middle_name), '', as.character(middle_name))))){
          # assigns the position of the middlename match to n
          n = j
          # optional, sets MatchCount to 1 since now the search is narrowed down
          api_content_count <- 1
          #breaking the search for matching middlename loop if successfully found the match so the n selector points to correct value
          break
        }
        else{
          # if not middlename searches matched, set the default selector value to 1 to pick the first API result for search parameters
          n = 1
        }
        #cat(j, trim(tolower(api_content$results[[j]]$basic$middle_name)), "\n")
      }
    }
    
    # RESULT_NPI column with NPI number from API for that row/person
    input$RESULT_NPI[i] <- api_content$results[[n]]$number
    # RESULT_Address1 column with Address1 value from API for that row/person
    input$RESULT_Address1[i] <- api_content$results[[n]]$addresses[[1]]$address_1
    # RESULT_City column with City value from API for that row/person
    input$RESULT_City[i] <- api_content$results[[n]]$addresses[[1]]$city
    # RESULT_IsPresent column with TRUE/FALSE flag where input row/user has been found in API
    input$RESULT_IsPresent[i] <- 'True'
    # IMPORTANT: RESULT_MatchCount column shows how many people were found in NPI with the SAME firsname, lastname living in the same state
    input$RESULT_MatchCount[i] <- api_content_count
    
    # Adding all possible result columns from API
    input$RESULT_enumeration_type[i] <- api_content$results[[n]]$enumeration_type
    input$RESULT_last_updated_epoch[i] <- api_content$results[[n]]$last_updated_epoch
    input$RESULT_created_epoch[i] <- api_content$results[[n]]$created_epoch
    input$RESULT_first_name[i] <- api_content$results[[n]]$basic$first_name
    input$RESULT_last_name[i] <- api_content$results[[n]]$basic$last_name
    input$RESULT_sole_proprietor[i] <- api_content$results[[n]]$basic$sole_proprietor
    input$RESULT_gender[i] <- api_content$results[[n]]$basic$gender
    input$RESULT_enumeration_date[i] <- api_content$results[[n]]$basic$enumeration_date
    input$RESULT_last_updated[i] <- api_content$results[[n]]$basic$last_updated
    input$RESULT_status[i] <- api_content$results[[n]]$basic$status
    input$RESULT_name[i] <- api_content$results[[n]]$basic$name
    input$RESULT_country_code[i] <- api_content$results[[n]]$addresses[[1]]$country_code
    input$RESULT_country_name[i] <- api_content$results[[n]]$addresses[[1]]$country_name
    input$RESULT_address_purpose[i] <- api_content$results[[n]]$addresses[[1]]$address_purpose
    input$RESULT_address_type[i] <- api_content$results[[n]]$addresses[[1]]$address_type
    input$RESULT_address_2[i] <- api_content$results[[n]]$addresses[[1]]$address_2
    input$RESULT_state[i] <- api_content$results[[n]]$addresses[[1]]$state
    input$RESULT_postal_code[i] <- api_content$results[[n]]$addresses[[1]]$postal_code
    input$RESULT_telephone_number[i] <- api_content$results[[n]]$addresses[[1]]$telephone_number
    input$RESULT_code[i] <- api_content$results[[n]]$taxonomies[[1]]$code
    input$RESULT_desc[i] <- api_content$results[[n]]$taxonomies[[1]]$desc
    input$RESULT_primary[i] <- api_content$results[[n]]$taxonomies[[1]]$primary
    input$RESULT_taxonomystate[i] <- api_content$results[[n]]$taxonomies[[1]]$state
    input$RESULT_license[i] <- api_content$results[[n]]$taxonomies[[1]]$license
    
    
    
  }
  # if API call returns 0 results, add columns to the output with respective values instead of above
  else{
    input$RESULT_NPI[i] = 'NA'
    input$RESULT_Address1[i] <- 'NA'
    input$RESULT_City[i] <- 'NA'
    input$RESULT_IsPresent[i] <- 'False'
    input$RESULT_MatchCount[i] <- api_content_count
    
    input$RESULT_enumeration_type[i] <- 'NA'
    input$RESULT_last_updated_epoch[i] <- 'NA'
    input$RESULT_created_epoch[i] <- 'NA'
    input$RESULT_first_name[i] <- 'NA'
    input$RESULT_last_name[i] <- 'NA'
    input$RESULT_sole_proprietor[i] <- 'NA'
    input$RESULT_gender[i] <- 'NA'
    input$RESULT_enumeration_date[i] <- 'NA'
    input$RESULT_last_updated[i] <- 'NA'
    input$RESULT_status[i] <- 'NA'
    input$RESULT_name[i] <- 'NA'
    input$RESULT_country_code[i] <- 'NA'
    input$RESULT_country_name[i] <- 'NA'
    input$RESULT_address_purpose[i] <- 'NA'
    input$RESULT_address_type[i] <- 'NA'
    input$RESULT_address_2[i] <- 'NA'
    input$RESULT_state[i] <- 'NA'
    input$RESULT_postal_code[i] <- 'NA'
    input$RESULT_telephone_number[i] <- 'NA'
    input$RESULT_code[i] <- 'NA'
    input$RESULT_desc[i] <- 'NA'
    input$RESULT_primary[i] <- 'NA'
    input$RESULT_taxonomystate[i] <- 'NA'
    input$RESULT_license[i] <- 'NA'
  }
  
  # optional line of code. Shows progress. Countdown of rows
  cat("\r", " remaining: ", nrow(input) - i, "\r")
}

# write output into output.csv file stored on the desktop
write.csv(input, output, row.names=FALSE)

# optional. Show output in R window. Only works if run from R Studio
View(input)
```
