# assembling THCIC COPD data for both outpatient and inpatient THCIC data

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)



#### outpatient data for COPD (J44): 2018-2020 ###############################################################
#this follows 'OP THCIC clean up 10-2023.R'
#apparently the data were delivered in several different files, so they need merging
setwd("Z:/THCIC/Outpatient THCIC data 2018-2020")


################# 2018 data
# "base" includes the main variables
op2018full <- read_tsv("OP_2018_base.txt")%>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44.")) #doing the filtering at this step to save some memory

# "thcic_ID" includes the hospital THCIC ID 
op2018hospitalID <- read_tsv("op_2018_THCIC_ID.txt")

# "enc" includes the date of encounter variable STMT_PERIOD_FROM
op2018enc <- read_tsv("op_2018_enc.txt")

# linking THCIC hospital ID and data (merge the variables you want/need)
op2018 <- left_join(op2018full, op2018hospitalID, by = "RECORD_ID") %>% 
          left_join(., op2018enc) %>% 
  mutate(Year = 2018) %>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44."))

################# 2019 data
# "base" includes the main variables
op2019full <- read_tsv("OP_2019_base.txt")%>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44.")) #doing the filtering at this step to save some memory

# "thcic_ID" includes the hospital THCIC ID 
op2019hospitalID <- read_tsv("op_2019_THCIC_ID.txt")

# "enc" includes the date of encounter variable STMT_PERIOD_FROM
op2019enc <- read_tsv("op_2019_enc.txt")

# linking THCIC hospital ID and data (merge the variables you want/need)
op2019 <- left_join(op2019full, op2019hospitalID, by = "RECORD_ID") %>% 
  left_join(., op2019enc) %>% 
  mutate(Year = 2019) %>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44."))


################# 2020 data
# "base" includes the main variables
op2020full <- read_tsv("OP_2020_base.txt")%>% 
     filter(str_detect(PRINC_DIAG_CODE, "^J44.")) #doing the filtering at this step to save some memory

# "thcic_ID" includes the hospital THCIC ID 
op2020hospitalID <- read_tsv("op_2020_THCIC_ID.txt")

# "enc" includes the date of encounter variable STMT_PERIOD_FROM
op2020enc <- read_tsv("op_2020_enc.txt")

# linking THCIC hospital ID and data (merge the variables you want/need)
op2020 <- left_join(op2020full, op2020hospitalID, by = "RECORD_ID") %>% 
  left_join(., op2020enc) %>% 
  mutate(Year = 2020) %>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44."))


# op2018_full <- read_csv("Z:/THCIC/Outpatient THCIC data 2018-2020/Filtered Data/op2018_full.csv", guess_max = 10000) %>% 
#   filter(., str_detect(PRINC_DIAG_CODE, "^J44.")) #doing the filtering at this step to save some memory
# op2019_full <- read_csv("Z:/THCIC/Outpatient THCIC data 2018-2020/Filtered Data/op2019_full.csv", guess_max = 10000) %>% 
#   filter(., str_detect(PRINC_DIAG_CODE, "^J44.")) 
# op2020_full <- read_csv("Z:/THCIC/Outpatient THCIC data 2018-2020/Filtered Data/op2020_full.csv", guess_max = 10000) %>% 
#   filter(., str_detect(PRINC_DIAG_CODE, "^J44.")) 
# 
# op2018enc <- read_tsv("Z:/THCIC/Outpatient THCIC data 2018-2020/op_2018_enc.txt")
# op2019enc <- read_tsv("Z:/THCIC/Outpatient THCIC data 2018-2020/op_2019_enc.txt")
# op2020enc <- read_tsv("Z:/THCIC/Outpatient THCIC data 2018-2020/op_2020_enc.txt")

### combine the dfs from each year
dfs_to_keep <- c("op2018", "op2019", "op2020")
rm(list=setdiff(ls(), dfs_to_keep))
gc()

op2018 <- op2018 %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION),
                            RACE = as.character(RACE))
op2019 <- op2019 %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION),
                            RACE = as.character(RACE))
op2020 <- op2020 %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION),
                            RACE = as.character(RACE))

op_data_2018_2020 <- bind_rows(op2018, op2019, op2020) #test <- sample_n(op_data_2018_2020, 100)


## Cleaning up some errors in the THCIC data: using a script from RAZ for this
# there are some errors, there should only be 0-5 options, see page 7 of RDF_OP.pdf for references
op_data_2018_2020$RACE <- factor(ifelse(op_data_2018_2020$RACE%in% c(6,7,8,9,"O"), 0, op_data_2018_2020$RACE))
op_data_2018_2020$RACE <- droplevels(op_data_2018_2020$RACE) #drop unused levels then relabel them
op_data_2018_2020$RACE <- factor(op_data_2018_2020$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))

# there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
op_data_2018_2020$ETHNICITY <- factor(ifelse(op_data_2018_2020$ETHNICITY%in% c(3,4,5), 0, op_data_2018_2020$ETHNICITY))
op_data_2018_2020$ETHNICITY <- droplevels(op_data_2018_2020$ETHNICITY) #drop unused levels then relabel them
op_data_2018_2020$ETHNICITY <- factor(op_data_2018_2020$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))


table(op_data_2018_2020$RACE, op_data_2018_2020$ETHNICITY)
# there are 1898 cases that are Black and Latinx

# creating a new variable that combines race and ethnicity
# if a person is Black and Latinx, they are categorized as Black
# if a person is Latinx, they could be any race EXCEPT Black
op_data_2018_2020$RACE_ETHNICITY[op_data_2018_2020$ETHNICITY == "Non-Latinx" &  op_data_2018_2020$RACE == "White"] <- "White"
op_data_2018_2020$RACE_ETHNICITY[op_data_2018_2020$ETHNICITY == "Latinx"] <- "Latinx"
op_data_2018_2020$RACE_ETHNICITY[op_data_2018_2020$RACE == "Black"] <- "Black"

#fixing census block input errors -- removing periods
op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- str_replace_all(op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, "[^[:alnum:]]", "")

#filling in any missing zeros at the end (numbers should be at least 11 digits long -- the 12th digit is an added level of specificity we can't use with census data)
op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- stringi::stri_pad_right(op_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, 11, 0)

#save 
write_csv(op_data_2018_2020, "Z:/THCIC/Outpatient THCIC data 2018-2020/OPTexasCOPD18_20_enc_date.csv")
#op_data_2018_2020 <- read_csv("Z:/THCIC/Outpatient THCIC data 2018-2020/OPTexasCOPD18_20_enc_date.csv")





### outpatient data for COPD (J44): 2015 q4 - 2017 data ##################################################################################
#we want the 'base' files; 'charges' contain revenue codes

setwd("Z:/THCIC/Outpatient THCIC data 2015Q4-2017/OP Decrypted")
yearly_files <- c("OP4q2015_Base.txt", "OP2016_Base.txt", "OP2017_Base.txt")

icd10_codes <- "J44"  #paste(c("J44")) #COPD - not bothering to exclude any of them  collapse = "|") 

for(i in 1:length(yearly_files)){
  file_i <- read_tsv(yearly_files[i], #n_max = 10000,
                     col_types = cols(
                       PROVIDER_ZIP = col_character(),
                       ETHNICITY = col_double(),
                       RACE = col_double(),
                       PAT_AGE_GROUP = col_double()),
                     guess_max = 10000)
  
  file_i$PRINC_DIAG_CODE_first <- substr(file_i$PRINC_DIAG_CODE, 1, 3)
  file_i_filt <- file_i %>% filter(str_detect(PRINC_DIAG_CODE_first, icd10_codes)) 
  if(i == 1) {file_all <- file_i_filt}
  if(i > 1){file_all <- bind_rows(file_all, file_i_filt)}
}

op_data_2015_2017 <- file_all 

#------- Cleaning up some errors in the THCIC data ---------------#
# there are some errors, there should only be 0-5 options, see page 7 of RDF_OP.pdf for references
op_data_2015_2017$RACE <- factor(ifelse(op_data_2015_2017$RACE%in% c(6,7,8,9,"O"), 0, op_data_2015_2017$RACE))
op_data_2015_2017$RACE <- droplevels(op_data_2015_2017$RACE) #drop unused levels then relabel them
op_data_2015_2017$RACE <- factor(op_data_2015_2017$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))

# there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
op_data_2015_2017$ETHNICITY <- factor(ifelse(op_data_2015_2017$ETHNICITY%in% c(3,4,5,8), 0, op_data_2015_2017$ETHNICITY))
op_data_2015_2017$ETHNICITY <- droplevels(op_data_2015_2017$ETHNICITY) #drop unused levels then relabel them
op_data_2015_2017$ETHNICITY <- factor(op_data_2015_2017$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))


table(op_data_2015_2017$RACE, op_data_2015_2017$ETHNICITY)
# there are 884 cases that are Black and Latinx

# creating a new variable that combines race and ethnicity
# if a person is Black and Latinx, they are categorized as Black
# if a person is Latinx, they could be any race EXCEPT Black
op_data_2015_2017$RACE_ETHNICITY[op_data_2015_2017$ETHNICITY == "Non-Latinx" &  op_data_2015_2017$RACE == "White"] <- "White"
op_data_2015_2017$RACE_ETHNICITY[op_data_2015_2017$ETHNICITY == "Latinx"] <- "Latinx"
op_data_2015_2017$RACE_ETHNICITY[op_data_2015_2017$RACE == "Black"] <- "Black"
write.csv(op_data_2015_2017, "Z:/THCIC/Katz/op_copd_2015q4_2017.csv", row.names = FALSE)
#op_data_2015_2017 <- read_csv("Z:/THCIC/Katz/op_copd_2015q4_2017.csv")


### outpatient: combining the outpatient 2015 q4 - 2017 data with the 2018-2020 data #################################################################################
str(op_data_2015_2017)
str(op_data_2018_2020)

op_data_2015_2017 <- op_data_2015_2017 %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION))
op_data_2018_2020 <- op_data_2018_2020 %>% mutate(PAT_ADDR_CENSUS_BLOCK_GROUP= as.character(PAT_ADDR_CENSUS_BLOCK_GROUP),
                                                            PAT_AGE_GROUP = as.numeric(PAT_AGE_GROUP))

op_copd_2015q4_2020 <- bind_rows(op_data_2018_2020, op_data_2015_2017)
write_csv(op_copd_2015q4_2020, "Z:/THCIC/Katz/op_copd_2015q4_2020.csv")







#### inpatient data for COPD (J44): 2018-2020 ###############################################################
#this data appears to have been created in 'OP THCIC clean up 10-2023.R'
#apparently the files were delivered in several different files, so I'm picking it up at the point where only date still needs to be added

ip2018_full <- read_csv("Z:/THCIC/Inpatient THCIC data 2018-2020/ip2018_full.csv", guess_max = 10000) %>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44.")) #doing the filtering at this step to save some memory
ip2019_full <- read_csv("Z:/THCIC/Inpatient THCIC data 2018-2020/ip2019_full.csv", guess_max = 10000) %>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44.")) 
ip2020_full <- read_csv("Z:/THCIC/Inpatient THCIC data 2018-2020/ip2020_full.csv", guess_max = 10000) %>% 
  filter(str_detect(PRINC_DIAG_CODE, "^J44.")) 

ip2018enc <- read_tsv("Z:/THCIC/Inpatient THCIC data 2018-2020/ip_2018_enc.txt")
ip2019enc <- read_tsv("Z:/THCIC/Inpatient THCIC data 2018-2020/ip_2019_enc.txt")
ip2020enc <- read_tsv("Z:/THCIC/Inpatient THCIC data 2018-2020/ip_2020_enc.txt")

ip2018 <- left_join(ip2018_full, ip2018enc) %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION))
ip2019 <- left_join(ip2019_full, ip2019enc) %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION))
ip2020 <- left_join(ip2020_full, ip2020enc) %>% mutate(SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION))

ip_data_2018_2020 <- bind_rows(ip2018, ip2019, ip2020) #test <- sample_n(ip_data_2018_2020, 100)

## Cleaning up some errors in the THCIC data: using a script from RAZ for this
# there are some errors, there should only be 0-5 options, see page 7 of RDF_OP.pdf for references
ip_data_2018_2020$RACE <- factor(ifelse(ip_data_2018_2020$RACE%in% c(6,7,8,9,"O"), 0, ip_data_2018_2020$RACE))
ip_data_2018_2020$RACE <- droplevels(ip_data_2018_2020$RACE) #drop unused levels then relabel them
ip_data_2018_2020$RACE <- factor(ip_data_2018_2020$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))

# there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
summary(ip_data_2018_2020$ETHNICITY)
ip_data_2018_2020$ETHNICITY <- factor(ifelse(ip_data_2018_2020$ETHNICITY%in% c(3,4,5), 0, ip_data_2018_2020$ETHNICITY))
ip_data_2018_2020$ETHNICITY <- droplevels(ip_data_2018_2020$ETHNICITY) #drop unused levels then relabel them
ip_data_2018_2020$ETHNICITY <- factor(ip_data_2018_2020$ETHNICITY, labels = c("Latinx", "Non-Latinx"))


table(ip_data_2018_2020$RACE, ip_data_2018_2020$ETHNICITY)
# there are 1008 cases that are Black and Latinx

# creating a new variable that combines race and ethnicity
# if a person is Black and Latinx, they are categorized as Black
# if a person is Latinx, they could be any race EXCEPT Black
ip_data_2018_2020$RACE_ETHNICITY[ip_data_2018_2020$ETHNICITY == "Non-Latinx" &  ip_data_2018_2020$RACE == "White"] <- "White"
ip_data_2018_2020$RACE_ETHNICITY[ip_data_2018_2020$ETHNICITY == "Latinx"] <- "Latinx"
ip_data_2018_2020$RACE_ETHNICITY[ip_data_2018_2020$RACE == "Black"] <- "Black"

#fixing census block input errors -- removing periods
ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- str_replace_all(ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, "[^[:alnum:]]", "")

#filling in any missing zeros at the end (numbers should be at least 11 digits long -- the 12th digit is an added level of specificity we can't use with census data)
ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP <- stringi::stri_pad_right(ip_data_2018_2020$PAT_ADDR_CENSUS_BLOCK_GROUP, 11, 0)


#save 
write_csv(ip_data_2018_2020, "Z:/THCIC/Inpatient THCIC data 2018-2020/ipTexasCOPD18_20_enc_date.csv")
#ip_data_2018_2020 <- read_csv("Z:/THCIC/Inpatient THCIC data 2018-2020/ipTexasCOPD18_20_enc_date.csv")



### inpatient:  extracting copd records from the 2015 q4 - 2017 data ##################################################################################
#we want the 'base' files; 'charges' contain revenue codes

setwd("Z:/THCIC/Inpatient THCIC data 2004-2017/raw data")
yearly_files <- c("IP_2015Q4_base.txt", "IP_2016_base.txt", "IP_2017_base.txt")

icd10_codes <- "J44"  #paste(c("J44")) #COPD - not bothering to exclude any of them  collapse = "|") 

for(i in 1:length(yearly_files)){
  file_i <- read_tsv(yearly_files[i], #n_max = 10000 #test <- read_tsv(yearly_files[1]); names(test)
                     col_types = cols(
                       #PROVIDER_ZIP = col_character(),
                       ETHNICITY = col_double(),
                       RACE = col_double(),
                       PAT_AGE_GROUP = col_double()),
                     guess_max = 10000)
  
  file_i$PRINC_DIAG_CODE_first <- substr(file_i$PRINC_DIAG_CODE, 1, 3)
  file_i_filt <- file_i %>% filter(str_detect(PRINC_DIAG_CODE_first, icd10_codes)) 
  if(i == 1) {file_all <- file_i_filt}
  if(i > 1){file_all <- bind_rows(file_all, file_i_filt)}
}

ip_data_2015_2017 <- file_all 

#------- Cleaning up some errors in the THCIC data ---------------#
# there are some errors, there should only be 1-5 options, see page 9 of RDF_IP.pdf for references
ip_data_2015_2017$RACE <- factor(ifelse(ip_data_2015_2017$RACE%in% c(6,7,8,9,"O", NA), 0, ip_data_2015_2017$RACE)) #unique(ip_data_2015_2017$RACE)
ip_data_2015_2017$RACE <- droplevels(ip_data_2015_2017$RACE) #drop unused levels then relabel them
ip_data_2015_2017$RACE <- factor(ip_data_2015_2017$RACE, labels = c("Missing", "American Indian", "Asian", "Black", "White", "Other"))

# there are some errors, there should only be 0-3 options, see page 8 of RDF_OP.pdf for references
ip_data_2015_2017$ETHNICITY <- factor(ifelse(ip_data_2015_2017$ETHNICITY%in% c(3,4,5,6,8, NA), 0, ip_data_2015_2017$ETHNICITY)) #unique(ip_data_2015_2017$ETHNICITY)
ip_data_2015_2017$ETHNICITY <- droplevels(ip_data_2015_2017$ETHNICITY) #drop unused levels then relabel them
ip_data_2015_2017$ETHNICITY <- factor(ip_data_2015_2017$ETHNICITY, labels = c("Missing", "Latinx", "Non-Latinx"))


table(ip_data_2015_2017$RACE, ip_data_2015_2017$ETHNICITY)
# there are 547 cases that are Black and Latinx

# creating a new variable that combines race and ethnicity
# if a person is Black and Latinx, they are categorized as Black
# if a person is Latinx, they could be any race EXCEPT Black
ip_data_2015_2017$RACE_ETHNICITY[ip_data_2015_2017$ETHNICITY == "Non-Latinx" &  ip_data_2015_2017$RACE == "White"] <- "White"
ip_data_2015_2017$RACE_ETHNICITY[ip_data_2015_2017$ETHNICITY == "Latinx"] <- "Latinx"
ip_data_2015_2017$RACE_ETHNICITY[ip_data_2015_2017$RACE == "Black"] <- "Black"
write.csv(ip_data_2015_2017, "Z:/THCIC/Katz/ip_copd_2015q4_2017.csv", row.names = FALSE)
#ip_data_2015_2017 <- read_csv("Z:/THCIC/Katz/ip_copd_2015q4_2017.csv")

### inpatient: combining the inpatient 2015 q4 - 2017 data with the 2018-2020 data #################################################################################
str(ip_data_2015_2017)
str(ip_data_2018_2020)

ip_data_2015_2017 <- ip_data_2015_2017 %>% mutate(SOURCE_OF_ADMISSION= as.character(SOURCE_OF_ADMISSION))
ip_data_2018_2020 <- ip_data_2018_2020 %>% mutate(PAT_ADDR_CENSUS_BLOCK_GROUP= as.character(PAT_ADDR_CENSUS_BLOCK_GROUP),
                                                            PAT_AGE_GROUP = as.numeric(PAT_AGE_GROUP))

ip_copd_2015q4_2020 <- bind_rows(ip_data_2018_2020, ip_data_2015_2017)
write_csv(ip_copd_2015q4_2020, "Z:/THCIC/Katz/ip_copd_2015q4_2020.csv")

### combining inpatient and outpatient data ############################################################################################
op_copd_2015q4_2020 <- read_csv("Z:/THCIC/Katz/op_copd_2015q4_2020.csv")
ip_copd_2015q4_2020 <- read_csv("Z:/THCIC/Katz/ip_copd_2015q4_2020.csv")

op_copd_2015q4_2020_join <- op_copd_2015q4_2020 %>% mutate(ip_op = "op",
                                                           SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION))
ip_copd_2015q4_2020_join <- ip_copd_2015q4_2020 %>% mutate(ip_op = "ip",
                                                           SOURCE_OF_ADMISSION = as.character(SOURCE_OF_ADMISSION))

names(op_copd_2015q4_2020_join)
names(ip_copd_2015q4_2020_join)

op_ip_copd_2015q4_2020 <- bind_rows(op_copd_2015q4_2020_join, ip_copd_2015q4_2020_join)

write_csv(op_ip_copd_2015q4_2020, "Z:/THCIC/Katz/op_ip_copd_2015q4_2020.csv")

#sort(names(test))


###visual test on number of cases over time
op_ip_copd_2015q4_2020 <- read_csv("Z:/THCIC/Katz/op_ip_copd_2015q4_2020.csv")
names(op_ip_copd_2015q4_2020)
str(op_ip_copd_2015q4_2020$STMT_PERIOD_FROM)
summary(op_ip_copd_2015q4_2020$STMT_PERIOD_FROM)


op_ip_copd_2015q4_2020 %>% 
  mutate(date_s = ymd(STMT_PERIOD_FROM),
         date_s2 = ymd(ADMIT_START_OF_CARE),
         year_s = year(date_s),
         year_s2 = year(date_s2)) %>% 
  mutate(year_s3 = case_when(is.na(year_s2) ~ year_s,
                             is.na(year_s) ~ year_s2,
                            FALSE ~ 99)) %>% 
  
  group_by(as.character(year_s3)) %>% 
  summarize(n = n())
