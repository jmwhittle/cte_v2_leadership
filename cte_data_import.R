library(tidyverse)
library(readxl)
library(gdata)

#@jas 5/3/2017
library(tidyverse)
library(readxl)
library(gdata)

#dws industry data
url_dws <- "https://jobs.utah.gov/jsp/almiswage/alloccs/wage-alloccs/?d-4029536-e=2&6578706f7274=1"
download.file(url_dws, destfile = "dws.xls")
industry <- read_excel("dws.xls", col_names = T, skip = 1)
industry$SOC <- stringr::str_sub(industry$`Occupation Title`, -7, -2)

#crosswalk data
url_corsswalk <- "https://www.workforceinfodb.org/ftp/download/SOCCIP2010/blsnces2011.zip"
download.file(url_corsswalk, destfile = "crosswalk.zip")
unzip("crosswalk.zip")
code.crosswalk <- read_xls("blsnces2011.xls", col_names = TRUE)
code.crosswalk$industry.group <- stringr::str_sub(code.crosswalk$soc2010, 1, 2)
code.crosswalk$SOC <- stringr::str_replace_all(code.crosswalk$soc2010, "-", "")
code.crosswalk.2 <- code.crosswalk[,3:6]
code.crosswalk.2$cip2010 <- as.numeric(code.crosswalk.2$cip2010) # changed to enable joining later

#if this file were added to i-drive this could be pulled via file path
academic.programs <- read_excel("2016-17_Master_List_of_Programs_FINAL_edits_2016-07-15.xlsx")
academic.programs <- academic.programs[  , !names(academic.programs) %in% c( "DW", "LISTED CIP CODE IN BANNER", 'X__1', "X__2", "X__3", "X__4", "X__5")]# should remove blank columns (5, can change this due to R naming conventions), and useless columns.

#makes an adjustment to the cip codes imported by read_excel to help with joining
academic.programs$cip2010 <- round(as.numeric(academic.programs$`2010 CIP Code [STVCIPC]`), 6)

#joining all three tables
industry.table <- left_join(industry, code.crosswalk.2, by = "SOC")
industry.academic <- left_join(academic.programs, industry.table, by = "cip2010")
industry.academic <- industry.academic[, !names(industry.academic) %in% c("2010 CIP Code [STVCIPC]")]

#removing some uglyness from the xlsx 
fat <- industry.academic %>% filter(AA == "-----" | 
                                      AAS == "-----" | 
                                      APE == "-----" | 
                                      AS == "-----" | 
                                      CP == "-----" | 
                                      CA == "-----" | 
                                      `CER/CC` == "-----")
industry.academic <- setdiff(industry.academic, fat) #dieting

# cleaning up the format a bit
industry.academic$deg.type <- ifelse(is.na(industry.academic$AA) == F, "AA", 
                                     ifelse(is.na(industry.academic$AS) == F, "AS", 
                                            ifelse(is.na(industry.academic$APE) == F, "APE", 
                                                   ifelse(is.na(industry.academic$AAS) == F, "AAS", 
                                                          ifelse(is.na(industry.academic$`CER/CC`) == F, "CER_CC", 
                                                                 ifelse(is.na(industry.academic$CP) == F, "CP", 
                                                                        ifelse(is.na(industry.academic$CA) == F, "CA", NA)))))))

temp <- unite(industry.academic, credit_hours, 3:9, remove = T, sep = "") 
industry.academic$credit_hours <- gsub("NA", "", temp$credit_hours) 
industry.academic <- industry.academic[,-c(3:9)]

#changing names and data types for the most used variables in industry.academic
industry.academic$hourly_med_wage <- as.numeric(industry.academic$`Hourly Median Wage`)
industry.academic$in_hourly_med_wage <- as.numeric(industry.academic$`Hourly Inexperienced Wage`)
industry.academic$annual_med_wage <- as.numeric(industry.academic$`Annual Median Wage`)
industry.academic$in_ann_med_wage <- as.numeric(industry.academic$`Annual Inexperienced Wage`)
industry.academic$current_jobs <- as.numeric(industry.academic$`Current Statewide DWS Job Orders`)
industry.academic$proj_ann_jobs <- as.numeric(industry.academic$`Projected Average Annual Openings`)
industry.academic$star_ratings <- as.numeric(industry.academic$`Star Ratings`)

#removing redundent columns 
drop.names <- c("Hourly Median Wage", 
                "Hourly Inexperienced Wage",
                "Annual Median Wage", 
                "Annual Inexperienced Wage", 
                "Projected Average Annual Openings", 
                "Star Ratings", 
                "Current Statewide DWS Job Orders")  
industry.academic <- industry.academic[  , !names(industry.academic) %in% drop.names]

#changing annoying names
names(industry.academic)[names(industry.academic) == "PROGRAM CODE [SMAPRLE]"] <- "program_code"
names(industry.academic)[names(industry.academic) == "Occupation Title"] <- "occupation_title"

#creating CTE only program
cte.table <- industry.academic %>% 
  filter(`CTE Program` == "yes" & 
           Education != "Master's degree" & 
           Education != "Doctoral or professional degree" & 
           Education != "Bachelor's degree")