# election_results_analysis
This is a simple data clean on November 2016 Presidential election results for the County of Los Angeles


## Mike came in with a data request for some external party
## they want the 2016 Nov General Election results for LA City and 2nd SV district
## precinct level data: registered voters, ballots cast, turnout percentage
########################################################################################
library(readxl)
library(dplyr)


data <- read_xls("E:/JordanBerninger/nov16_results_request_Mike_010418/PRES_AND_VICE_PRES_11-08-16_by_Precinct_3496-4802.xls", skip = 2)

colnames(data) <- tolower(colnames(data))

data2 <- data %>% select("location","consolidation" = "precinct", "vbm_only" = "vote by mail only", "registration", "type", "ballots" = "ballots cast",
                             "stein" = "jill stein", "clinton" = "hillary clinton", "riva" = "gloria e la riva", 
                             "trump" = "donald j trump", "johnson" = "gary johnson")

la.data <- data2 %>% filter(location == "LOS ANGELES") %>%
  filter(type == "TOTAL") %>%
  filter(vbm_only == "N")

#######################################################################################################
## First output - LA City - registration, ballots cast, turnout by consolidated precinct             ##
la.data.2 <- la.data %>% select(1,2,4,6) %>%                                                         ##
  mutate(turnout = ballots/registration) %>%                                                         ##
  mutate(turnout = sprintf("%1.2f%%", 100*turnout))                                                  ##
setwd("E:/JordanBerninger/nov16_results_request_Mike_010418")                                        ##
##write_csv(la.data.2, path = "nov16-results-consolidated-la-city.csv")                              ##   
#######################################################################################################

channel <- odbcConnect("slavote", uid="stat_user", pwd="samsung", believeNRows=FALSE)
system.time(nov16.pd <- sqlQuery(channel,"SELECT * FROM PrecinctDistrict WHERE election_id = 3496"))
## use consolidation to get precinct_id

nov16.pd.sv <- nov16.pd %>% filter(grepl("SUPERVISORIAL", name_1)) %>% 
  filter(votes_by_mail == "N") %>%
  distinct(consolidation, .keep_all = TRUE) 

data.sv <- left_join(data2, nov16.pd.sv, by = "consolidation") %>%
  filter(type == "TOTAL") %>%
  filter(vbm_only == "N") %>%
  select("sv.district" = 22,1,2,4,6)

## Second output - same metrics by supervisorial district
## not sure if he meant across LA county, or within LA City
## will do both, all county first

data.sv.county <- data.sv %>% select(-2,-3) %>%
  group_by(sv.district) %>% summarise(total.registration = sum(registration), total.ballots = sum(ballots), n.consolidated = n()) %>%
  mutate(turnout = total.ballots/total.registration) %>%
  mutate(turnout = sprintf("%1.2f%%", 100*turnout))

## write_csv(data.sv.county, path = "nov16-results-svdistrict-la-county.csv")
## now the same thing, but just for the consolidated precincts in LA City

data.sv.city <- data.sv %>% filter(location == "LOS ANGELES") %>% 
  select(-2,-3) %>%
  group_by(sv.district) %>% 
  summarise(total.registration = sum(registration), total.ballots = sum(ballots), n.consolidated = n())%>%
  mutate(turnout = total.ballots/total.registration) %>%
  mutate(turnout = sprintf("%1.2f%%", 100*turnout))

## write_csv(data.sv.city, path = "nov16-results-svdistrict-la-city.csv")
