library(tidyverse)
library(lubridate)

LMAnalysisData <- lmEliteResults %>% 
  select(-c(Age, `Birth Date`)) %>% 
  bind_rows(elite2025 %>% 
              mutate(Mark = NA,
                     Place = NA))

LMAnalysisData <- LMAnalysisData %>% 
  mutate(Mark = seconds(hms(Mark)))

pastResults <- pastResults %>% 
  mutate(Date = dmy(Date)) %>% 
  select(1:7) %>% 
  distinct()

parse_mixed_times <- function(times) {
  standardized <- ifelse(grepl("^\\d{1,2}:\\d{2}$", times),
                         paste0("00:", times),
                         times)
  hms(standardized)
}

calculate_stats <- function(pageIn, date) {
  dataSubset <- pastResults %>% 
    filter(page == pageIn & Date < date)
  
  numMarathons = sum(dataSubset$Discipline == "Marathon")
  finishesMarathons = sum(dataSubset$Discipline == "Marathon" & !is.na(dataSubset$Mark))
  prMarathon = min(seconds(hms(dataSubset$Mark[dataSubset$Discipline == "Marathon"])), na.rm = TRUE)
  
  numHalfMarathons = sum(dataSubset$Discipline == "Half Marathon")
  finishesHalfMarathons = sum(dataSubset$Discipline == "Half Marathon" & !is.na(dataSubset$Mark))
  prHalfMarathon = min(seconds(parse_mixed_times(dataSubset$Mark[dataSubset$Discipline == "Half Marathon"])), na.rm = TRUE)
}


pageIn <- LMAnalysisData$page[1]
date <- LMAnalysisData$date[1]
calculate_stats(LMAnalysisData$page[1], LMAnalysisData$date[1]) %>% view()
