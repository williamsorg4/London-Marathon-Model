library(tidyverse)
library(lubridate)

LMAnalysisData <- lmEliteResults %>% 
  select(-c(Age, `Birth Date`)) %>% 
  bind_rows(elite2025[c(1:7, 36)])

LMAnalysisData <- LMAnalysisData %>% 
  mutate(Mark = seconds(hms(Mark)))

pastResults <- pastResults %>% 
  mutate(Date = dmy(date)) %>% 
  distinct()

pastResults <- pastResults %>% 
  mutate(Mark = gsub("\\*", "", performance))

pastResults <- pastResults %>% 
  mutate(dob = dmy(dob))

pastResults <- pastResults %>% 
  select(-performance) %>% 
  rename(Discipline = discipline)

pastResults <- pastResults %>% 
  select(-date)

pastResults$Discipline[pastResults$Discipline == "10 Kilometres Road"] <- "10,000 Metres"

parse_mixed_times <- function(times) {
  standardized <- ifelse(grepl("^\\d{1,2}:\\d{2}$", times),
                         paste0("00:", times),
                         times)
  hms(standardized)
}


# Create a function to calculate stats -------------------------------------------------------
calculate_stats <- function(pageIn, date) {
  # Select athletes races from before race day
  dataSubset <- pastResults %>% 
    filter(page == pageIn & Date < date)
  
  # Get marathon stats
  numMarathons = sum(dataSubset$Discipline == "Marathon")
  finishesMarathons = sum(dataSubset$Discipline == "Marathon" & !is.na(hms(dataSubset$Mark)))
  finishpctMarathons = finishesMarathons / numMarathons
  prMarathon = min(seconds(hms(dataSubset$Mark[dataSubset$Discipline == "Marathon"])), na.rm = TRUE)
  worstMarathon = max(seconds(hms(dataSubset$Mark[dataSubset$Discipline == "Marathon"])), na.rm = TRUE)
  avgMarathon = mean(seconds(hms(dataSubset$Mark[dataSubset$Discipline == "Marathon"])), na.rm = TRUE)
  sdMarathon = sd(seconds(hms(dataSubset$Mark[dataSubset$Discipline == "Marathon"])), na.rm = TRUE)
  recentMarathon = dataSubset %>% 
    filter(Discipline == "Marathon") %>% 
    arrange(desc(Date)) %>% 
    pull(Mark) %>% 
    .[1] %>% 
    hms() %>% 
    seconds()
  marathons = seconds(hms(dataSubset$Mark[dataSubset$Discipline == "Marathon"]))
  
  # Get half marathon stats
  numHalfMarathons = sum(dataSubset$Discipline == "Half Marathon")
  finishesHalfMarathons = sum(dataSubset$Discipline == "Half Marathon" & !is.na(parse_mixed_times(dataSubset$Mark)))
  finishpctHalfMarathons = finishesHalfMarathons / numHalfMarathons
  prHalfMarathon = min(seconds(parse_mixed_times(dataSubset$Mark[dataSubset$Discipline == "Half Marathon"])), na.rm = TRUE)
  worstHalfMarathon = max(seconds(parse_mixed_times(dataSubset$Mark[dataSubset$Discipline == "Half Marathon"])), na.rm = TRUE)
  avgHalfMarathon = mean(seconds(parse_mixed_times(dataSubset$Mark[dataSubset$Discipline == "Half Marathon"])), na.rm = TRUE)
  sdHalfMarathon = sd(seconds(parse_mixed_times(dataSubset$Mark[dataSubset$Discipline == "Half Marathon"])), na.rm = TRUE)
  recentHalfMarathon = dataSubset %>% 
    filter(Discipline == "Half Marathon") %>% 
    arrange(desc(Date)) %>% 
    pull(Mark) %>% 
    .[1] %>% 
    parse_mixed_times() %>% 
    seconds()
  halfmarathons = seconds(parse_mixed_times(dataSubset$Mark[dataSubset$Discipline == "Half Marathon"]))
  
  # Get 10k stats
  num10k = sum(dataSubset$Discipline == "10,000 Metres")
  finishes10k = sum(dataSubset$Discipline == "10,000 Metres" & !is.na(ms(dataSubset$Mark)))
  finishpct10k = finishes10k / num10k
  pr10k = min(seconds(ms(dataSubset$Mark[dataSubset$Discipline == "10,000 Metres"])), na.rm = TRUE)
  worst10k = max(seconds(ms(dataSubset$Mark[dataSubset$Discipline == "10,000 Metres"])), na.rm = TRUE)
  avg10k = mean(seconds(ms(dataSubset$Mark[dataSubset$Discipline == "10,000 Metres"])), na.rm = TRUE)
  sd10k = sd(seconds(ms(dataSubset$Mark[dataSubset$Discipline == "10,000 Metres"])), na.rm = TRUE)
  recent10k = dataSubset %>% 
    filter(Discipline == "10,000 Metres") %>% 
    arrange(desc(Date)) %>% 
    pull(Mark) %>% 
    .[1] %>% 
    ms() %>% 
    seconds()
  tenKs = seconds(ms(dataSubset$Mark[dataSubset$Discipline == "10,000 Metres"]))
  
  lastrace = max(dataSubset$Date)
  
  output <- tibble(
        dob = dataSubset$dob[1],
        lastrace = lastrace,
         
         numMarathons = numMarathons,
         finishesMarathons = finishesMarathons,
        finishpctMarathons = finishpctMarathons,
         prMarathon = prMarathon,
         worstMarathon = worstMarathon,
         avgMarathon = avgMarathon,
         sdMarathon = sdMarathon,
         recentMarathon = recentMarathon,
         marathons = list(marathons),
         
         numHalfMarathons = numHalfMarathons,
         finishesHalfMarathons = finishesHalfMarathons,
        finishpctHalfMarathons = finishpctHalfMarathons,
         prHalfMarathon = prHalfMarathon,
         worstHalfMarathon = worstHalfMarathon,
         avgHalfMarathon = avgHalfMarathon,
         sdHalfMarathon = sdHalfMarathon,
         recentHalfMarathon = recentHalfMarathon,
         halfmarathons = list(halfmarathons),
         
         num10k = num10k,
         finishes10k = finishes10k,
         pr10k = pr10k,
         worst10k = worst10k,
         avg10k = avg10k,
         sd10k = sd10k,
         recent10k = recent10k,
         tenKs = list(tenKs))
  return(output)
}




elite2025 <- elite2025 %>% 
  rowwise() %>%
  mutate(result = list(calculate_stats(page, raceDate))) %>%
  unnest(result)

elite2025 <- elite2025 %>% 
  select(1:8, continent, sex)


elite2025 <- elite2025 %>% 
  rename(raceDate = date,
         Nat = Nat.,
         lastRaceDate = lastrace)

elite2025 <- elite2025 %>% 
  mutate(age = raceDate - dob,
         .before = Mark)


LMAnalysisData <- LMAnalysisData %>%
  mutate(
    continent = case_when(
      Nat %in% c("ETH", "KEN", "ERI", "RSA", "UGA") ~ "Africa",
      Nat %in% c("GBR", "IRL", "BEL", "ESP", "UKR", "NED", "MDA", "ITA", "POR", 
                 "POL", "GER", "AUT", "LTU", "FRA", "NOR", "ROU") ~ "Europe",
      Nat %in% c("USA", "CAN", "BRA", "MEX", "PAR") ~ "Americas",
      Nat %in% c("JPN", "IND", "CHN", "ISR", "BRN") ~ "Asia",
      Nat %in% c("AUS") ~ "Oceania"
    ), .before = sex
  )
elite2025 <- elite2025 %>% 
  filter(RaceStatus != "Not Started")

elite2025 <- elite2025 %>% 
  rename(Mark = Finish,
         Place = `Place gender`) %>% 
  select(-c(RaceStatus))


elite2025 <- elite2025 %>%
  mutate(sex = case_when(
    Name %in% c(
      "Megertu ALEMU", "Holly ARCHER", "Tigst ASSEFA", "Molly BOOKMYER", "Phily BOWDEN",
      "Vivian Jepkemei CHERUIYOT", "Stella CHESANG", "Haven Hailu DESSE", "Rose HARVEY",
      "Sifan HASSAN", "Joyciline JEPKOSGEI", "Eilish MCCOLGAN", "Charlotte PURDUE",
      "Susanna SULLIVAN", "Sofiia YAREMCHUK"
    ) ~ "women",
    
    Name %in% c(
      "Jacob ALLEN", "Carl AVERY", "David BISHOP", "Ross BRADEN", "Andrew BUCHANAN",
      "Luke CALDWELL", "Yemaneberhan CRIPPA", "Mohamed ESA", "Weynay GHEBRESILASIE",
      "Andrew HEYES", "James HOAD", "Sean HOGAN", "Eliud KIPCHOGE", "Hillary KIPKOECH",
      "Timothy KIPLAGAT", "Jacob KIPLIMO", "Marcelo LAGUERA", "Alexander LEPRETRE",
      "Adam LIPSCHITZ", "Mahamed MAHAMED", "Jonathan MELLOR", "Milkesa MENGESHA",
      "Alex MILNE", "Sondre Nordstad MOEN", "Alexander MUNYAO", "William MYCROFT",
      "Abdi NAGEEYE", "Amanal PETROS", "Jack ROWE", "Kevin SALVANO", "Sabastian SAWE",
      "Philip SESEMANN", "Jake SMITH", "Logan SMITH", "Chris THOMAS", "Tamirat TOLA",
      "Alex YEE"
    ) ~ "men",
    
    TRUE ~ NA_character_
  ))



saveRDS(elite2025, "elite2025.rds")

LMAnalysisData <- LMAnalysisData %>% 
  filter(year(raceDate) != 2025) %>% 
  bind_rows(elite2025)





saveRDS(LMAnalysisData, "LMAnalysisData.rds")
