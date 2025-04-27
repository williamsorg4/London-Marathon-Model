library(rvest)
library(tidyverse)
library(lubridate)
library(RSelenium)
library(netstat)

# Get past London Marathon results ---------------------------------

links <- c("https://worldathletics.org/competition/calendar-results/results/7210829",
           "https://worldathletics.org/competition/calendar-results/results/7191880",
           "https://worldathletics.org/competition/calendar-results/results/7176624",
           "https://worldathletics.org/competition/calendar-results/results/7160158",
           "https://worldathletics.org/competition/calendar-results/results/7138848",
           "https://worldathletics.org/competition/calendar-results/results/7130122",
           "https://worldathletics.org/competition/calendar-results/results/7117866")
names(links) <- c(2024:2018)


# Iterate through world athletics results
lm_results <- tibble()
for(link in links) {
  page <- read_html(link)
  
  # Get men and women finish results
  men_temp <- page %>% 
    html_nodes('table') %>% 
    html_table() %>% 
    .[[1]] %>% 
    .[,1:5]
  
  women_temp <- page %>% 
    html_nodes('table') %>% 
    html_table() %>% 
    .[[2]] %>% 
    .[,1:5]
  
  # Get all athlete page links
  athlete_pages <- page %>% 
    html_nodes('.EventResults_name__3UzJp a') %>% 
    html_attr('href')
  
  # Add race identifier
  men_temp$sex <- "men"
  women_temp$sex <- "women"
  
  # Add athlete pages to tibble
  men_temp$page <- athlete_pages[1:nrow(men_temp)]
  women_temp$page <- athlete_pages[(nrow(men_temp) + 1):(nrow(women_temp) + nrow(men_temp))]
  
  # Combine men and women
  temp <- men_temp %>% 
    rbind(women_temp)
  
  # Add race date
  temp$date <- page %>% 
    html_node(".styles_highlight__1pnl-") %>% 
    html_text()
  
  # Add to complete tibble
  lm_results <- lm_results %>% 
    rbind(temp)
}

# organize and adjust for missing birth date data
lm_results <- lm_results %>% 
  select(date, sex, Place, Name, `Birth Date`, Nat., Mark, page) %>% 
  mutate(date = dmy(date),
         Place = Place %>% 
           gsub('/.', "", .) %>% 
           as.integer(),
         `Birth Date` = case_when(`Birth Date` %in% c(1950:2010) ~ dmy(paste0("15-6-", `Birth Date`)),
                                  .default = dmy(`Birth Date`))) %>% 
  mutate(Age = date - `Birth Date`, .before = page)




# Filter down to only elite ----------------------------------------------
lm_results %>% 
  count(date)

## 2018 -----------------------------------------------------------------
link <- "https://results.tcslondonmarathon.com/2018/?event=ELIT&num_results=100&pid=search&search%5Bnation%5D=%25&search_sort=name"
page <- read_html(link)
table2018 <- page %>% 
  html_table() %>% 
  .[[1]]

indiResults <- page %>% 
  html_nodes(".list-table a") %>% 
  html_attr('href') %>% 
  .[seq(from = 1, to = 81, by = 2)] %>% 
  paste0("https://results.tcslondonmarathon.com/2018/", .)

table2018$link <- indiResults

# Race Status for removing DNS
raceStatuses <- c()
for(link in table2018$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2018$RaceStatus <- raceStatuses

elite2018 <- table2018 %>% 
  select(Name, `Place gender`, Finish, RaceStatus, link) %>% 
  filter(RaceStatus != "Not Started") %>%
  mutate(Name = str_remove(Name, "^»\\s*")) %>% 
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2018-04-22")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)


## 2019 ----------------------------------------------------------------
link <- "https://results.tcslondonmarathon.com/2019/?event=ELIT&num_results=100&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
page <- read_html(link)
table2019 <- tibble(
  `Place gender` = page %>% 
    html_nodes('.row+ .row .place-primary , .place-primary.numeric') %>% 
    html_text() %>% 
    as.integer(),
  Name = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  Finish = page %>% 
    html_nodes(".row+ .row .pull-right .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2019/", .)
)

raceStatuses <- c()
for(link in table2019$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2019$RaceStatus <- raceStatuses


elite2019 <- table2019 %>% 
  filter(RaceStatus != "Not Started") %>%
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2019-04-28")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)

## 2020 ------------------------------------------------------------------
linkmen <- "https://results.tcslondonmarathon.com/2020/?event=LMRM&num_results=50&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
pagemen <- read_html(linkmen)

tablemen2020 <- tibble(
  `Place gender` = pagemen %>% 
    html_nodes('#cbox-main .event-LMRM .place-primary') %>% 
    html_text() %>% 
    as.integer(),
  Name = pagemen %>% 
    html_nodes(".type-eval a") %>% 
    html_text(),
  Finish = pagemen %>% 
    html_nodes("#cbox-main .event-LMRM .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = pagemen %>% 
    html_nodes(".type-eval a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2020/", .)
)


linkwomen <- "https://results.tcslondonmarathon.com/2020/?event=LMRW&num_results=50&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
pagewomen <- read_html(linkwomen)

tablewomen2020 <- tibble(
  `Place gender` = pagewomen %>% 
    html_nodes('#cbox-main .event-LMRW .place-primary') %>% 
    html_text() %>% 
    as.integer(),
  Name = pagewomen %>% 
    html_nodes(".type-eval a") %>% 
    html_text(),
  Finish = pagewomen %>% 
    html_nodes("#cbox-main .event-LMRW .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = pagewomen %>% 
    html_nodes(".type-eval a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2020/", .)
)

table2020 <- tablemen2020 %>% 
  rbind(tablewomen2020)


raceStatuses <- c()
for(link in table2020$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status_field.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2020$RaceStatus <- raceStatuses


elite2020 <- table2020 %>% 
  filter(RaceStatus != "Not Started") %>%
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2020-10-04")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)


## 2021 -----------------------------------------------------------
link <- "https://results.tcslondonmarathon.com/2021/?event=ELIT&num_results=100&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
page <- read_html(link)

table2021 <- tibble(
  `Place gender` = page %>% 
    html_nodes('.row+ .row .place-primary , .place-primary.numeric') %>% 
    html_text() %>% 
    as.integer(),
  Name = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  Finish = page %>% 
    html_nodes(".row+ .row .pull-right .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2021/", .)
)

raceStatuses <- c()
for(link in table2021$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status_field.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2021$RaceStatus <- raceStatuses


elite2021 <- table2021 %>% 
  filter(RaceStatus != "Not Started") %>%
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2021-10-03")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)

## 2022 ---------------------------------------------------------

link <- "https://results.tcslondonmarathon.com/2022/?event=ELIT&num_results=50&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
page <- read_html(link)

table2022 <- tibble(
  `Place gender` = page %>% 
    html_nodes('.row+ .row .place-primary , .place-primary.numeric') %>% 
    html_text() %>% 
    as.integer(),
  Name = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  Finish = page %>% 
    html_nodes(".row+ .row .pull-right .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2022/", .)
)

raceStatuses <- c()
for(link in table2022$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status_field.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2022$RaceStatus <- raceStatuses



elite2022 <- table2022 %>% 
  filter(RaceStatus != "Not Started") %>%
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2022-10-02")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)




## 2023 ---------------------------------------------------------------

link <- "https://results.tcslondonmarathon.com/2023/?event=ELIT&num_results=100&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
page <- read_html(link)

table2023 <- tibble(
  `Place gender` = page %>% 
    html_nodes('.row+ .row .place-primary , .place-primary.numeric') %>% 
    html_text() %>% 
    as.integer(),
  Name = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  Finish = page %>% 
    html_nodes(".row+ .row .pull-right .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2023/", .)
)


raceStatuses <- c()
for(link in table2023$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status_field.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2023$RaceStatus <- raceStatuses


elite2023 <- table2023 %>% 
  filter(RaceStatus != "–") %>%
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2023-04-23")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)


## 2024 --------------------------------------------------------------
link <- "https://results.tcslondonmarathon.com/2024/?event=ELIT&num_results=100&pid=search&pidp=start&search%5Bsex%5D=%25&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
page <- read_html(link)

table2024 <- tibble(
  `Place gender` = page %>% 
    html_nodes('.row+ .row .place-primary , .place-primary.numeric') %>% 
    html_text() %>% 
    as.integer(),
  Name = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  Finish = page %>% 
    html_nodes(".row+ .row .pull-right .type-time") %>% 
    html_text() %>% 
    gsub("Finish", "", .),
  link = page %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2024/", .)
)


raceStatuses <- c()
for(link in table2024$link) {
  page <- read_html(link)
  
  raceStatus <- page %>% 
    html_node('.f-race_status_field.last') %>% 
    html_text()
  
  raceStatuses <- append(raceStatuses, raceStatus)
  
}

table2024$RaceStatus <- raceStatuses



elite2024 <- table2024 %>% 
  filter(RaceStatus != "–") %>%
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2024-04-21")
  ) %>% 
  select(date, Name, `Nat.`, Finish, `Place gender`, RaceStatus, link)





## Filter -------------------------------------------------------
# Get only elite runners from lm_results

lmEliteResults <- elite2018 %>% 
  mutate(Name = case_when(
    Name == "Jonny MELLOR" ~ "Jonathan MELLOR",
    Name == "Yohanes GEBREGERGISH" ~ "Yohanes GHEBREGERGIS",
    Name == "Mary KEITANY" ~ "Mary Jepkosgei KEITANY",
    Name == "Guye ADOLA" ~ "Guye Idemo ADOLA",
    Name == "Rebecca WADE" ~ "Becky WADE",
    Name == "Ihor OLEFIRENKO" ~ "Igor OLEFIRENKO",
    Name == "Daniel WANJIRU" ~ "Daniel KINYUA",
    Name == "Tola Shura KITATA" ~ "Shura KITATA",
    Name == "Vivian CHERUIYOT" ~ "Vivian Jepkemei CHERUIYOT",
    TRUE ~ Name
  )) %>% 
  inner_join(lm_results %>% 
               filter(year(date) == 2018)) %>% 
  select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)


lmEliteResults <- lmEliteResults%>% 
  rbind(elite2019 %>% 
          mutate(Name = case_when(
            Name == "Derlis AYALA" ~ "Derlys AYALA",
            Name == "Vivian CHERUIYOT" ~ "Vivian Jepkemei CHERUIYOT",
            Name == "Mick CLOHISEY" ~ "Michael CLOHISEY",
            Name == "Andy DAVIES" ~ "Andrew DAVIES",
            Name == "Leul GEBRESILASIE" ~ "Leul GEBRESILASE",
            Name == "Mary KEITANY" ~ "Mary Jepkosgei KEITANY",
            Name == "Wilson KIPSANG" ~ "Wilson Kipsang KIPROTICH",
            Name == "Tola Shura KITATA" ~ "Shura KITATA",
            Name == "Linet MASAI" ~ "Linet Chepkwemoi MASAI",
            Name == "Ihor OLEFIRENKO" ~ "Igor OLEFIRENKO",
            Name == "Ahmed Osman" ~ "Ahmed OSMAN",
            Name == "Nitender Singh RAWAT" ~ "Nitender Rawat SINGH",
            Name == "Carla Salome ROCHA" ~ "Salomé ROCHA",
            Name == "Sonia SAMUELS" ~ "Sonia THOMAS-SAMUELS",
            Name == "Haftamnesh TESFAY" ~ "Haftamnesh TESFAYE",
            Name == "Ruth van der MEIJDEN" ~ "Ruth VAN DER MEIJDEN",
            Name == "Daniel WANJIRU" ~ "Daniel KINYUA",
            TRUE ~ Name
          )) %>% 
          inner_join(lm_results %>% 
                       filter(year(date) == 2019)) %>% 
          filter(Mark != "DQ") %>% 
          select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)
  )


lmEliteResults <- lmEliteResults%>% 
  rbind(elite2020 %>%
          mutate(
            Name = case_when(
              Name == "Remigijus KANCYS" ~ "Remigijus KANČYS",
              Name == "Gideon KIPKETER" ~ "Gideon Kipkemoi KIPKETER",
              Name == "Cam LEVINS" ~ "Cameron LEVINS",
              Name == "Juan LUIS BARRIOS" ~ "Juan Luis BARRIOS",
              Name == "Sondre NORDSTAD MOEN" ~ "Sondre Nordstad MOEN",
              Name == "Shelia CHELANGAT" ~ "Sheila CHELANGAT",  # Assuming typo correction
              Name == "Vivian CHERUIYOT" ~ "Vivian Jepkemei CHERUIYOT",
              Name == "Lydia MATHATHI" ~ "Lydia Njeri MATHATHI",
              Name == "Eilish MCCOLGAN" ~ "Liz MCCOLGAN",
              Name == "Alemu MEGERTU" ~ "Megertu ALEMU",
              Name == "Carla Salome ROCHA" ~ "Salomé ROCHA",
              TRUE ~ Name
            )) %>% 
          inner_join(lm_results %>% 
                       filter(year(date) == 2020)) %>% 
          select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)
        )


lmEliteResults <- lmEliteResults%>% 
  rbind(elite2021 %>%
          mutate(Name = case_when(
            Name == "Weynay GHEBRESELASSIE" ~ "Weynay GHEBRESILASIE",
            Name == "Alemu MEGERTU" ~ "Megertu ALEMU",
            .default = Name
          ),
          Nat. = case_when(
            Name == "Joan Chelimo MELLY" ~ "ROU",
            .default = Nat.
          )) %>% 
          inner_join(lm_results %>% 
                       filter(year(date) == 2021)) %>% 
          select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)
        )


lmEliteResults <- lmEliteResults%>% 
  rbind(elite2022 %>%
          mutate(Name = case_when(
            Name == "Jia ERENJIA" ~ "Erenjia JIA",
            Name == "Alemu MEGERTU" ~ "Megertu ALEMU",
            Name == "Mary NGUGI" ~ "Mary Wacera NGUGI",
            .default = Name
          )) %>% 
          inner_join(lm_results %>% 
                       filter(year(date) == 2022)) %>%
          select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)
        )

lmEliteResults <- lmEliteResults%>% 
  rbind(elite2023 %>%
          mutate(Name = case_when(
            Name == "Nicholas BOWKER" ~ "Nick BOWKER",
            Name == "Shelia CHEPKIRUI" ~ "Sheila CHEPKIRUI",
            Name == "Sir Mo FARAH" ~ "Mo FARAH",
            Name == "Tom GROSCHEL" ~ "Tom GRÖSCHEL",
            Name == "Alemu MEGERTU" ~ "Megertu ALEMU",
            Name == "Phil SESEMANN" ~ "Philip SESEMANN",
            .default = Name
          )) %>% 
          inner_join(lm_results %>% 
                       filter(year(date) == 2023)) %>% 
          select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)
        )


lmEliteResults <- lmEliteResults%>% 
  rbind(elite2024 %>%
          mutate(Name = case_when(
            Name == "Helen GAUNT" ~ "Helen WINSOR",
            Name == "Alexander Mutiso MUNYAO" ~ "Alexander MUNYAO",
            Name == "Charlie SANDISON" ~ "Charles SANDISON",
            Name == "Sean TOBIN" ~ "Seán TOBIN",
            .default = Name
          )) %>%
          inner_join(lm_results %>% 
                       filter(year(date) == 2024)) %>% 
          select(date, Name, Nat., Age, sex, Mark, Place, `Birth Date`, link, page)
  )


saveRDS(lmEliteResults, "lmEliteResults.rds")

# Get 2025 London Marathon Elite Field -------------------------------
## Get Startlist ----------------------------------------------

menlink <- "https://results.tcslondonmarathon.com/2025/?event=ELIT&pid=search&pidp=start&search%5Bsex%5D=M&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
menpage <- read_html(menlink)


mentable2025 <- tibble(
  Name = menpage %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  link = menpage %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2024/", .),
  sex = "men"
)


womenlink <- "https://results.tcslondonmarathon.com/2025/?event=ELIT&num_results=50&pid=search&pidp=start&search%5Bsex%5D=W&search%5Bage_class%5D=%25&search%5Bnation%5D=%25&search_sort=name"
womenpage <- read_html(womenlink)


womentable2025 <- tibble(
  Name = womenpage %>% 
    html_nodes(".type-fullname a") %>% 
    html_text(),
  link = womenpage %>% 
    html_nodes(".type-fullname a") %>% 
    html_attr('href') %>% 
    paste0("https://results.tcslondonmarathon.com/2024/", .),
  sex = "women"
)

table2025 <- womentable2025 %>% 
  bind_rows(mentable2025)



elite2025 <- table2025 %>% 
  separate(Name, into = c("Last", "rest"), sep = ", ", remove = TRUE) %>% 
  separate(rest, into = c("First", "Country"), sep = " \\(", remove = TRUE) %>% 
  mutate(
    `Nat.` = str_remove(Country, "\\)"),
    Name = paste(str_squish(First), str_squish(Last)),
    date = ymd("2025-04-27")
  ) %>% 
  select(date, Name, `Nat.`, link, sex)


# Get all world athletics links that already exist
elite2025 <- elite2025 %>% 
  mutate(Name = case_when(
    Name == "Alexander Mutiso MUNYAO" ~ "Alexander MUNYAO",
    Name == "Weynay GHEBRESILASE" ~ "Weynay GHEBRESILASIE",
    Name == "Vivian CHERUIYOT" ~ "Vivian Jepkemei CHERUIYOT",
    Name == "Sondre Norstad MOEN" ~ "Sondre Nordstad MOEN",
    .default = Name
  )) %>% 
  left_join(lm_results %>% 
              select(Name, page) %>% 
              distinct())


elite2025$page[elite2025$Name == "Alex YEE"] <- "/athletes/great-britain-ni/alexander-yee-14644036"
elite2025$page[elite2025$Name == "Phily BOWDEN"] <- "/athletes/great-britain-ni/philippa-bowden-14586863"
elite2025$page[elite2025$Name == "Chris THOMAS"] <- "/athletes/great-britain-ni/chris-thomas-15015981"

## Use Selenium to get all World Athletics Links -------------------

# All names without attached world athletics
missingNames <- elite2025$Name[is.na(elite2025$page)]
missingPages <- rep(NA, length(missingNames))


rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "134.0.6998.165",
                             verbose = FALSE, 
                             port = free_port())

remDr <- rs_driver_object$client

remDr$open()

remDr$navigate("https://worldathletics.org/athletes/ethiopia/kenenisa-bekele-14181357")


# Iterate through missingNames
for (i in 1:length(missingNames)) {
  name <- missingNames[i]
  
  search_box <- remDr$findElement(using = 'css selector', '.athletesProfileTabs_athletesProfileTabsSearchField__21A7R .AthleteSearchField_searchInput__gS3eH')
  search_box$sendKeysToElement(list(name, key = 'enter'))
  Sys.sleep(2)
  search_result <- remDr$findElement(using = 'css', '#__next > div:nth-child(3) > div > div > div.athletesProfileTabs_athletesProfileTabsSearchField__21A7R > div > div.AthleteSearchField_optionWrap__1Oogf > div > a:nth-child(1)')
  templink <- search_result$getElementAttribute("href") %>% 
    gsub("https://worldathletics.org", "", .)
  
  missingPages[i] <- templink
  Sys.sleep(1)
  search_box$clearElement()
  elite2025$page[elite2025$Name == name] <- templink
}

saveRDS(elite2025, "elite2025.rds")




# Get all athletes history through world athletics using selenium -------

allAthletes <- lmEliteResults %>% 
  select(Name, page) %>% 
  rbind(elite2025 %>% 
          select(Name, page)) %>%
  distinct()


wait_for_element <- function(using = 'css selector', value, timeout = 10) {
  for (i in 1:timeout) {
    try({
      element <- remDr$findElement(using = using, value = value)
      if (!is.null(element)) return(element)
    }, silent = TRUE)
    Sys.sleep(1)
  }
  stop(paste("Element not found with", using, ":", value))
}

#pastResults <- tibble()

for (i in 1:length(allAthletes$page)) {
  name <- allAthletes$Name[i]
  page <- allAthletes$page[i]
  
  remDr$navigate(paste0("https://worldathletics.org", page))
  
  dob_elem <- wait_for_element("css selector", ".athletesBio_athletesBioDetails__1wgSI:nth-child(2) .athletesBio_athletesBioTagValue__oKZC4")
  dob <- dob_elem$getElementText() %>% unlist()
  
  statpage <- wait_for_element("css selector", ".athletesTabsButton_AthletesTabsButtonItem__1pPWF+ .athletesTabsButton_AthletesTabsButtonItem__1pPWF .athletesButton_underline__9GAM2")
  statpage$clickElement()
  
  resultpage <- wait_for_element("css selector", ".athletesTabsButton_AthletesTabsButtonItem__1pPWF:nth-child(5) .athletesButton_athletesButton__1_h0o")
  resultpage$clickElement()
  
  year_select <- wait_for_element("css selector", ".athletesSelectInput_athletesSelectContainer__3v2oU+ .athletesSelectInput_athletesSelectContainer__3v2oU .css-1hwfws3")
  year_select$clickElement()
  
  yearlist_elem <- wait_for_element("css selector", "#__next > div:nth-child(4) > div > div > div.athletesStatsInfos_athletesStatsInfos__22gUO > div.athletesStatisticsTable_athletesStatisticsTable__3Eq1T.athletesStatsInfos_itemXL__2SfPY > div.athletesStatisticsTable_athletesStatisticsContent__dDNOs > div > div.profileStatistics_fullHeight__2Nn0b > div.profileStatistics_tabContent__vINmY.profileStatistics_active__1QQ9F.profileStatistics_results__1xala > div.profileStatistics_statsContainer__96c3m > div.profileStatistics_filterWidth__1B10f > div:nth-child(2) > div > div.athletesSelectInput__menu.css-1trz5dz-menu > div")
  yearlist <- yearlist_elem$getElementText()
  careerlength <- yearlist %>% str_split("\n") %>% unlist() %>% length()
  
  for (j in 0:(careerlength - 1)) {
    # Select year
    yearButton <- wait_for_element("xpath", sprintf("//*[starts-with(@id, 'react-select-') and contains(@id, '-option-%d')]", j))
    yearButton$clickElement()
    
    # Visible table
    Sys.sleep(0.5)
    resultTable_elem <- wait_for_element("css selector", ".profileStatistics_flexGrowScroll__3O74_ .profileStatistics_statsTable__xU9PN")
    resultTable <- resultTable_elem$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      .[[1]] %>% 
      select(1:4)
    
    race_count <- nrow(resultTable)
    # temporary_invisible <- tibble()
    print(race_count)
    # for (k in seq(from = 1, to = race_count)) {
    #   race <- wait_for_element("css selector", paste0(".profileStatistics_flexGrowScroll__3O74_ tr:nth-child(", k, ") td:nth-child(5)"))
    #   race$clickElement()
    #   Sys.sleep(1)
    #   hidden_row_elem <- wait_for_element("css selector", ".profileStatistics_trDropdown__1WwxW td")
    #   hidden_row <- hidden_row_elem$getElementText() %>%
    #     str_split("\n") %>%
    #     .[[1]]
    #   
    #   names_columns <- c("Country", "ResultScore", "Category", "Race", "Place")
    #   variables <- hidden_row[c(FALSE, TRUE)]
    #   
    #   if (length(variables) == 3){
    #     variables <- c(variables[1], 0, variables[2], variables[3], NA)
    #   }
    #   if (length(variables) == 4){
    #     variables <- c(variables[1], NA, variables[2], variables[3], variables[4])
    #   }
    #   if (length(variables) == 6){
    #     variables <- variables[1:5]
    #   }
    #   
    #   temporary_hidden_row <- tibble(!!!setNames(variables, names_columns))
    #   temporary_invisible <- bind_rows(temporary_invisible, temporary_hidden_row)
    #   
    #   # Collapse hidden row
    #   race <- wait_for_element("css selector", paste0(".profileStatistics_flexGrowScroll__3O74_ tr:nth-child(", k, ") td:nth-child(5)"))
    #   race$clickElement()
    # }
    
    resultTable <- resultTable %>% 
      mutate(row = row_number()) %>% 
      mutate(name = name,
             dob = dob,
             page = page,
             .before = Discipline)
    
    # temporary_invisible <- temporary_invisible %>% 
    #   mutate(row = row_number())
    # 
    # resultTable <- resultTable %>% 
    #   full_join(temporary_invisible, join_by(row))
    
    pastResults <- pastResults %>% 
      bind_rows(resultTable)
    
    # Open Year selector for next iteration
    year_select <- wait_for_element("css selector", ".athletesSelectInput_athletesSelectContainer__3v2oU+ .athletesSelectInput_athletesSelectContainer__3v2oU .css-1hwfws3")
    year_select$clickElement()
    Sys.sleep(1)
  }
}


saveRDS(pastResults, "pastResults.rds")
