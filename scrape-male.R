library(tidyverse)
library(rvest)

# Save URLS
url_2012 <- "https://www.theguardian.com/football/datablog/2012/dec/24/world-best-footballers-top-100-list"
url_2013 <- "https://www.theguardian.com/news/datablog/2013/dec/24/world-best-footballers-top-100-list-2013-lionel-messi"
url_2014 <- "https://interactive.guim.co.uk/spreadsheetdata/1j7iKilzBNsYPqaR9q0vuKsxbkxbEPtgDYveCz49PEqg.json"
url_2015 <- "https://interactive.guim.co.uk/docsdata/1Q0df6tACnmPHT9EygMGVnuDWaWmN4vA-oY4HeblnL3k.json"
url_2016 <- "https://interactive.guim.co.uk/docsdata/14DihvJu1mYEx7_6q02vaP5D2EJwKi1LaazyF9jbJq6g.json"
url_2017 <- "https://interactive.guim.co.uk/docsdata/1ijYpfwo56EuZuE98Qj1k11WMJC-SRTKj_12kw-Pcrvs.json"
url_2018 <- "https://interactive.guim.co.uk/docsdata/1AKBcrwqtP7K4Gd9WPRrlDXFVpI2eVEa60IOhyHA66Tk.json"
url_2019 <- "https://interactive.guim.co.uk/docsdata/1L1AndH6CdEoXW_2Sjqcwr9EWdh_xmxTVqE4Dslc6vFQ.json"
url_2020 <- "https://interactive.guim.co.uk/docsdata/1vK8-O66EQDxE6yyQM0NEt530ZuADIIAabGur5Ee9_Rs.json"
url_2021 <- "https://interactive.guim.co.uk/docsdata/1cenmJgIfl8Z19CsHumWq4BMrDp45lVXkxALr6c2Z0_Q.json"

# Save data to data frames
df_2012 <- read_html(url_2012) %>% html_table() %>% as.data.frame()
df_2012 <- df_2012[2:nrow(df_2012),]
df_2013 <- read_html(url_2013) %>% html_table() %>% as.data.frame()
df_2014 <- jsonlite::fromJSON(url_2014) %>% as.data.frame()
df_2015 <- jsonlite::fromJSON(url_2015) %>% as.data.frame()
df_2016 <- jsonlite::fromJSON(url_2016) %>% as.data.frame()
df_2017 <- jsonlite::fromJSON(url_2017) %>% as.data.frame()
df_2018 <- jsonlite::fromJSON(url_2018) %>% as.data.frame()
df_2019 <- jsonlite::fromJSON(url_2019) %>% as.data.frame()
df_2020 <- jsonlite::fromJSON(url_2020) %>% as.data.frame()
df_2021 <- jsonlite::fromJSON(url_2021) %>% as.data.frame()

# Write to CSV
write.csv(df_2012, "data/male-raw-2012.csv", row.names=FALSE)
write.csv(df_2013, "data/male-raw-2013.csv", row.names=FALSE)
write.csv(df_2014, "data/male-raw-2014.csv", row.names=FALSE)
write.csv(df_2015, "data/male-raw-2015.csv", row.names=FALSE)
write.csv(df_2016, "data/male-raw-2016.csv", row.names=FALSE)
write.csv(df_2017, "data/male-raw-2017.csv", row.names=FALSE)
write.csv(df_2018, "data/male-raw-2018.csv", row.names=FALSE)
write.csv(df_2019, "data/male-raw-2019.csv", row.names=FALSE)
write.csv(df_2020, "data/male-raw-2020.csv", row.names=FALSE)
write.csv(df_2021, "data/male-raw-2021.csv", row.names=FALSE)

# Find club/league combinations to fill out missing data in early datasets
club_league_combs <- select(df_2021, club = sheets.players.Club.on.20.Dec.2021, league = sheets.players.League) %>% 
  rbind(select(df_2020, club = sheets.players.Club.on.20.Dec.2020, league = sheets.players.League)) %>% 
  rbind(select(df_2019, club = sheets.players.Club.on.20.Dec.2019, league = sheets.players.League)) %>% 
  rbind(select(df_2018, club = sheets.players.Club.on.20.Dec.2018, league = sheets.players.League)) %>% 
  rbind(select(df_2017, club = sheets.players.Club.on.20.Dec.2017, league = sheets.players.League)) %>% 
  rbind(select(df_2016, club = sheets.Sheet1.Club.on.20.Dec.2016, league = sheets.Sheet1.League)) %>% 
  rbind(select(df_2015, club = sheets.Sheet1.Club.on.20.Dec.2015, league = sheets.Sheet1.League)) %>% 
  mutate(league = str_trim(league)) %>% 
  mutate(league = ifelse(club == "Bayern Munich", "Germany", league)) %>% # Fix anomaly in data
  distinct() %>% 
  arrange(club)

# Create modified dataframes
new_colnames <- c("rank", "name", "position", "club", "nationality", "age", "year")

df_2012_mod <- df_2012 %>% 
  mutate(year = "2012")

df_2013_mod <- df_2013 %>% 
  select(-X2012) %>% 
  mutate(year = "2013")

df_2014_mod <- df_2014 %>% 
  select(ends_with(c("rank", "Sheet1.name", "position", "club", "nationality")), 
         contains("ageon")) %>% 
  mutate(year = "2014")

df_2015_mod <- df_2015 %>% 
  select(ends_with(c("rank", "Sheet1.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2015")

df_2016_mod <- df_2016 %>% 
  select(ends_with(c("rank", "Sheet1.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2016")

df_2017_mod <- df_2017 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2017")

df_2018_mod <- df_2018 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2018")

df_2019_mod <- df_2019 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2019")

df_2020_mod <- df_2020 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2020")

df_2021_mod <- df_2021 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on"))) %>% 
  mutate(year = "2021")

colnames(df_2012_mod) <- new_colnames
colnames(df_2013_mod) <- new_colnames
colnames(df_2014_mod) <- new_colnames
colnames(df_2015_mod) <- new_colnames
colnames(df_2016_mod) <- new_colnames
colnames(df_2017_mod) <- new_colnames
colnames(df_2018_mod) <- new_colnames
colnames(df_2019_mod) <- new_colnames
colnames(df_2020_mod) <- new_colnames
colnames(df_2021_mod) <- new_colnames

df_combined <- df_2012_mod %>% 
  rbind(df_2013_mod) %>% 
  rbind(df_2014_mod) %>% 
  rbind(df_2015_mod) %>% 
  rbind(df_2016_mod) %>% 
  rbind(df_2017_mod) %>% 
  rbind(df_2018_mod) %>% 
  rbind(df_2019_mod) %>% 
  rbind(df_2020_mod) %>% 
  rbind(df_2021_mod) %>% 
  mutate(rank = as.integer(rank)) %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(club = case_when(
    club == "Atletico Madrid" ~ "Atlético Madrid",
    club %in% c("Inter", "Internacional") ~ "Internazionale",
    club == "Man City" ~ "Manchester City",
    club %in% c("Man Utd", "Manchester Utd") ~ "Manchester United",
    club %in% c("Newcastle", "Newcastle Utd") ~ "Newcastle United",
    club %in% c("Paris St-Germain", "PSG") ~ "Paris Saint-Germain",
    club == "São Paulo" ~ "São Paulo FC",
    club == "Sporting" ~ "Sporting Lisbon",
    club == "Tottenham" ~ "Tottenham Hotspur",
    club == "Zenit Saint Petersburg" ~ "Zenit St Petersburg",
    TRUE ~ club
  )) %>% 
  left_join(club_league_combs) %>% 
  mutate(league = case_when(
    club %in% c("Corinthians", "Santos") ~ "Brazil",
    club %in% c("Guangzhou Evergrande", "Shanghai Shenhua") ~ "China",
    club %in% c("Newcastle United", "Swansea City", "Stoke City", "Southampton") ~ "England",
    club %in% c("Marseille", "Montpelier", "St Etienne") ~ "France",
    club %in% c("Schalke") ~ "Germany",
    club %in% c("Anzhi Makhachkala", "CSKA Moscow") ~ "Russia",
    club %in% c("Celtic") ~ "Scotland",
    club %in% c("Fenerbahce") ~ "Turkey",
    TRUE ~ league
  ))

df_combined <- df_combined[,c("year", "rank", "name", "position", "age", "nationality", "club", "league")]

write.csv(df_combined, "data/male-clean-combined.csv", row.names=FALSE)
