library(tidyverse)
library(rvest)

# Old data
old_df <- read_csv("data/male-clean-combined.csv")

club_league_combs <- select(old_df, club, league) %>% 
  distinct() %>% 
  arrange(club)

# Save URLS
url_2022 <- "https://interactive.guim.co.uk/docsdata/1m8GiO3Ee0q30t2GC4m76VbVmVJw-26RHoefB6YkZTQQ.json"

# Save data to data frames
df_2022 <- jsonlite::fromJSON(url_2022) %>% as.data.frame()

# Write to CSV
write.csv(df_2022, "data/male-raw-2022.csv", row.names=FALSE)

# Create modified dataframes
new_colnames <- c("rank", "name", "position", "club", "nationality", "age", "year")

df_2022_mod <- df_2022 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "Age.on"))) %>% 
  mutate(year = "2022")

colnames(df_2022_mod) <- new_colnames

df_combined <- df_2022_mod %>% 
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
    club %in% c("Dinamo Zagreb") ~ "Croatia",
    club %in% c("Newcastle United", "Swansea City", "Stoke City", "Southampton", "Brighton", "Fulham", "West Ham") ~ "England",
    club %in% c("Marseille", "Montpelier", "St Etienne", "Angers") ~ "France",
    club %in% c("Schalke") ~ "Germany",
    club %in% c("Benfica") ~ "Portugal",
    club %in% c("Anzhi Makhachkala", "CSKA Moscow") ~ "Russia",
    club %in% c("Al Nassr") ~ "Saudi Arabia",
    club %in% c("Celtic") ~ "Scotland",
    club %in% c("Fenerbahce") ~ "Turkey",
    TRUE ~ league
  ))

df_combined <- df_combined[,c("year", "rank", "name", "position", "age", "nationality", "club", "league")]
df_combined <- old_df %>% rbind(df_combined)

write.csv(df_combined, "data/male-clean-combined-2022.csv", row.names=FALSE)
