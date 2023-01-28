library(tidyverse)
library(rvest)

# Save URLS
url_2018 <- "https://interactive.guim.co.uk/docsdata/1z83ACM3aUr_toXtNlhYPh394Wn9LaGt2_M5Lifgjgrc.json"
url_2019 <- "https://interactive.guim.co.uk/docsdata/1AH6d9-VMIGN9ya326QnbOqpW2yT-RnGJ9vSDN7sNsj8.json"
url_2020 <- "https://interactive.guim.co.uk/docsdata/1zWOS3gGwb4sYnyL-FEKysvtgAb3E1R_KolnT0pDIbAQ.json"
url_2021 <- "https://interactive.guim.co.uk/docsdata/1kzKAF_X_ZAZGj_DyamcMjauxblQC9NP_yODYK4iQGHA.json"
url_2022 <- "https://interactive.guim.co.uk/docsdata/1cESbaFzU0-Sw_BvsJixIS84Bq_VaNAIB0g9iwAXVOfk.json"

# Save data to data frames
df_2018 <- jsonlite::fromJSON(url_2018) %>% as.data.frame()
df_2019 <- jsonlite::fromJSON(url_2019) %>% as.data.frame()
df_2020 <- jsonlite::fromJSON(url_2020) %>% as.data.frame()
df_2021 <- jsonlite::fromJSON(url_2021) %>% as.data.frame()
df_2022 <- jsonlite::fromJSON(url_2022) %>% as.data.frame()

# Write to CSV
write.csv(df_2018, "data/female-raw-2018.csv", row.names=FALSE)
write.csv(df_2019, "data/female-raw-2019.csv", row.names=FALSE)
write.csv(df_2020, "data/female-raw-2020.csv", row.names=FALSE)
write.csv(df_2021, "data/female-raw-2021.csv", row.names=FALSE)
write.csv(df_2022, "data/female-raw-2022.csv", row.names=FALSE)

# Create modified dataframes
new_colnames <- c("rank", "name", "position", "club", "nationality", "age", "league", "year")

df_2018_mod <- df_2018 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on", "league"))) %>% 
  mutate(year = "2018")

df_2019_mod <- df_2019 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on", "league"))) %>% 
  mutate(year = "2019")

df_2020_mod <- df_2020 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on", "league"))) %>% 
  mutate(year = "2020")

df_2021_mod <- df_2021 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on", "league"))) %>% 
  mutate(year = "2021")

df_2022_mod <- df_2022 %>% 
  select(ends_with(c("rank", "sheets.players.name", "position")), 
         contains(c("club", "nationality", "age.on", "league"))) %>% 
  mutate(year = "2022")

colnames(df_2018_mod) <- new_colnames
colnames(df_2019_mod) <- new_colnames
colnames(df_2020_mod) <- new_colnames
colnames(df_2021_mod) <- new_colnames
colnames(df_2022_mod) <- new_colnames

df_combined <- df_2018_mod %>% 
  rbind(df_2019_mod) %>% 
  rbind(df_2020_mod) %>% 
  rbind(df_2021_mod) %>% 
  rbind(df_2022_mod) %>% 
  mutate(rank = as.integer(rank)) %>% 
  mutate(age = as.integer(age))

df_combined <- df_combined[,c("year", "rank", "name", "position", "age", "nationality", "club", "league")]

write.csv(df_combined, "data/female-clean-combined.csv", row.names=FALSE)
