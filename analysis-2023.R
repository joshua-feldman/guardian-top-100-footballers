library(tidyverse)

df <- read_csv("data/male-clean-combined-2023.csv")

df_2023 <- df %>% 
  filter(year == 2023)

table(df_2023$club) %>% sort()
table(df_2023$league) %>% sort()
table(df_2023$nationality) %>% sort()

# Ranking of certain players over time
df %>%
  filter(name %in% c("Erling Haaland", "Jude Bellingham", "Kylian Mbappé")) %>%
  ggplot(aes(year, rank, color = name)) +
  geom_line() +
  geom_point() +
  scale_y_reverse()

# Number of players in certain leagues over time
df %>%
  filter(league %in% c("England", "Spain", "Italy", "Germany", "France")) %>%
  group_by(year, league) %>%
  count() %>%
  ggplot(aes(year, n, color = league)) +
  geom_line() +
  geom_point()

# Number of players in certain clubs over time
df %>%
  count(club) %>%
  arrange(-n) %>%
  top_n(10, n)

df %>%
  filter(club %in% c("Real Madrid", "Barcelona")) %>%
  group_by(year, club) %>%
  count() %>%
  ggplot(aes(year, n, color = club)) +
  geom_line() +
  geom_point()

df %>%
  filter(club %in% c("Manchester City", "Manchester United")) %>%
  group_by(year, club) %>%
  count() %>%
  ggplot(aes(year, n, color = club)) +
  geom_line() +
  geom_point()

# Mean age over time
df %>%
  group_by(year) %>%
  summarise(age = mean(age)) %>%
  ungroup() %>%
  ggplot(aes(year, age)) +
  geom_line() +
  geom_point()

# Position over time
df %>%
  mutate(position = tolower(position)) %>%
  mutate(position = str_trim(position)) %>%
  mutate(position_group = ifelse(position %in% c("defender", "forward", "midfielder", "goalkeeper", "winger"), position, "multiple")) %>%
  # filter(position_group != "other") %>%
  group_by(year, position_group) %>%
  count() %>%
  ggplot(aes(year, n, color = position_group)) +
  geom_line() +
  geom_point()

# Variance by player
var_by_player <- df %>%
  group_by(name) %>%
  summarise(sd = sd(rank), n = n()) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  filter(!is.na(sd))

# Players who have featured every year
every_year_players <- df %>%
  group_by(name) %>%
  summarise(n = n(), mean_rank = mean(rank)) %>%
  ungroup() %>%
  filter(n == 11)

df %>%
  filter(name %in% every_year_players$name) %>%
  ggplot(aes(year, rank, color = name)) +
  geom_line() +
  geom_point() +
  scale_y_reverse()
