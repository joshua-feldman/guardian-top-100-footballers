library(tidyverse)

df <- read_csv("data/male-clean-combined-2022.csv")

# Ranking of certain players over time
df %>%
  filter(name %in% c("Lionel Messi", "Cristiano Ronaldo", "Kylian Mbappé", "Karim Benzema")) %>%
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
