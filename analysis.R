library(tidyverse)
library(showtext)
library(ggtext)

font_add_google("Nunito")
showtext_auto(enable = TRUE)

main_color <- "#2e2e2e"

th <- theme_minimal() +
  theme(text = element_text(family = "Nunito", color = "white"),
        panel.background = element_rect(fill = main_color, color = main_color),
        plot.title = element_text(face = "bold", color = "white"),
        plot.background = element_rect(fill = main_color, color = main_color),
        strip.text = element_text(face = "bold", color = "white"),
        axis.text = element_text(color = "white"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        legend.position = 'top',
        plot.margin = margin(20, 20, 20, 20),
        plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(margin = margin(20, 0, 0, 0)),
        panel.grid.major = element_line(color = "#595959"),
        panel.grid.minor = element_line(color = "#595959"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), color = "white"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), color = "white"))

theme_set(th)

df <- read_csv("data/male-clean-2021.csv")

df %>% 
  count(club = Club.on.20.Dec.2021) %>% 
  arrange(-n) %>% 
  mutate(club = reorder(club, n)) %>% 
  ggplot(aes(n, club, fill = n, label = n)) +
  geom_col(color = NA) +
  geom_text(hjust = 1.1, color = "white", family = "Nunito", fontface = "bold") +
  guides(fill = FALSE) +
  labs(title = "Number of players in the Guardian's *100 Best Male Footballers 2021*",
       subtitle = "Manchester City have the most players in the top 100 (*N = 12*), followed by Chelsea (*N = 10*) and Paris Saint-Germain (*N = 9*).",
       x = "Number of players",
       caption = "Graphic: Joshua Feldman") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  viridis::scale_fill_viridis(begin = 0.25, end = 0.75) +
  theme(axis.title.y = element_blank(),
        # axis.text.x = element_blank(),
        plot.title = element_markdown(face = "bold", color = "white"),
        plot.subtitle = element_markdown(),
        axis.text.y = element_text(margin = margin(0, -20, 0, 0)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("plots/male-2021-players-by-clubs.png")

