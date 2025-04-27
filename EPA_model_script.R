library(nflfastR)
library(dplyr)
library(ggplot2)

pbp_2024 <- load_pbp(2024)


pbp_clean <- pbp_2024 %>%
  filter(play_type %in% c("run", "pass"),
         !is.na(epa)) 

pbp_clean$play_type <- as.factor(pbp_clean$play_type)
epa_model <- lm(epa ~ down + ydstogo + yardline_100 + half_seconds_remaining + score_differential + play_type,
                data = pbp_clean)
summary(epa_model)

team_offense_epa <- pbp_clean %>%
  group_by(posteam) %>%
  summarize(off_epa_per_play = mean(epa)) %>%
  arrange(desc(off_epa_per_play))

head(team_offense_epa, 10)

team_defense_epa <- pbp_clean %>%
  group_by(defteam) %>%
  summarize(def_epa_allowed_per_play = mean(epa)) %>%
  arrange(def_epa_allowed_per_play)

head(team_defense_epa, 10)

data("teams_colors_logos")

team_offense_epa <- team_offense_epa %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

team_defense_epa <- team_defense_epa %>%
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))

ggplot(team_offense_epa, aes(x = reorder(posteam, off_epa_per_play), y = off_epa_per_play, fill = team_color)) +
  geom_col() +
  scale_fill_identity() +
  coord_flip() +
  labs(
    title = "NFL 2024 Team Offensive EPA per Play",
    x = "Team",
    y = "Offensive EPA per Play"
  ) +
  theme_minimal()

ggplot(team_defense_epa, aes(x = reorder(defteam, def_epa_allowed_per_play), y = def_epa_allowed_per_play, fill = team_color)) +
  geom_col() +
  scale_fill_identity() +
  coord_flip() +
  labs(
    title = "NFL 2024 Team Defensive EPA Allowed per Play",
    x = "Team",
    y = "Defensive EPA Allowed per Play"
  ) +
  theme_minimal()


#Diving Deeper into Play Types
library(tidyr)

offense_by_play_type <- pbp_clean %>%
  group_by(posteam, play_type) %>%
  summarize(mean_epa = mean(epa)) %>%
  pivot_wider(names_from = play_type, values_from = mean_epa)
head(offense_by_play_type)

pbp_clean <- pbp_clean %>%
  mutate(
    distance_bucket = case_when(
      ydstogo <= 3 ~ "Short (1-3)",
      ydstogo <= 7 ~ "Medium (4-7)",
      TRUE ~ "Long (8+)"
    )
  )

epa_by_situation <- pbp_clean %>%
  group_by(down, distance_bucket) %>%
  summarize(mean_epa = mean(epa))

epa_by_situation

team_epa_summary <- team_offense_epa %>%
  left_join(team_defense_epa, by = c("posteam" = "defteam"))
team_epa_summary <- team_epa_summary %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_color),
            by = c("posteam" = "team_abbr"))

library(ggplot2)

ggplot(team_epa_summary, aes(x = off_epa_per_play, y = def_epa_allowed_per_play, label = posteam)) +
  geom_point(aes(color = team_color), size = 4) +
  scale_color_identity() +
  geom_text(vjust = -1, size = 3) +
  geom_hline(yintercept = mean(team_epa_summary$def_epa_allowed_per_play), linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = mean(team_epa_summary$off_epa_per_play), linetype = "dashed", color = "gray50") +
  labs(
    title = "NFL 2024: Offensive vs Defensive EPA",
    x = "Offensive EPA per Play",
    y = "Defensive EPA Allowed per Play"
  ) +
  theme_minimal()

team_epa_variability <- pbp_clean %>%
  group_by(posteam) %>%
  summarize(
    mean_epa = mean(epa),
    sd_epa = sd(epa)
  ) %>%
  arrange(desc(sd_epa))

head(team_epa_variability)













