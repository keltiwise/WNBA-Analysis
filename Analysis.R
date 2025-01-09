# clear environment before starting analysis
rm(list = ls())

# install dependencies
# install.packages("wehoop") # R package with all WNBA and NCAA stats
library(wehoop) # after installing load 
library(dplyr)
library(tidyverse)
library(ggplot2)
# install released version from CRAN:
# install.packages("sportyR")
library(sportyR)
# install.packages("ggimage")
library(ggimage)
library(grid)
library(gridExtra)
library(grid)
library(gridExtra)
library(ggimage)

# ------- LOAD DATA ----------

# load play by play data
pbp <- wehoop::load_wnba_pbp()
# load team data with ID's
teams <- wehoop::espn_wnba_teams()
print(teams)
# load team box scores
teambox <- wehoop::load_wnba_team_box()
# column names for teambox
colnames(teambox)
# only team box scores for 2024 season
teambox24 <- teambox[teambox$season == 2024, ]
# gather team logos and create seperate data frame
teamlogos <- teambox24[c("team_logo", "team_name")] 
teamlogos <- teamlogos %>%
  distinct(team_name, .keep_all = TRUE) # only distinct values - do not need multiple

# ------- REGRESSION MODELING -------

# add scoring margin to teambox data
teambox1 <- teambox %>%
  mutate(scoring_margin = team_score - opponent_team_score)

# Filter relevant columns for analysis
regression_data <- teambox1 %>%
  filter(season == 2024) %>%  # Use only the 2024 season data
  select(scoring_margin, field_goal_pct, three_point_field_goal_pct, 
         free_throw_pct, total_rebounds, assists, turnovers) %>%
  drop_na()  # Remove rows with missing values

# Fit a linear regression model
lm_model <- lm(scoring_margin ~ field_goal_pct + three_point_field_goal_pct + 
                 free_throw_pct + total_rebounds + assists + turnovers, 
               data = regression_data)

# Summarize the model
summary(lm_model)

# Evaluate the model with diagnostic plots
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots
plot(lm_model)

# Predict scoring margins and compare with actual values
regression_data <- regression_data %>%
  mutate(predicted_margin = predict(lm_model, regression_data),
         residual = scoring_margin - predicted_margin)

# Visualize actual vs predicted scoring margins
ggplot(regression_data, aes(x = scoring_margin, y = predicted_margin)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Scoring Margins",
    x = "Actual Scoring Margin",
    y = "Predicted Scoring Margin"
  ) +
  theme_minimal()

# Visualize residuals
ggplot(regression_data, aes(x = predicted_margin, y = residual)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals of the Regression Model",
    x = "Predicted Scoring Margin",
    y = "Residual"
  ) +
  theme_minimal()

# ------- ANALYSIS ----------

# new data frame where 2024 season data is grouped by team to get the sum of all
# data - team box was split per game
teamsums <- teambox24 %>%
  group_by(team_name) %>%
  summarize(
    teamlogo = first(team_logo), # keep team logo in data set
    across(where(is.numeric), sum, na.rm = TRUE))

# remove team usa and olympic row and remove ID columns
teamsums <- teamsums[-c(12:13), ]
teamsums <- teamsums[-c(3:7, 30:31)] # columns not needed
# get frequency statistics to find average of summed columns
totals <- teambox24 %>%
  count(team_name, sort = TRUE, name = "Frequency")
totals <- totals[-c(13, 14), ]
# merge frequency data frame into totals
teamsums <- merge(teamsums, totals, by = "team_name")
# average stats per game
teamaverages <- teamsums %>%
  group_by(team_name) %>%  # group by team
  summarize(
    teamlogo = first(teamlogo),
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE) / Frequency, .names = "avg_{.col}"))  # calculate averages

# plotting average 3 pointers attempted vs average 3 pointers made
ggplot(teamaverages, (aes(x = avg_three_point_field_goals_attempted, y = 
                   avg_three_point_field_goals_made))) + 
  geom_image(aes(image = teamlogo), size = 0.08) + # adding team logo
  labs(
    title = "WNBA Team Averages: 3 Pointers Attempted vs. Made",
    subtitle = "Averages Per Game",
    x = "Average 3 Pointers Attempted Per Game",
    y = "Average 3 Pointers Made Per Game"
  ) + 
  theme_minimal() + # white background 
  theme( # make it stand out more to readers
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# plotting 3 point percent vs 2 point percent
ggplot(teamaverages, aes(x = avg_field_goal_pct, 
                         y = avg_three_point_field_goal_pct)) + 
  geom_image(aes(image = teamlogo), size = 0.08) + # adding team logo
  labs(
    title = "WNBA Team Averages: 3 Point % vs 2 Point %",
    subtitle = "Averages Per Game",
    x = "Average Field Goal Percent (2 Pointers) Per Game",
    y = "Average Three Point Field Goal Percent Per Game"
  ) +
  theme_minimal() + 
  theme( # make it stand out again
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# looking into shooting trends by quarter
quarter_pbp <- pbp[pbp$shooting_play == "TRUE", ] # narrow down rows that only were shooting plays
# group by team and period number the shot occured in
quarter_pbp_shots <- quarter_pbp %>%
  group_by(team_id, period_number) %>%
  summarise(total_shots = n(), .groups = "drop")
# drop team usa and team wnba - not a part of wnba season
quarter_pbp_shots <- quarter_pbp_shots[quarter_pbp_shots$team_id != 96, ]
quarter_pbp_shots <- quarter_pbp_shots[quarter_pbp_shots$team_id != 97, ]
quarter_pbp_shots <- quarter_pbp_shots[quarter_pbp_shots$period_number != 5, ]
quarter_pbp_shots <- quarter_pbp_shots[quarter_pbp_shots$period_number != 6, ]
# adding team name
quarter_pbp_shots <- quarter_pbp_shots %>%
  mutate(team = case_when(
    team_id == 20 ~ "Atlanta",
    team_id == 19 ~ "Chicago",
    team_id == 18 ~ "Connecticut",
    team_id == 3 ~ "Dallas",
    team_id == 5 ~ "Indiana",
    team_id == 17 ~ "Las Vegas",
    team_id == 6 ~ "Los Angeles",
    team_id == 8 ~ "Minnesota",
    team_id == 9 ~ "New York",
    team_id == 11 ~ "Phoenix",
    team_id == 14 ~ "Seattle",
    team_id == 16 ~ "Washington"
  ))
# adding team name
quarter_pbp_shots <- quarter_pbp_shots %>%
  mutate(team_name = case_when(
    team == "Atlanta" ~ "Dream",  
    team == "Chicago" ~ "Sky",
    team == "Connecticut" ~ "Sun",  
    team == "Dallas" ~ "Wings",
    team == "Indiana" ~ "Fever",
    team == "Las Vegas" ~ "Aces",
    team == "Los Angeles" ~ "Sparks",
    team == "Minnesota" ~ "Lynx",
    team == "New York" ~ "Liberty",
    team == "Phoenix" ~ "Mercury",
    team == "Seattle" ~ "Storm",
    team == "Washington" ~ "Mystics"
  ))
# merge to include team name in data frame
quarter_pbp_shots <- merge(quarter_pbp_shots, totals, by = "team_name")
# to find average using games played by each team
quarter_pbp_shots$avg <- quarter_pbp_shots$total_shots / quarter_pbp_shots$Frequency
# merge team logos to data set for plotting
quarter_pbp_shots<- merge(quarter_pbp_shots, teamlogos, by = "team_name")
# plot total shots and period
ggplot(quarter_pbp_shots, aes(x = period_number, y = total_shots, 
                              color = team, group = team)) + 
  geom_line(size = 1.2) +  # draw the lines
  geom_point(size = 3) +   # add points to the lines
  scale_x_continuous(breaks = seq(1, max(quarter_pbp_shots$period_number), 1)) +  # ensure quarters are labeled correctly
  labs(
    title = "Total Shots by Quarter and Team",
    x = "Quarter",
    y = "Total Shots",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
# using smooth lines instead of harsh lines
ggplot(quarter_pbp_shots, aes(x = period_number, y = total_shots, group = team)) + 
  geom_smooth(method = "loess", size = 1.2, se = FALSE) +   
  geom_image(aes(image = team_logo), size = 0.1) + 
  scale_x_continuous(breaks = seq(1, max(quarter_pbp_shots$period_number), 1)) +  
  labs(
    title = "Total Shots by Quarter and Team",
    x = "Quarter",
    y = "Total Shots",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
# plotting the average shots per game
ggplot(quarter_pbp_shots, aes(x = period_number, y = avg, 
                              group = team)) + 
  geom_line(size = 1.2) +  # Draw the lines
  geom_image(aes(image = team_logo), size = 0.1) +   # Add points to the lines
  scale_x_continuous(breaks = seq(1, max(quarter_pbp_shots$period_number), 1)) +  # Ensure quarters are labeled correctly
  labs(
    title = "Average Shots by Quarter and Team",
    subtitle = "Average Per Game",
    x = "Quarter",
    y = "Average Shots",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
# dividing each plot into team so there is no overlap
ggplot(quarter_pbp_shots, aes(x = period_number, y = avg, group = team)) + 
  geom_line(size = 1.2) +  # Draw the lines
  geom_image(aes(image = team_logo), size = 0.1) +  # add team logos as points
  scale_x_continuous(breaks = seq(1, max(quarter_pbp_shots$period_number), 1)) + 
  scale_y_continuous(limits = c(20, 23)) + 
  labs(
    title = "Average Shots by Quarter and Team",
    x = "Quarter",
    y = "Average Shots",
    color = "Team"
  ) +
  facet_wrap(~ team, scales = "free_y") +  # facet wrap by team
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12)  # adjusts facet labels' text size
  )
# round to nearest whole number
quarter_pbp_shots$wholeavg <- round(quarter_pbp_shots$avg)
# using rounded average and smooth lines
ggplot(quarter_pbp_shots, aes(x = period_number, y = wholeavg, group = team)) + 
  geom_smooth(method = "loess", size = 1.2, se = FALSE, color = 'black') +
  geom_image(aes(image = team_logo), size = 0.1) +  # add team logos as points
  scale_x_continuous(breaks = seq(1, max(quarter_pbp_shots$period_number), 1)) + 
  scale_y_continuous(limits = c(20, 23)) + 
  labs(
    title = "Average Shots by Quarter and Team",
    x = "Quarter",
    y = "Average Shots",
    color = "Team"
  ) +
  facet_wrap(~ team) +  # facet wrap by team
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12)  # adjusts facet labels' text size
  )

# analyzing team performance
team_performance <- teambox %>%
  group_by(team_name) %>%
  summarise(
    avg_score = mean(team_score, na.rm = TRUE),
    avg_fg_pct = mean(field_goal_pct, na.rm = TRUE),
    avg_3pt_pct = mean(three_point_field_goal_pct, na.rm = TRUE),
    avg_ft_pct = mean(free_throw_pct, na.rm = TRUE),
    avg_rebounds = mean(total_rebounds, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_score))
# drop team usa and team wnba
team_performance <- team_performance[team_performance$team_name != "Team WNBA", ]
team_performance <- team_performance[team_performance$team_name != "Team USA", ]
team_performance

# correlation between average field goal percentage and average score
correlation <- cor(team_performance$avg_fg_pct, team_performance$avg_score, use = "complete.obs")
print(paste("Correlation between FG% and Avg Score:", round(correlation, 2)))

# looking into athlete performance
athlete <- wehoop::load_wnba_player_box() # load athlete database
table(athlete$team_name)
# drop team usa and team wnba
athlete <- athlete[athlete$team_name != "Team WNBA" & athlete$team_name != "Team USA", ]
# get distinct values of athletes - don't need repeats
athlete_unique <- athlete %>%
  distinct(athlete_id, athlete_display_name, team_logo, team_color) %>%
  rename(athlete_id_1 = athlete_id)
# adding player id and name to play by play data
pbp <- pbp %>%
  left_join(athlete_unique, by = c("athlete_id_1" = "athlete_id_1"))
# looking into top scoring performers
# filter relevant events: scoring
player_metrics <- pbp %>%
  filter(
    scoring_play == TRUE | 
      type_text %in% c("Assist", "Rebound")
  ) %>%
  select(
    athlete_id_1, 
    team_id, 
    home_team_name, 
    away_team_name, 
    type_text, 
    score_value,
    athlete_display_name,
    team_logo,
    team_color
  )

# aggregate metrics by player
player_contributions <- player_metrics %>%
  group_by(athlete_id_1, team_id, athlete_display_name, team_logo, team_color) %>%
  summarize(
    total_points = sum(score_value, na.rm = TRUE), # total points scored
    total_events = n(), # total events contributed
    .groups = "drop"
  )
# calculate team totals for percentage contribution
team_totals <- pbp %>%
  filter(
    scoring_play == TRUE | 
      type_text %in% c("Assist", "Rebound")
  ) %>%
  group_by(team_id) %>%
  summarize(
    team_total_points = sum(score_value, na.rm = TRUE),
    .groups = "drop"
  )

contributions <- athlete %>%
  group_by(athlete_id, team_id, athlete_display_name, team_name, team_logo, team_color) %>%
  summarize(
    total_rebounds = sum(rebounds, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    .groups = "drop"
  )

# merge player contributions with team totals
player_contributions <- player_contributions %>%
  left_join(team_totals, by = "team_id") %>%
  mutate(
    pct_points = round((total_points / team_total_points) * 100, 2)
  )
# visualize individual contributions
# points
# adding # infront of team color for hex use
player_contributions$team_color <- paste("#", player_contributions$team_color, sep = "")
contributions$team_color <- paste("#", contributions$team_color, sep = "")
# prepare the data (extract top 10 scorers)
data <- player_contributions %>% top_n(10, total_points)
# Create the bar plot
ggplot(data, aes(x = reorder(athlete_display_name, -total_points), y = total_points)) +
  geom_bar(stat = "identity", fill = data$team_color) +
  geom_image(aes(image = team_logo), size = 0.09, by = "height", nudge_x = 0.05) +
  coord_flip() +
  labs(
    title = "Top 10 Scorers",
    subtitle = "2024 Season",
    x = "Player",
    y = "Total Points",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
# narrow down top rebounders
rebounds <- contributions %>% top_n(10, total_rebounds)
# plot top rebounders
ggplot(rebounds, aes(x = reorder(athlete_display_name, -total_rebounds), y = total_rebounds)) +
  geom_bar(stat = "identity", fill = rebounds$team_color) +
  geom_image(aes(image = team_logo), size = 0.09, by = "height", nudge_x = 0.05) +
  coord_flip() +
  labs(
    title = "Top 10 Rebounders",
    subtitle = "2024 Season",
    x = "Player",
    y = "Total Rebounds",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
# narrow down top 10 players with most assists
assists <- contributions %>% top_n(10, total_assists)
# plot most assists
ggplot(assists, aes(x = reorder(athlete_display_name, -total_assists), y = total_assists)) +
  geom_bar(stat = "identity", fill = assists$team_color) +
  geom_image(aes(image = team_logo), size = 0.09, by = "height", nudge_x = 0.05) +
  coord_flip() +
  labs(
    title = "Top 10 Assists",
    subtitle = "2024 Season",
    x = "Player",
    y = "Total Assists",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# ------- SHOOTING CHARTS -------

# inspect play by play data and narrow it down to what is needed
# only 2024 season
shooting <- pbp[pbp$shooting_play == "TRUE" & pbp$season == 2024, ]
# now it is narrowed down to only shooting plays - excludes loose ball fouls,
# tip offs, etc.
# look into only one team - fever - look into home vs away shooting
# narrow down to home
fever_home <- shooting[shooting$home_team_name == "Indiana", ]
# plot fever shooting for 2024
geom_basketball(league = "wnba", color_updates = list(
  plot_background = '#ffffff'
)) + 
  geom_point(data = fever_home,
             aes(coordinate_x, coordinate_y,
                 color = scoring_play),
             size = 2.5, alpha = 0.6) + 
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Made Shot", "Missed Shot")) + 
  theme_minimal() + 
  labs(title = "Indiana Fever Shot Chart Home Games (2024)",
       subtitle = "Shot attempts colored by outcome",
       x = NULL, y = NULL, color = "Shot Outcome") +
  theme(plot.background = element_rect(fill = "white", color = "white"),  
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))
# lets look at fever away games
fever_away <- shooting[shooting$away_team_name == "Indiana", ]
# plotting away games for fever
geom_basketball(league = "wnba", color_updates = list(
  plot_background = '#ffffff'
)) + 
  geom_point(data = fever_away,
             aes(coordinate_x, coordinate_y,
                 color = scoring_play),
             size = 2.5, alpha = 0.6) + 
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Made Shot", "Missed Shot")) + 
  theme_minimal() + 
  labs(title = "Indiana Fever Shot Chart Away Games (2024)",
       subtitle = "Shot attempts colored by outcome",
       x = NULL, y = NULL, color = "Shot Outcome") +
  theme(plot.background = element_rect(fill = "white", color = "white"),  
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# looking into caitlin clark shooting chart
# extract athlete id for clark
clark <- athlete[athlete$athlete_display_name == 'Caitlin Clark', ]
# aggregate to home hames
clark_home <- shooting[shooting$athlete_id_1 == 4433403 & 
                         shooting$home_team_name == "Indiana", ]
# plotting home games
geom_basketball(league = "wnba", color_updates = list(
  plot_background = '#ffffff'
)) + 
  geom_point(data = clark_home,
             aes(coordinate_x, coordinate_y,
                 color = scoring_play),
             size = 2.5, alpha = 0.6) + 
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Made Shot", "Missed Shot")) + 
  theme_minimal() + 
  labs(title = "Caitlin Clark Shot Chart Home Games (2024)",
       subtitle = "Shot attempts colored by outcome",
       x = NULL, y = NULL, color = "Shot Outcome") +
  theme(plot.background = element_rect(fill = "white", color = "white"),  
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))
# aggregate to away games
clark_away <- shooting[shooting$athlete_id_1 == 4433403 & 
                         shooting$away_team_name == "Indiana", ]
# plotting away game shooting chart
geom_basketball(league = "wnba", color_updates = list(
  plot_background = '#ffffff'
)) + 
  geom_point(data = clark_away,
             aes(coordinate_x, coordinate_y,
                 color = scoring_play),
             size = 2.5, alpha = 0.6) + 
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Made Shot", "Missed Shot")) + 
  theme_minimal() + 
  labs(title = "Caitlin Clark Shot Chart Away Games (2024)",
       subtitle = "Shot attempts colored by outcome",
       x = NULL, y = NULL, color = "Shot Outcome") +
  theme(plot.background = element_rect(fill = "white", color = "white"),  
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# looking into aja wilson shooting chart 
# extract aja wilson athlete id
wilson <- athlete[athlete$athlete_display_name == "A'ja Wilson", ]
# aggregate to home games
wilson_home <- shooting[shooting$athlete_id_1 == 3149391 & 
           shooting$home_team_name == "Las Vegas", ]
# plotting home games
geom_basketball(league = "wnba", color_updates = list(
  plot_background = '#ffffff'
)) + 
  geom_point(data = wilson_home,
             aes(coordinate_x, coordinate_y,
                 color = scoring_play),
             size = 2.5, alpha = 0.6) + 
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Made Shot", "Missed Shot")) + 
  theme_minimal() + 
  labs(title = "A'ja Wilson Shot Chart Home Games (2024)",
       subtitle = "Shot attempts colored by outcome",
       x = NULL, y = NULL, color = "Shot Outcome") +
  theme(plot.background = element_rect(fill = "white", color = "white"),  
        panel.grid = element_blank(),  
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

wilson_away <- shooting[shooting$athlete_id_1 == 3149391 & 
           shooting$away_team_name == "Las Vegas", ]
# plotting home games
geom_basketball(league = "wnba", color_updates = list(
  plot_background = '#ffffff'
)) + 
  geom_point(data = wilson_away,
             aes(coordinate_x, coordinate_y,
                 color = scoring_play),
             size = 2.5, alpha = 0.6) + 
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                     labels = c("Made Shot", "Missed Shot")) + 
  theme_minimal() + 
  labs(title = "A'ja Wilson Shot Chart Away Games (2024)",
       subtitle = "Shot attempts colored by outcome",
       x = NULL, y = NULL, color = "Shot Outcome") +
  theme(plot.background = element_rect(fill = "white", color = "white"),  
        panel.grid = element_blank(),  # Remove grid lines
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

