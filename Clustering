library(tidyverse)
library(reactablefmtr)

# Import pitch dataset from your source

pitchers_2023 <- read_csv("blabla.csv")


# Filter for regular season games and add columns for count, is_whiff, is_called_strike etc...

table(pitchers_2023$description)

dataset <- pitchers_2023 %>%
  filter(game_type == "R") %>%
  select(1:92) %>%
  mutate(
    count = as.factor(case_when(
      balls == 0 & strikes == 0 ~ "0-0",
      balls == 1 & strikes == 0 ~ "1-0",
      balls == 2 & strikes == 0 ~ "2-0",
      balls == 3 & strikes == 0 ~ "3-0",
      balls == 0 & strikes == 1 ~ "0-1",
      balls == 0 & strikes == 2 ~ "0-2",
      balls == 1 & strikes == 1 ~ "1-1",
      balls == 2 & strikes == 1 ~ "2-1",
      balls == 3 & strikes == 1 ~ "3-1",
      balls == 3 & strikes == 2 ~ "3-2",
      balls == 2 & strikes == 2 ~ "2-2",
      balls == 1 & strikes == 2 ~ "1-2",
      
    )),
    count_type = case_when(
      count %in% c("0-0", "1-1", "3-2") ~ "Even Count",
      count %in% c("1-0", "2-0", "3-0", "2-1", "3-1") ~ "Batter Ahead",
      count %in% c("0-1", "0-2", "1-2", "2-2") ~ "Pitcher Ahead"
    )
    ,.after = strikes
  ) %>%
  mutate(
    is_called_strike = ifelse(description == "called_strike", 1, 0),
    is_swing = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play", "missed_bunt", "foul_bunt"), 1, 0),
    is_whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip"), 1, 0),
    is_in_zone = ifelse(zone %in% c(1,2,3,4,5,6,7,8,9), 1, 0),
    is_chase = ifelse(is_in_zone == 0 & is_swing == 1, 1, 0),
    is_in_play = ifelse(description == "hit_into_play", 1, 0),
    is_hard_hit = ifelse(is_in_play == 1 & launch_speed >= 95, 1, 0),
    fair_ball_launch_angle = ifelse(is_in_play == 1, launch_angle, NA),
    is_oppo_handed = ifelse(p_throws == "R" & stand == "L" | p_throws == "L" & stand == "R", "YES", "NO")
  )

# Summary pitch usage data by count situation

summary_usage <- dataset %>%
  filter(!is.na(count_type)) %>%
  group_by(player_name, pitcher, pitch_type, count_type) %>%
  summarize(pitch_count = n()) %>%
  ungroup() %>%
  group_by(count_type, pitcher) %>%
  mutate(
    total_pitches = sum(pitch_count),
    usage_rate = round((pitch_count / total_pitches) * 100, digits = 1)
  ) %>%
  select(-total_pitches) %>%
  ungroup() %>%
  filter(!is.na(pitch_type) & pitch_type != "PO") %>%
  pivot_wider(names_from = count_type, values_from = c(usage_rate, pitch_count))


# Summary pitch performance data (including whiff%, zone%, chase%, called strike%, avg launch angle, hard hit%)

summary_performance <- dataset %>%
  group_by(player_name, pitcher, pitch_type) %>%
  summarize(
    n_pitches = n(),
    zone_rate = round(sum(is_in_zone) / n(), digits = 2),
    chase_rate = round(sum(is_chase) / sum(is_in_zone == 0), digits = 2),
    called_strike_rate = round(sum(is_called_strike) / n(), digits = 2),
    whiff_rate = round(sum(is_whiff) / sum(is_swing), digits = 2),
    avg_launch_allowed = round(mean(fair_ball_launch_angle, na.rm = TRUE), digits = 1),
    hard_hit_rate = round(sum(is_hard_hit, na.rm = TRUE) / sum(is_in_play, na.rm = TRUE), digits = 2)
  ) %>%
  filter(n_pitches >= 200 & !is.na(pitch_type)) %>%
  ungroup()

# Platoon split (measured using called strike + whiff rate)

summary_platoon <- dataset %>%
  group_by(player_name, pitcher, pitch_type, is_oppo_handed) %>%
  summarize(
    CSW = round((sum(is_whiff) + sum(is_called_strike)) / n(), digits = 2)
    ) %>%
  ungroup() %>%
  pivot_wider(names_from = is_oppo_handed, names_prefix = "is_oppo_handed_CSW_", values_from = CSW) %>%
  mutate(
    CSW_platoon_diff = is_oppo_handed_CSW_NO - is_oppo_handed_CSW_YES, .before = is_oppo_handed_CSW_NO
  )

# Join all summary data into one df

summary_all <- left_join(summary_performance, summary_usage, by = c("player_name", "pitcher", "pitch_type"))

summary_all <- left_join(summary_all, summary_platoon, by = c("player_name", "pitcher", "pitch_type"))

summary_all <- summary_all %>%
  relocate(CSW_platoon_diff, .before = `pitch_count_Batter Ahead`) %>%
  drop_na(5:14)

# Scale columns for clustering

km_scaled_cols <- scale(summary_all[,5:14])

# KM

n_clusters <- 10

wss <- numeric(n_clusters)

set.seed(280)

# Loop over clusters
for (i in 1:n_clusters) {
  km.out <- kmeans(km_scaled_cols, centers = i, nstart = 20)
  # Save wss
  wss[i] <- km.out$tot.withinss
}

# Scree plot

wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

 ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
   geom_hline(yintercept = wss, linetype = 'dashed') +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
  
# Selected 7 clusters. Now run k-means with 7 centers

set.seed(280)

km.out <- kmeans(km_scaled_cols, centers = 7, nstart = 20)

summary_all$cluster <- factor(km.out$cluster)

# Move cluster to front

summary_all <- summary_all %>%
  relocate(cluster, .after = pitch_type)

# Summarize cluster values for all 10 characteristics

cluster_summary <- summary_all %>%
  group_by(cluster) %>%
  summarize(
    n_unique_pitches = n(),
    zone_rate = round(mean(zone_rate), digits = 2),
    chase_rate = round(mean(chase_rate), digits = 2),
    called_strike_rate = round(mean(called_strike_rate), digits = 2),
    whiff_rate = round(mean(whiff_rate), digits = 2),
    avg_launch = round(mean(avg_launch_allowed), digits = 1),
    hard_hit = round(mean(hard_hit_rate), digits = 2),
    `Batter Ahead Usage` = round(mean(`usage_rate_Batter Ahead`), digits = 0),
    `Even Count Usage`= round(mean(`usage_rate_Even Count`), digits = 0),
    `Pitcher Ahead Usage`= round(mean(`usage_rate_Pitcher Ahead`), digits = 0),
    CSW_platoon_diff = round(mean(CSW_platoon_diff), digits = 2)
  )

# Name clusters and join back to pitch by pitch datset

summary_all <- summary_all %>%
  mutate(
    cluster_name = case_when(
      cluster == 1 ~ "Low Usage In-Zone Pitch",
      cluster == 2 ~ "Wormkiller",
      cluster == 3 ~ "Run of the Mill Primary",
      cluster == 4 ~ "Plus Primary",
      cluster == 5 ~ "2 Strike Whiffer",
      cluster == 6 ~ "Versatile Secondary",
      cluster == 7 ~ "Elite Secondary"
    ),
    .after = cluster
  )

pitch_clusters <- summary_all %>%
  select(pitcher, pitch_type, cluster, cluster_name)

dataset_with_clusters <- left_join(pitch_clusters, dataset)

# Run Value for Clusters (overall and by count)

cluster_run_values <- dataset_with_clusters %>%
  group_by(cluster_name) %>%
  summarize(
    n_pitches = n(),
    avg_run_value_100 = round(mean(delta_run_exp, na.rm = TRUE) * 100, digits = 1)
  ) %>%
  ungroup()

cluster_run_values_by_count_type <- dataset_with_clusters %>%
  group_by(cluster_name, count_type) %>%
  summarize(
    n_pitches = n(),
    avg_run_value_100 = round(mean(delta_run_exp, na.rm = TRUE) * 100, digits = 1)
  ) %>%
  drop_na() %>%
  ungroup()

# Common clusters as primary, secondary, tertiary pitches.

most_common_clusters <- dataset_with_clusters %>%
  group_by(player_name, pitcher, pitch_name, cluster_name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(player_name, pitcher) %>%
  mutate(rank = rank(-count, ties.method = "first")) %>%
  filter(rank <= 3) %>%
  select(-count, -pitch_name) %>%
  arrange(player_name, pitcher, rank) %>%
  pivot_wider(names_from = rank, values_from = cluster_name, names_prefix = "Top") 


table(most_common_clusters$Top1) # Most Common Primary Pitches
table(most_common_clusters$Top2) # Most Common Secondary Pitches
table(most_common_clusters$Top3) # Most Common Third Pitches


# Common Arsenals (with at least 2 pitches)

arsenals <- most_common_clusters %>%
  group_by(Top1, Top2, Top3) %>%
  summarise(count = n()) %>%
  filter(!is.na(Top2)) %>%
  arrange(desc(count)) %>%
  ungroup()

# Common First 2 Pitch Mixes

two_pitch_arsenals <- most_common_clusters %>%
  group_by(Top1, Top2) %>%
  summarize(`# of Arsenals` = n(),
            `% of Arsenals` = round((`# of Arsenals` / 368) * 100, digits = 1)) %>%
  filter(!is.na(Top2)) %>%
  arrange(desc(`# of Arsenals`)) %>%
  ungroup()

# Common Secondary Pitches for Wormkiller

wormkiller_secondary <- most_common_clusters %>%
  filter(Top1 == "Wormkiller") %>%
  group_by(Top2) %>%
  summarize(`# of Arsenals` = n(),
            `% of Arsenals` = round((`# of Arsenals` / 56) * 100, digits = 1)) %>%
  filter(!is.na(Top2)) %>%
  arrange(desc(`# of Arsenals`)) %>%
  ungroup()

# Common Secondary Pitches for Run of The Mill Primary

rotm_secondary <- most_common_clusters %>%
  filter(Top1 == "Run of the Mill Primary") %>%
  group_by(Top2) %>%
  summarize(`# of Arsenals` = n(),
            `% of Arsenals` = round((`# of Arsenals` / 105) * 100, digits = 1)) %>%
  filter(!is.na(Top2)) %>%
  arrange(desc(`# of Arsenals`)) %>%
  ungroup()

# Common Secondary Pitches for Plus Primary

plusprimary_secondary <- most_common_clusters %>%
  filter(Top1 == "Plus Primary") %>%
  group_by(Top2) %>%
  summarize(`# of Arsenals` = n(),
            `% of Arsenals` = round((`# of Arsenals` / 68) * 100, digits = 1)) %>%
  filter(!is.na(Top2)) %>%
  arrange(desc(`# of Arsenals`)) %>%
  ungroup()


# Best Pitches

best_pitches <- dataset_with_clusters %>%
  group_by(player_name, pitcher, pitch_type, cluster) %>%
  summarize(
    n_pitches = n(),
    run_value = round(sum(delta_run_exp, na.rm = TRUE), digits = 1),
    run_value_100 = round(mean(delta_run_exp, na.rm = TRUE) * 100, digits = 2)
  )

# By Cluster Pitch Types

cluster_1 <- summary_all %>%
  filter(cluster == 1)

table(cluster_1$pitch_type) # Low Usage In-Zone Pitch is typically a sinker. Sometimes also a cutter, curve or four-seam

cluster_2 <- summary_all %>%
  filter(cluster == 2)

table(cluster_2$pitch_type) # Wormkiller is almost always a sinker. Occasionally also a four-seam or cutter

cluster_3 <- summary_all %>%
  filter(cluster == 3)

table(cluster_3$pitch_type) # Run of the mill primary is almost always a four-seamer.

cluster_4 <- summary_all %>%
  filter(cluster == 4)

table(cluster_4$pitch_type) # Plus primary is generally a four-seam.

cluster_5 <- summary_all %>%
  filter(cluster == 5)

table(cluster_5$pitch_type) # 2 strike secondary is a hodgepodge of pitches. Most often a changeup.

cluster_6 <- summary_all %>%
  filter(cluster == 6)

table(cluster_6$pitch_type) # Versatile secondary is also a hodgepodge. Most often a cutter or slider.

cluster_7 <- summary_all %>%
  filter(cluster == 7)

table(cluster_7$pitch_type) # Elite secondary is usually a slider of some kind.
