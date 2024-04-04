library(tidyverse)
library(reactablefmtr)

# Cluster Summary Table

cluster_summary_table <- summary_all %>%
  group_by(cluster_name, cluster) %>%
  summarize(
    `Pitches` = n(),
    `Zone%` = round(mean(zone_rate) * 100, digits = 0),
    `Chase%` = round(mean(chase_rate) * 100, digits = 0),
    `Called Strike%` = round(mean(called_strike_rate) * 100, digits = 0),
    `Whiff%` = round(mean(whiff_rate) * 100, digits = 0),
    `Average LA` = round(mean(avg_launch_allowed), digits = 1),
    `Hard Hit%` = round(mean(hard_hit_rate) * 100, digits = 0),
    `Batter Ahead Usage%` = round(mean(`usage_rate_Batter Ahead`), digits = 0),
    `Even Count Usage%`= round(mean(`usage_rate_Even Count`), digits = 0),
    `Pitcher Ahead Usage%`= round(mean(`usage_rate_Pitcher Ahead`), digits = 0),
    `CSW% Platoon Split` = round(mean(CSW_platoon_diff) * 100, digits = 0)
  ) %>%
  arrange(cluster) %>%
  select(-cluster)

cluster_summary_table %>%
reactable(theme = fivethirtyeight(centered = TRUE, header_font_size = 10, cell_padding = 4),
          columns = list(
            cluster_name = colDef(width = 160),
            `Pitches` = colDef(width = 55),
            `Zone%` = colDef(width = 50, cell = color_tiles(., box_shadow = TRUE)),
            `Chase%` = colDef(width = 55, cell = color_tiles(., box_shadow = TRUE)),
            `Called Strike%` = colDef(width = 100, cell = color_tiles(., box_shadow = TRUE)),
            `Whiff%` = colDef(width = 55, cell = color_tiles(., box_shadow = TRUE)),
            `Average LA` = colDef(width = 75, cell = color_tiles(., box_shadow = TRUE)),
            `Hard Hit%` = colDef(width = 70, cell = color_tiles(., box_shadow = TRUE)),
            `Batter Ahead Usage%` = colDef(width = 140, cell = color_tiles(., box_shadow = TRUE)),
            `Even Count Usage%` = colDef(width = 130, cell = color_tiles(., box_shadow = TRUE)),
            `Pitcher Ahead Usage%` = colDef(width = 140, cell = color_tiles(., box_shadow = TRUE)),
            `CSW% Platoon Split` = colDef(width = 130, cell = color_tiles(., box_shadow = TRUE))
          )) %>%
  add_title(
    title = "Pitch Cluster Characteristics", font_size = 25, align = "center"
  ) 

# Pitch Types in each Cluster

pitch_types_in_cluster <- summary_all %>%
  group_by(cluster_name, cluster, pitch_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(cluster) %>%
  select(-cluster) %>%
  pivot_wider(names_from = pitch_type, values_from = count, values_fill = list(count = 0))


pitch_types_in_cluster %>%
  rename(Cluster = cluster_name) %>%
  reactable(theme = fivethirtyeight(centered = TRUE, header_font_size = 10, cell_padding = 4),
            columns = list(
              Cluster = colDef(width = 180),
              CH = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              CU = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              FC = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              FF = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              FO = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              FS = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              KC = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              SL = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              ST = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              SI = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE)),
              SV = colDef(width = 30, cell = color_tiles(., box_shadow = TRUE))
            )) %>%
  add_title(
    title = "Pitch Types by Cluster", font_size = 20, align = "center"
  ) 



# Cluster 1 Table

cluster_1 %>%
  arrange(desc(n_pitches)) %>%
  select(player_name, pitch_type) %>%
    head(10) %>%
    rename(Pitcher = player_name, `Pitch Type` = pitch_type) %>%
    reactable(theme = fivethirtyeight(header_font_size = 10, cell_padding = 2),
              columns = list(
                Pitcher = colDef(width = 120),
                `Pitch Type` = colDef(width = 70)
              )) %>%
    add_title(
      title = "Low Usage In-Zone Pitch", font_size = 15, align = "left"
    ) 


# Cluster Run Values by Count Type

cluster_run_values_by_count_type %>%
  rename(Cluster = cluster_name, `Count Type` = count_type, `# of Pitches` = n_pitches, `RV per 100` = avg_run_value_100) %>%
  reactable(theme = fivethirtyeight(centered = TRUE, header_font_size = 10, cell_padding = 2),
            columns = list(
              Cluster = colDef(width = 180),
              `Count Type` = colDef(width = 100),
              `# of Pitches` = colDef(width = 80),
              `RV per 100` = colDef(width = 70, cell = color_tiles(., box_shadow = TRUE))
            ), defaultPageSize = 25) %>%
  add_title(
    title = "Cluster Performance by Count Situation", font_size = 20, align = "center"
  ) 


# Arsenals

two_pitch_arsenals %>%
slice(c(1:10, 26:35)) %>%
  rename(`Primary Pitch` = Top1, `Secondary Pitch` = Top2) %>%
  reactable(theme = fivethirtyeight(centered = TRUE, header_font_size = 10, cell_padding = 4),
            columns = list(
              `Primary Pitch` = colDef(width = 170),
              `Secondary Pitch` = colDef(width = 170),
              `# of Arsenals` = colDef(width = 90, cell = color_tiles(., box_shadow = TRUE)),
              `% of Arsenals` = colDef(width = 90, cell = color_tiles(., box_shadow = TRUE))
            ), defaultPageSize = 25) %>%
  add_title(
    title = "Most and Least Common Arsenals", font_size = 20, align = "center"
  ) 

# Wormkiller Secondary

wormkiller_secondary %>%
  rename(`Secondary Pitch` = Top2) %>%
  reactable(theme = fivethirtyeight(centered = TRUE, header_font_size = 10, cell_padding = 4),
            columns = list(
              `Secondary Pitch` = colDef(width = 170),
              `# of Arsenals` = colDef(width = 90, cell = color_tiles(., box_shadow = TRUE)),
              `% of Arsenals` = colDef(width = 90, cell = color_tiles(., box_shadow = TRUE))
            )) %>%
  add_title(
    title = "Wormkiller Secondary Offerings", font_size = 20, align = "center"
  ) 

# ROTM Secondary

rotm_secondary %>%
  rename(`Secondary Pitch` = Top2) %>%
  reactable(theme = fivethirtyeight(centered = TRUE, header_font_size = 10, cell_padding = 4),
            columns = list(
              `Secondary Pitch` = colDef(width = 170),
              `# of Arsenals` = colDef(width = 90, cell = color_tiles(., box_shadow = TRUE)),
              `% of Arsenals` = colDef(width = 90, cell = color_tiles(., box_shadow = TRUE))
            )) %>%
  add_title(
    title = "Secondary Offerings for a Run of the Mill Primary", font_size = 20, align = "center"
  ) 
