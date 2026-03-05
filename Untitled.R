library(tidyverse)
library(readxl)
library(dplyr)
library(waffle)
library(scales)

df_resp <- read.table("~/Desktop/CJS/0126data_studio/project-2-atus/data/atusresp_2024.dat", sep = ",", header = TRUE)
df_act <- read.table("~/Desktop/CJS/0126data_studio/project-2-atus/data/atusact_2024.dat", sep = ",", header = TRUE)

class(df_act$TUCASEID)
df_act$TUCASEID <- as.character(df_act$TUCASEID)
class(df_act$TUCASEID)

class(df_resp$TUCASEID)
df_resp$TUCASEID <- as.character(df_resp$TUCASEID)
class(df_resp$TUCASEID)

df_act <- df_act %>%
  left_join(df_resp %>% select(TUCASEID, TUFINLWGT), by = "TUCASEID") %>%
  rename(weight = TUFINLWGT)

sum(df_resp$TUFINLWGT)

df_simpler <- df_resp %>%
  select(TUCASEID, TUFINLWGT)


class(df_act$TUSTARTTIM)
head(df_act$TUSTARTTIM)

class(df_act$TUSTOPTIME)
head(df_act$TUSTOPTIME)

df_act$start_hour <- as.numeric(substr(df_act$TUSTARTTIM, 1, 2))
df_act$start_minute <- as.numeric(substr(df_act$TUSTARTTIM, 4, 5))
df_act$start_second <- as.numeric(substr(df_act$TUSTARTTIM, 7, 8))

df_act$end_hour <- as.numeric(substr(df_act$TUSTOPTIME, 1, 2))
df_act$end_minute <- as.numeric(substr(df_act$TUSTOPTIME, 4, 5))
df_act$end_second <- as.numeric(substr(df_act$TUSTOPTIME, 7, 8))

df_act <- df_act %>%
  mutate(start_since_midnight = (start_hour*60)+(start_minute)+(start_second/60))

df_act <- df_act %>%
  mutate(end_since_midnight = (end_hour*60)+(end_minute)+(end_second/60))


bin_length <- 30
bin_starts <- seq(0, 1440 - bin_length, by = bin_length)
bins <- data.frame(
  bin_id = seq_along(bin_starts),
  bin_start = bin_starts,
  bin_end = bin_starts + bin_length
)

bins <- bins %>%
  mutate(
    bin_start_time = sprintf("%02d:%02d %s",
                             (bin_start %/% 60) %% 12 %>% {ifelse(. == 0, 12, .)},
                             bin_start %% 60,
                             ifelse(bin_start < 720, "AM", "PM")),
    bin_end_time = sprintf("%02d:%02d %s",
                           (bin_end %/% 60) %% 12 %>% {ifelse(. == 0, 12, .)},
                           bin_end %% 60,
                           ifelse(bin_end < 720, "AM", "PM"))
  )

activity_bins <- tidyr::crossing(df_act, bins)

activity_bins <- activity_bins %>%
  mutate(
    overlap = pmax(0, pmin(end_since_midnight, bin_end) - pmax(start_since_midnight, bin_start))
  ) %>%
  filter(overlap > 0)


bin_assign <- activity_bins %>%
  group_by(TUCASEID, bin_id) %>%
  slice_max(overlap, n = 1, with_ties = FALSE) %>%
  ungroup()

bin_percent <- bin_assign %>%
  group_by(bin_id, TRCODE) %>%
  summarise(weighted_count = sum(weight), .groups = "drop") %>%
  group_by(bin_id) %>%
  mutate(percent = 100 * weighted_count / sum(weighted_count)) %>%
  ungroup()

bin_percent <- bin_percent %>%
  left_join(bins %>% select(bin_id, bin_start_time, bin_end_time), by = "bin_id")

activity_codes <- read_excel("~/Desktop/CJS/0126data_studio/project-2-atus/lexiconwex2024.xls")

activity_codes <- activity_codes %>%
  mutate(
    TRCODE = sprintf("%06d", as.integer(code))
  ) %>%
  select(TRCODE, activity)

bin_percent <- bin_percent %>%
  mutate(
    TRCODE = sprintf("%06d", as.integer(TRCODE))
  )

bin_percent <- bin_percent %>%
  left_join(activity_codes, by = "TRCODE")


bin_percent_wide <- bin_percent %>%
  select(bin_id, activity, percent) %>%
  pivot_wider(names_from = activity, values_from = percent, values_fill = 0)

bin_percent_wide <- bin_percent_wide %>%
  left_join(bins %>% select(bin_id, bin_start_time, bin_end_time), by = "bin_id") %>%
  select(bin_id, bin_start_time, bin_end_time, everything())

top5_per_bin <- bin_percent %>%
  group_by(bin_id) %>%
  slice_max(order_by = percent, n = 5) %>%
  arrange(bin_id, desc(percent)) %>%
  ungroup()

top5_wide <- top5_per_bin %>%
  group_by(bin_id) %>%
  mutate(rank = row_number(desc(percent))) %>%
  ungroup() %>%
  select(bin_id, rank, activity, percent) %>%
  pivot_wider(
    names_from = rank,
    values_from = c(activity, percent),
    names_sep = "_"
  )


top5_wide <- top5_wide %>%
  left_join(
    bins %>% select(bin_id, bin_start_time, bin_end_time),
    by = "bin_id"
  )

top5_wide <- top5_wide %>%
  select(bin_id, bin_start_time, bin_end_time, 
         activity_1, percent_1, activity_2, percent_2,
         activity_3, percent_3, activity_4, percent_4,
         activity_5, percent_5)


bin_percent %>%
  filter(activity == "Sleeping") %>%
  ggplot() +
  aes(x=bin_id, y=percent) +
  geom_col()


bin_percent %>%
  filter(bin_id == 48) %>%
  arrange(desc(percent))

bin_percent %>%
  select(-TRCODE, -weighted_count, -bin_start_time, -bin_end_time)

bin_ids <- c(1:48)

for (id in bin_ids) {
  bin_data <- bin_percent %>%
    filter(bin_id == id) %>%
    arrange(desc(percent))
  
  top5 <- bin_data %>%
    slice_max(percent, n=5)
  
  individual_bin_data <- bins %>%
    filter(bin_id == id)
  
  individual_start_time <- individual_bin_data$bin_start_time
  
  other <- bin_data %>%
    slice(-(1:5)) %>%
    summarize(
      activity = "Other",
      percent=sum(percent))
  
  waffle_data <- bind_rows(top5, other)
  
  waffle_data$bin_id = id

  waffle_data <- waffle_data %>%
    mutate(
      floor_val = floor(percent),
      remainder = percent - floor_val
    )
  
  leftover <- 100 - sum(waffle_data$floor_val)
  
  waffle_data <- waffle_data %>%
    arrange(desc(remainder)) %>%
    mutate(
      pct_round = floor_val + ifelse(row_number() <= leftover, 1, 0)
    ) %>%
    arrange(desc(percent)) %>%
    select(-floor_val, -remainder)
  
  waffle_data <- waffle_data %>%
    filter(activity != "Other") %>%
    bind_rows(waffle_data %>% filter(activity == "Other"))
  
  waffle_vec <- setNames(waffle_data$pct_round, waffle_data$activity)
  
  waffle_data <- waffle_data %>%
    mutate(
      activity_group = case_when(
        activity %in% c("Work, main job", "Travel related to working") ~ "Work-related",
        activity %in% c("Television and movies (not religious)",
                        "Socializing and communicating with others", "Playing games",
                        "Reading for personal interest",
                        "Computer use for leisure (exc. Games)") ~ "Leisure",
        activity %in% c("Sleeping", "Sleeplessness") ~ "Sleep",
        activity %in% c("Other") ~ "Other",
        activity %in% c("Eating and drinking", "Food and drink preparation") ~ "Eating and drinking",
        activity %in% c("Interior cleaning") ~ "Interior cleaning",
        activity %in% c("Washing, dressing and grooming oneself") ~ "Washing and dressing oneself",
        TRUE ~ "Other"
      )
    )
  
  waffle_data <- waffle_data %>%
    group_by(activity_group) %>%
    mutate(group_total = sum(pct_round)) %>%
    ungroup()
  
  waffle_data <- waffle_data %>%
    arrange(
      activity_group == "Other",
      desc(group_total),
      desc(pct_round)
    )
  
  time_label <- unique(bin_data$bin_start_time)
  
  p <- ggplot(waffle_data, aes(fill = activity_group, values = pct_round)) +
    geom_waffle(n_rows = 10, flip=T, size = 0.3, color = "white") +
    scale_fill_manual(values = c(
      "Other" = "grey70", 
      "Sleep" = "#1f77b4",
      "Leisure" = "yellow2",
      "Washing and dressing oneself" = "green3",
      "Work-related" = "red3",
      "Eating and drinking" = "purple2",
      "Interior cleaning" = "pink2"
    )) +
    coord_equal() +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none")
  
  ggsave(paste0("~/Desktop/CJS/0126data_studio/project-2-atus/waffle plots/waffle_bin_", id, "_", individual_start_time, ".png"), plot=p, width = 6, height = 6, units = "in") 
}



for (id in bin_ids) {
  bin_data <- bin_percent %>%
    filter(bin_id == id) %>%
    arrange(desc(percent))
  
  top5 <- bin_data %>%
    slice_max(percent, n=5)
  
  individual_bin_data <- bins %>%
    filter(bin_id == id)
  
  individual_start_time <- individual_bin_data$bin_start_time
  
  other <- bin_data %>%
    slice(-(1:5)) %>%
    summarize(
      activity = "Other",
      percent=sum(percent))
  
  waffle_data <- bind_rows(top5, other)
  
  waffle_data$bin_id = id
  
  waffle_data <- waffle_data %>%
    mutate(
      floor_val = floor(percent),
      remainder = percent - floor_val
    )
  
  leftover <- 100 - sum(waffle_data$floor_val)
  
  waffle_data <- waffle_data %>%
    arrange(desc(remainder)) %>%
    mutate(
      pct_round = floor_val + ifelse(row_number() <= leftover, 1, 0)
    ) %>%
    arrange(desc(percent)) %>%
    select(-floor_val, -remainder)
  
  waffle_data <- waffle_data %>%
    filter(activity != "Other") %>%
    bind_rows(waffle_data %>% filter(activity == "Other"))
  
  waffle_vec <- setNames(waffle_data$pct_round, waffle_data$activity)
  
  waffle_data <- waffle_data %>%
    mutate(
      activity_group = case_when(
        activity %in% c("Work, main job", "Travel related to working") ~ "Work-related",
        activity %in% c("Television and movies (not religious)",
                        "Socializing and communicating with others", "Playing games",
                        "Reading for personal interest",
                        "Computer use for leisure (exc. Games)") ~ "Leisure",
        activity %in% c("Sleeping", "Sleeplessness") ~ "Sleep",
        activity %in% c("Other") ~ "Other",
        activity %in% c("Eating and drinking", "Food and drink preparation") ~ "Eating and drinking",
        activity %in% c("Interior cleaning") ~ "Interior cleaning",
        activity %in% c("Washing, dressing and grooming oneself") ~ "Washing and dressing oneself",
        TRUE ~ "Other"
      )
    )
  
  waffle_data <- waffle_data %>%
    group_by(activity_group) %>%
    mutate(group_total = sum(pct_round)) %>%
    ungroup()
  
  waffle_data <- waffle_data %>%
    arrange(
      activity_group == "Other",
      desc(group_total),
      desc(pct_round)
    )
  
  bins_to_plot <- c("12:00 AM", "5:00 AM", "7:00 AM", "9:30 AM",
                    "12:00 PM", "5:00 PM", "7:30 PM", "10:00 PM", "12:00 AM")
  
  waffle_subset <- waffle_data %>% 
    filter(bin_start_time %in% bins_to_plot)
  
  time_label <- unique(bin_data$bin_start_time)
  
  p <- ggplot(waffle_data, aes(fill = activity_group, values = pct_round)) +
    geom_waffle(n_rows = 10, flip=T, size = 0.3, color = "white") +
    scale_fill_manual(values = c(
      "Other" = "grey70", 
      "Sleep" = "#1f77b4",
      "Leisure" = "yellow2",
      "Washing and dressing oneself" = "green3",
      "Work-related" = "red3",
      "Eating and drinking" = "purple2",
      "Interior cleaning" = "pink2"
    )) +
    coord_equal() +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none")
  
  ggsave(paste0("~/Desktop/CJS/0126data_studio/project-2-atus/waffle plots/waffle_bin_", id, "_", individual_start_time, ".png"), plot=p, width = 6, height = 6, units = "in") 
}

for_looking <- top5_wide %>%
  select(bin_start_time, activity_1, percent_1)

for_looking %>%
  ggplot() +
  aes(x=bin_start_time, y=percent_1, color=activity_1) +
  geom_point()
