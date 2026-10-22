library(tidyverse)
library(baseballr)
library(readr)
library(dplyr)
library(tidyr)
library(xml2)
library(purrr)
library(xgboost)
library(ggplot2)
 
boxscores <- list.files("~/Desktop/Normal Cornbelters Research Projects/Baseball Summer 2026-xml-20260814-0133",
                        pattern = "\\.xml$", full.names = TRUE)

parse_runvalue_pbp <- function(file) {
  doc <- read_xml(file)
  
  venue <- xml_find_first(doc, "//venue")
  game_date <- xml_attr(venue, "date")
  
  play_nodes <- xml_find_all(doc, "//inning/batting/play")
  
  map_dfr(play_nodes, function(p){
    inning_num <- xml_attr(xml_parent(xml_parent(p)), "number")
    vh <- xml_attr(xml_parent(p),"vh")
    
    batter_node <- xml_find_first(p, "batter")
    runners <- xml_find_all(p, "runner")
    
    runner_scores <- sum(xml_attr(runners,"scored") == "1", na.rm = TRUE)
    batter_scored <- ifelse(xml_attr(batter_node, "tobase") == "4", 1, 0)
    runs_on_play <- runner_scores + batter_scored
    
    batter_out <- ifelse(xml_attr(batter_node, "out") == "1" , 1, 0)
    runner_outs <- sum(xml_attr(runners, "out") == "1", na.rm = TRUE)
    outs_on_play <- batter_out + runner_outs
    
    data.frame(
      game_date = game_date,
      inning = inning_num,
      side = vh,
      seq = as.integer(xml_attr(p,"seq")),
      batter = xml_attr(p, "batter"),
      pitcher = xml_attr(p, "pitcher"),
      outs_before = as.integer(xml_attr(p, "outs")),
      on_1b_before = xml_attr(p, "first"),
      on_2b_before = xml_attr(p, "second"),
      on_3b_before = xml_attr(p, "third"),
      action = xml_attr(batter_node, "action"),
      outs_on_play = outs_on_play,
      runs_on_play = runs_on_play,
      pitch_count = xml_attr(xml_find_first(p, "pitches"), "text"),
      stringsAsFactors = FALSE
    )
  })
}

all_pbp <- map_dfr(boxscores, function(f) {
  parse_runvalue_pbp(f) %>% mutate(source_file = basename(f))
})


all_pbp_re <- all_pbp %>%
  mutate(
    RE_before = case_when(
      is.na(on_1b_before) & is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 0 ~ 0.461,
      is.na(on_1b_before) & is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 1 ~ 0.243,
      is.na(on_1b_before) & is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 2 ~ 0.095,
      
      !is.na(on_1b_before) & is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 0 ~ 0.831,
      !is.na(on_1b_before) & is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 1 ~ 0.489,
      !is.na(on_1b_before) & is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 2 ~ 0.214,
      
      is.na(on_1b_before) & !is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 0 ~ 1.068,
      is.na(on_1b_before) & !is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 1 ~ 0.644,
      is.na(on_1b_before) & !is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 2 ~ 0.305,
      
      !is.na(on_1b_before) & !is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 0 ~ 1.373,
      !is.na(on_1b_before) & !is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 1 ~ 0.908,
      !is.na(on_1b_before) & !is.na(on_2b_before) & is.na(on_3b_before) & outs_before == 2 ~ 0.343,
      
      is.na(on_1b_before) & is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 0 ~ 1.426,
      is.na(on_1b_before) & is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 1 ~ 0.865,
      is.na(on_1b_before) & is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 2 ~ 0.413,
      
      !is.na(on_1b_before) & is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 0 ~ 1.798,
      !is.na(on_1b_before) & is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 1 ~ 1.140,
      !is.na(on_1b_before) & is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 2 ~ 0.471,
      
      is.na(on_1b_before) & !is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 0 ~ 1.920,
      is.na(on_1b_before) & !is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 1 ~ 1.352,
      is.na(on_1b_before) & !is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 2 ~ 0.570,
      
      !is.na(on_1b_before) & !is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 0 ~ 2.282,
      !is.na(on_1b_before) & !is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 1 ~ 1.520,
      !is.na(on_1b_before) & !is.na(on_2b_before) & !is.na(on_3b_before) & outs_before == 2 ~ 0.736
    )
  )

all_pbp_re <- all_pbp_re %>%
  arrange(source_file, inning, side, seq) %>%
  group_by(source_file, inning, side) %>%
  mutate(
    outs_after = outs_before + outs_on_play,
    on_1b_after = lead(on_1b_before),
    on_2b_after = lead(on_2b_before),
    on_3b_after = lead(on_3b_before),
    is_last_play = row_number() == n()
  ) %>%
  mutate(side = case_when(
    str_to_lower(side) == "h" ~ "Bottom",
    str_to_lower(side) == "v" ~ "Top",
    TRUE ~ side
  ))

all_pbp_re <- all_pbp_re %>%
  mutate(
    RE_after = case_when(
      outs_after >= 3 ~ 0,
      is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 0 ~ 0.461,
    
      is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 0 ~ 0.461,
      is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 1 ~ 0.243,
      is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 2 ~ 0.095,
      
      !is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 0 ~ 0.831,
      !is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 1 ~ 0.489,
      !is.na(on_1b_after) & is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 2 ~ 0.214,
      
      is.na(on_1b_after) & !is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 0 ~ 1.068,
      is.na(on_1b_after) & !is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 1 ~ 0.644,
      is.na(on_1b_after) & !is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 2 ~ 0.305,
      
      !is.na(on_1b_after) & !is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 0 ~ 1.373,
      !is.na(on_1b_after) & !is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 1 ~ 0.908,
      !is.na(on_1b_after) & !is.na(on_2b_after) & is.na(on_3b_after) & outs_after == 2 ~ 0.343,
      
      is.na(on_1b_after) & is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 0 ~ 1.426,
      is.na(on_1b_after) & is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 1 ~ 0.865,
      is.na(on_1b_after) & is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 2 ~ 0.413,
      
      !is.na(on_1b_after) & is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 0 ~ 1.798,
      !is.na(on_1b_after) & is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 1 ~ 1.140,
      !is.na(on_1b_after) & is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 2 ~ 0.471,
      
      is.na(on_1b_after) & !is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 0 ~ 1.920,
      is.na(on_1b_after) & !is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 1 ~ 1.352,
      is.na(on_1b_after) & !is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 2 ~ 0.570,
      
      !is.na(on_1b_after) & !is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 0 ~ 2.282,
      !is.na(on_1b_after) & !is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 1 ~ 1.520,
      !is.na(on_1b_after) & !is.na(on_2b_after) & !is.na(on_3b_after) & outs_after == 2 ~ 0.736
    )
  )
all_pbp_re <- all_pbp_re %>%
  rename(
    Inning = inning,
    Batter = batter,
    Pitcher = pitcher,
    `Top/Bottom` = side,
    Date = game_date
  ) %>%
  mutate(
    run_expectancy = RE_after + runs_on_play - RE_before
  ) %>%
  mutate(Inning = as.integer(Inning))

all_pbp_expanded <- all_pbp_re %>%
  mutate(Date = mdy(Date)) %>%
  mutate(pitch_count = as.character(pitch_count)) %>%
  mutate(pitch_code = strsplit(pitch_count, "")) %>%
  unnest(pitch_code) %>%
  group_by(source_file, Inning, `Top/Bottom`, seq) %>% 
  mutate(pitch_num = row_number()) %>%
  ungroup() 
#View(all_pbp_expanded)
names(all_pbp)
write.csv(all_pbp_expanded, "expanded_data.csv", row.names = FALSE)

# Adding Trackman Data in

trackman <- read_csv("combined_baseball_data_2026.csv") %>%
  separate(Pitcher, into = c("last", "first"), sep = ", ") %>%
  mutate(Pitcher = paste(first, last)) %>%
  select(-first, -last)


trackman <- trackman %>%
  separate(Batter, into = c("last", "first"), sep = ", ") %>%
  mutate(Batter = paste(first, last)) %>%
  select(-first, -last)%>%
  select(Date, Pitcher, Batter, Inning, `Top/Bottom`, TaggedPitchType, SpinRate, RelSpeed, 
         InducedVertBreak, HorzBreak, PitchCall, PAofInning, PitchofPA,
         Balls, Strikes, PitchCall, PitcherThrows, SpinAxis, RelHeight)
trackman <- trackman %>%
  arrange(Date, Pitcher, Batter, Inning, `Top/Bottom`, PAofInning, PitchofPA) %>%
  group_by(Date, Pitcher, Batter, Inning, `Top/Bottom`, PAofInning) %>%
  mutate(
    is_terminal = case_when(
      PitchCall == "InPlay" ~ TRUE,
      PitchCall == "BallCalled" & Balls == 3 ~ TRUE,
      PitchCall %in% c("StrikeCalled","SwingingStrike") & Strikes == 2 ~ TRUE,
      TRUE ~ FALSE
    ),
    balls_after = case_when(
      PitchCall %in% c("StrikeCalled", "SwingingStrike") ~ Strikes + 1,
      PitchCall %in% c("FoulBallNot Fieldable", "FoulBallFieldable") & Strikes < 2 ~ Strikes + 1,
      TRUE ~ Strikes
    ),
      TaggedPitchType = case_when(
        TaggedPitchType %in% c("FourSeamFastBall", "Fastball") ~ "Fastball",
        TaggedPitchType %in% c("TwoSeamFastBall", "Sinker") ~ "Sinker",
        TRUE ~ TaggedPitchType
    )
  ) %>% 
  ungroup()
trackman %>%
  mutate(
    TaggedPitchType = case_when(
      TaggedPitchType %in% c("FourSeamFastBall", "Fastball") ~ "Fastball",
      TaggedPitchType %in% c("TwoSeamFastBall", "Sinker") ~ "Sinker",
      TRUE ~ TaggedPitchType
    )
  ) %>%
  group_by(TaggedPitchType) %>%
  summarize(
    n = n(),
    avg_velo = mean(RelSpeed, na.rm = TRUE),
    avg_ivb = mean(InducedVertBreak, na.rm = TRUE),
    avg_hb = mean(HorzBreak, na.rm = TRUE)
  )
trackman <- trackman %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
#View(trackman)


joined_data <- trackman %>%
  left_join(
    all_pbp_expanded %>% select(Date, Pitcher, Batter, Inning, `Top/Bottom`, run_expectancy),
    by = c("Date", "Pitcher", "Batter", "Inning", "Top/Bottom")
  ) %>%
  select(Date, Pitcher, Batter, Inning, `Top/Bottom`, TaggedPitchType, SpinRate, 
         RelSpeed, InducedVertBreak, HorzBreak, run_expectancy, PitchCall, 
         Balls, Strikes, PAofInning, PitchofPA, PitcherThrows, SpinAxis, RelHeight)
write.csv(joined_data, "Joined_data.csv", row.names = FALSE)

str(trackman$Date)
str(all_pbp_expanded$Date)

#View(joined_data)
# Training the model

count_re <- joined_data %>%
  group_by(Balls, Strikes) %>%
  summarize(mean_re = mean(run_expectancy, na.rm = TRUE), .groups = "drop")


re_data <- joined_data %>%
  left_join(count_re, by = c("Balls", "Strikes")) %>%
  rename(RE_before_pitch = mean_re) %>%
  group_by(Date, Pitcher, Batter, Inning, `Top/Bottom`, PAofInning) %>%
  arrange(PitchofPA, .by_group = TRUE) %>%
  mutate(
    RE_after_pitch = lead(RE_before_pitch, default = NA),
    RE_after_pitch = ifelse(is.na(RE_after_pitch), run_expectancy, RE_after_pitch) ,
    pitch_run_value = RE_after_pitch - RE_before_pitch
  ) %>%
  ungroup()

stuff_plus_data <- re_data %>%
  filter(!is.na(pitch_run_value), !is.na(SpinRate), !is.na(RelSpeed)) %>%
  mutate(
    HorzBreak_std = ifelse(PitcherThrows == "Left", -HorzBreak, HorzBreak)
  ) %>%
  select(TaggedPitchType, RelSpeed, SpinRate, InducedVertBreak, HorzBreak_std, pitch_run_value, Pitcher, RelHeight, SpinAxis)

stuff_model <- function(df) {
  X <- as.matrix(df %>% select(RelSpeed, SpinRate, InducedVertBreak, HorzBreak_std, SpinAxis, RelHeight))
  y <- df$pitch_run_value
  

  set.seed(42)
  train_idx <- sample(seq_len(nrow(X)), size = floor(0.8 * nrow(X)))
  
  dtrain <- xgb.DMatrix(data = X[train_idx, , drop = FALSE], label = y[train_idx])
  dvalid <- xgb.DMatrix(data = X[-train_idx, , drop = FALSE], label = y[-train_idx])
  
  xgb.train(
    data = dtrain,
    watchlist = list(train = dtrain, valid = dvalid),
    nrounds = 1000,
    early_stopping_rounds = 25,
    objective = "reg:squarederror",
    max_depth = 4,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 0
  )
}
models_by_pitch_type <- stuff_plus_data %>%
  group_split(TaggedPitchType) %>%
  set_names(map_chr(., ~ unique(.x$TaggedPitchType))) %>%
  map(stuff_model)

stuff_plus_data <- stuff_plus_data %>%
  group_split(TaggedPitchType) %>%
  map_dfr(function(df) {
    pt <- unique(df$TaggedPitchType)
    mod <- models_by_pitch_type[[pt]]
    X <- as.matrix(df %>% select(RelSpeed, SpinRate, InducedVertBreak, HorzBreak_std, RelHeight, SpinAxis))
    df$predicted_run_value <- predict(mod, X)
    df
  })

overall_mean_rv <- mean(stuff_plus_data$predicted_run_value, na.rm = TRUE)
overall_sd_rv <- sd(stuff_plus_data$predicted_run_value, na.rm = TRUE)

stuff_plus_data <- stuff_plus_data %>%
  mutate(stuff_plus = 100 - 10 * (predicted_run_value - overall_mean_rv) / overall_sd_rv)

pitcher_stuff <- stuff_plus_data %>%
  group_by(Pitcher, TaggedPitchType) %>%
  summarize(n_pitches = n(), avg_stuff_plus = mean(stuff_plus), .groups = "drop") %>%
  arrange(desc(avg_stuff_plus)) %>%
  filter(n_pitches > 300)

pitch_based_stuff <- pitcher_stuff %>%
  group_by(TaggedPitchType) %>%
  summarize(
    avg_stuff_plus_pitch = mean(avg_stuff_plus, na.rm = TRUE),
    sd_stuff_plus_pitch = sd(avg_stuff_plus, na.rm = TRUE)
  )

pitch_based_stuff <- stuff_plus_data %>%
  group_by(TaggedPitchType) %>%
  summarize(
    avg_stuff_plus_pitch = mean(stuff_plus, na.rm = TRUE),
    sd_stuff_plus_pitch = sd(stuff_plus, na.rm = TRUE)
  )
View(pitch_based_stuff)

pitcher_avgs <- pitcher_stuff %>%
  group_by(Pitcher) %>%
  summarize(
    avg_stuff = mean(avg_stuff_plus, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_stuff)) %>%
  head(10)
#View(pitcher_avgs)
pitcher_stuff <- pitcher_stuff %>%
  mutate(pitcher_label = paste0(Pitcher, " (", TaggedPitchType,")"),
         avg_stuff_plus = as.numeric(avg_stuff_plus)) %>%
  head(10)

map_dfr(models_by_pitch_type, function(m) {
  tibble(best_iteration = attr(m, "early_stop")$best_iteration,
         best_score = attr(m, "early_stop")$best_score)
}, .id = "pitch_type")

ggplot(pitcher_stuff, aes(x = avg_stuff_plus, y = reorder(pitcher_label, avg_stuff_plus), fill = avg_stuff_plus)) + 
       geom_bar(stat = "identity") +
        geom_text(
          aes(label = round(avg_stuff_plus, 1)),
          hjust = -0.1,
          size = 3.5,
          color = "black") +
         labs(
           title = "Top 10 Pitchers (Stuff+ by Pitch Type)",
           x = "Stuff+",
           y = "Pitcher (Pitch Type)"
         ) +
  theme(legend.position = "none") +
  theme_minimal()
ggsave("pitch_type_stuff.png")



ggplot(pitcher_avgs, aes(x = avg_stuff, y = reorder(Pitcher, avg_stuff), fill = avg_stuff))+
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(avg_stuff, 1)),
    hjust = -0.1,
    size = 3.5, 
    color = "black") +
  labs(
    title = "Top 10 Pitchers by Avg. Stuff+",
    x = "Stuff+",
    y = "Pitcher"
  ) + 
  theme(legend.position = "none") +
  theme_minimal()
  
    
ggsave("pitcher_stuff.png")

    
    
    