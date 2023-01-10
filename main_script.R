# Notes

#Pass Route
#   Distance to nearest defender (separation)
#   Distance downfield
#   Speed
#   Orientation
#   Horizontal location
#
# QB
#   Orientation
#   Speed
#   Dist from LOS
#
# Pass Block
#   Orientation
#   Speed
#   Distance to two closest players
#
# Pass Rush
#   Orientation
#   Speed
#   Distance to QB
#   Horizontal location

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(recipes)
library(MLmetrics)
library(ngscleanR)
library(patchwork)
library(ggridges)


# Data Prep ---------------------------------------------------------------



compute_o_diff <- function(df, prefix = "qb") {
  
  name_x <- sym(paste0(prefix, "_x"))
  name_y <- sym(paste0(prefix, "_y"))
  
  new_column <- paste0("o_to_", prefix)
  
  df <- df %>%
    mutate(
      # compute distances
      dis_x = {{name_x}} - x,
      dis_y = {{name_y}} - y,
      
      # get atan2 in degrees
      tmp = atan2(dis_y, dis_x) * (180 / pi),
      
      # set clockwise (360 - tmp) with 0 on top instead of east (+ 90)
      # https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space
      tmp = (360 - tmp) + 90,
      
      # make sure 0 to 360
      tmp = case_when(tmp < 0 ~ tmp + 360,
                      tmp > 360 ~ tmp - 360 ,
                      TRUE ~ tmp),
      
      # difference in angles
      diff = abs(o - tmp),
      
      # angle to qb
      !!new_column := pmin(360 - diff, diff)
    ) %>%
    select(-diff, -tmp)
  
  return(df)
  
}

tracking %>%
  count(event)


df <- games %>%
  # filter(week == 1) %>%
  left_join(tracking, by = "game_id") %>%
  inner_join(players) %>%
  left_join(pff) %>%
  left_join(plays %>% filter(!(drop_back_type %in% c("DESIGNED_RUN", 
                                                     "DESIGNED_ROLLOUT_LEFT",
                                                     "DESIGNED_ROLLOUT_RIGHT")))) %>%
  mutate(position = pff_position_lined_up) %>%
  mutate(event = ifelse(event == "None", NA, event)) %>%
  fill(event, .direction = "down") %>%
  # select(game_id, play_id) %>%
  # unique() %>%
  # inner_join(pff) %>%
  # count(pff_sack)
  filter(event %in% c("autoevent_ballsnap",
                      "ball_snap", 
                      "autoevent_passforward",
                      "pass_forward"))

n_qbs <- df %>%
  filter(position == "QB") %>%
  group_by(game_id, play_id, frame_id) %>%
  summarize(qbs = n()) %>%
  group_by(game_id, play_id) %>%
  summarise(qbs = max(qbs)) %>%
  filter(qbs == 1) %>%
  ungroup()

qbs <- df %>%
  filter(position == "QB") %>%
  dplyr::select(
    game_id,
    play_id,
    frame_id,
    qb_x = x,
    qb_y = y
  ) %>%
  inner_join(n_qbs, by = c("game_id", "play_id")) %>%
  select(-qbs)

df <- df %>%
  left_join(qbs) %>%
  clean_and_rotate() %>%
  compute_o_diff() %>%
  mutate(defense = ifelse(official_position %in% c("QB", "WR", "TE", "RB", "FB", "C", "G", "T", "NT"),
                          0, 1))  %>%
  mutate(dist_from_los = x - absolute_yardline_number) 


df_football <- tracking %>%
  filter(team == 'football') %>% 
  clean_and_rotate() %>%
  select(game_id, play_id, frame_id, x, y)
# df Coverage -------------------------------------------------------------


df_passRoute <- df %>%
  filter(game_id != 2021110100, play_id != 1267) %>%
  filter(pff_role == 'Pass Route') %>%
  select(game_id, play_id, frame_id, nfl_id, x, y, s, o) %>%
  left_join(plays %>% select(game_id, play_id, absolute_yardline_number)) %>%
  mutate(rec_dist_downfield = x - absolute_yardline_number)



df_coverage <- df %>%
  filter(pff_role == 'Coverage') %>%
  select(game_id, play_id, frame_id, nfl_id, x, y, s, o) %>%
  rename(cov_x = x, cov_y = y, cov_s = s, cov_o = o, cov_nfl_id = nfl_id) %>%
  left_join(df_passRoute) %>%
  mutate(cov_dist_to_rec = sqrt( (x - cov_x)**2 + (y - cov_y)**2 )) %>%
  group_by(game_id, play_id, frame_id, cov_nfl_id) %>%
  mutate(cov_dist_to_rec_rk = rank(cov_dist_to_rec, ties.method = 'first')) %>%
  filter(cov_dist_to_rec_rk <= 2) %>%
  arrange(game_id, play_id, frame_id, cov_y) %>%
  select(game_id, play_id, frame_id, contains("cov")) %>%
  pivot_wider(id_cols = game_id:cov_o, 
              names_from = cov_dist_to_rec_rk, 
              values_from = cov_dist_to_rec,
              names_prefix = "cov_nearest_rec_")

df_cov_number <- df_coverage %>%
  group_by(game_id, play_id, cov_nfl_id) %>%
  select(game_id, play_id, frame_id, cov_y) %>%
  filter(frame_id == min(frame_id)) %>%
  group_by(game_id, play_id) %>%
  mutate(cov_ = rank(cov_y, ties.method = 'first')) %>%
  select(game_id, play_id, cov_nfl_id, cov_)


df_coverage_wide <- df_coverage %>%
  left_join(df_cov_number, by = c('game_id', 'play_id','cov_nfl_id')) %>% 
  pivot_wider(id_cols = game_id:frame_id, names_from = cov_,
              names_sep = "_", values_from = cov_x:cov_nearest_rec_2)

df_coverage_wide[is.na(df_coverage_wide)] <- -1


# df Receivers ------------------------------------------------------------

df_passRoute <- df_passRoute %>%
  left_join(df_coverage %>%
              ungroup() %>%
              select(game_id, play_id, frame_id, cov_x, cov_y)) %>%
  mutate(rec_dist_to_defender = sqrt( (x - cov_x)**2 + (y - cov_y)**2 )) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  mutate(rec_dist_to_defender_rk = rank(rec_dist_to_defender, ties.method = 'first')) %>%
  filter(rec_dist_to_defender_rk <= 2) %>%
  pivot_wider(id_cols = game_id:rec_dist_downfield, 
              names_from = rec_dist_to_defender_rk, 
              values_from = rec_dist_to_defender,
              names_prefix = "rec_nearest_def_dist_") %>%
  select(-absolute_yardline_number) %>%
  rename(rec_x = x,
         rec_y = y,
         rec_s = s,
         rec_o = o)

df_rec_number <- df_passRoute %>%
  group_by(game_id, play_id, nfl_id) %>%
  filter(frame_id == min(frame_id)) %>%
  group_by(game_id, play_id) %>%
  mutate(rec_ = rank(rec_y, ties.method = 'first')) %>%
  select(game_id, play_id, nfl_id, rec_)


df_passRoute <- df_passRoute %>%
  left_join(df_rec_number) %>% 
  pivot_wider(id_cols = game_id:frame_id, names_from = rec_,
              names_sep = "_", values_from = rec_x:rec_nearest_def_dist_2)

df_passRoute[is.na(df_passRoute)] <- -1


# df QB -------------------------------------------------------------------



df_passer <- df %>%
  filter(pff_role == 'Pass') %>%
  select(game_id, play_id, frame_id, x, y, s, o) %>%
  left_join(plays %>% select(game_id, play_id, absolute_yardline_number)) %>%
  mutate(QB_dist_from_LOS = x - absolute_yardline_number) %>%
  select(-absolute_yardline_number) %>%
  rename(QB_x = x,
         QB_y = y,
         QB_s = s,
         QB_o = o)


# df Rushers --------------------------------------------------------------



df_passRush <- df %>%
  filter(game_id != 2021110100, play_id != 1267) %>%
  filter(pff_role == 'Pass Rush') %>%
  select(game_id, play_id, frame_id, nfl_id, x, y, s, o) %>%
  left_join(df_passer %>% select(game_id, play_id, frame_id, QB_x, QB_y)) %>%
  mutate(rush_dist_to_QB = sqrt((x - QB_x)**2 + (y - QB_y)**2)) %>%
  select(-starts_with('QB')) %>%
  rename(rush_x = x,
         rush_y = y,
         rush_s = s,
         rush_o = o)

df_rush_number <- df_passRush %>%
  group_by(game_id, play_id, nfl_id) %>%
  filter(frame_id == min(frame_id)) %>%
  group_by(game_id, play_id) %>%
  mutate(rush_ = rank(rush_y, ties.method = 'first')) %>%
  select(game_id, play_id, nfl_id, rush_)

df_passRush_wide <- df_passRush %>%
  left_join(df_rush_number) %>% 
  pivot_wider(id_cols = game_id:frame_id, names_from = rush_,
              names_sep = "_", values_from = rush_x:rush_dist_to_QB) 

df_passRush_wide[is.na(df_passRush_wide)] <- -1



# df Blockers -------------------------------------------------------------



df_passBlock <- df %>%
  filter(game_id != 2021110100, play_id != 1267) %>%
  filter(pff_role == 'Pass Block') %>%
  select(game_id, play_id, frame_id, nfl_id, x, y, s, o) %>%
  left_join(df_passRush %>%select(-nfl_id), by = c('game_id', 'play_id', 'frame_id')) %>%
  mutate(blk_dist_to_rush = sqrt((x - rush_x)**2 + (y - rush_y)**2)) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  mutate(blk_dist_to_rush_rk = rank(blk_dist_to_rush, ties.method = 'first')) %>%
  filter(blk_dist_to_rush_rk <= 2) %>%
  pivot_wider(id_cols = game_id:o, 
              names_from = blk_dist_to_rush_rk, 
              values_from = blk_dist_to_rush,
              names_prefix = "blk_nearest_def_dist_")  %>%
  rename(blk_x = x,
         blk_y = y,
         blk_s = s,
         blk_o = o,
         blk_nfl_id = nfl_id)


df_blk_number <- df_passBlock %>%
  group_by(game_id, play_id, blk_nfl_id) %>%
  filter(frame_id == min(frame_id)) %>%
  group_by(game_id, play_id) %>%
  mutate(blk_ = rank(blk_y, ties.method = 'first')) %>%
  select(game_id, play_id,blk_nfl_id,  blk_)

df_passBlock <- df_passBlock %>%
  left_join(df_blk_number) %>% 
  pivot_wider(id_cols = game_id:frame_id, names_from = blk_,
              names_sep = "_", values_from = blk_x:blk_nearest_def_dist_2) 

df_passBlock[is.na(df_passBlock)] <- -1


# df Target ---------------------------------------------------------------


df_target <- pff %>%
  select(game_id, play_id, pff_hit_allowed, pff_hurry_allowed, pff_sack_allowed) %>%
  unique() %>%
  group_by(game_id, play_id) %>%
  summarise(hit = sum(pff_hit_allowed, na.rm = T),
            hurry = sum(pff_hurry_allowed, na.rm = T),
            sack = sum(pff_sack_allowed, na.rm = T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pressure = sum(hit, hurry, sack),
         pressure = ifelse(pressure >= 1, 1, 0)) %>%
  select(game_id, play_id, pressure)



df_mod <- df_target %>%
  filter(game_id != 2021110100, play_id != 1267) %>%
  left_join(
    plays %>%
      select(game_id, play_id, quarter, down, yards_to_go, absolute_yardline_number, 
             offense_formation, personnel_o, personnel_d, defenders_in_box,
             drop_back_type, pff_play_action, pff_pass_coverage_type)) %>%
  left_join(df_passRoute, by = c('game_id', 'play_id')) %>%
  left_join(df_passRush_wide, by = c('game_id', 'play_id', 'frame_id')) %>%
  left_join(df_passer, by = c('game_id', 'play_id', 'frame_id')) %>%
  left_join(df_passBlock, by = c('game_id', 'play_id', 'frame_id')) %>%
  left_join(df_coverage_wide, by = c('game_id', 'play_id', 'frame_id')) %>%
  filter(!is.na(frame_id)) %>%
  na.omit() 

df_mod_ids <- df_mod %>%
  select(game_id, play_id, frame_id)

df_mod <- df_mod %>% 
  #select(-game_id, -play_id) %>%
  recipe(pressure ~ .) %>%
  step_dummy(c(offense_formation, personnel_o,
               personnel_d, pff_pass_coverage_type, drop_back_type),
             one_hot = TRUE)  %>% 
  prep() %>% 
  juice() 

colSums(is.na(df_mod))


train <- df_mod %>%
  left_join(games %>% select(game_id, week), by = c('game_id')) %>%
  filter(week != 8) %>%
  select(-week) 


test <- df_mod %>% 
  left_join(games %>% select(game_id, week), by = c('game_id')) %>%
  filter(week == 8) %>%
  select(-week) 


fit_glm <- glm(factor(pressure) ~ ., data = train,
               family = 'binomial')  


tuneRF(x = train[, c(-1, -2, -3)], y = factor(train$pressure), ntreeTry = 20, trace = T)

fit_rf <- randomForest(x = train %>% select(-c(game_id, play_id, pressure)),
                       y = factor(train$pressure), 
                       ntree = 75, do.trace = T, mtry = 22)

pred_glm <- predict(fit_glm, test, type = 'response')
pred_rf <- predict(fit_rf, test, type = 'prob')[, 2]
pred_rf_votes <- predict(fit_rf, test)

LogLoss(y_pred = pred_glm, y_true = test$pressure)
LogLoss(y_pred = pred_rf, y_true = test$pressure)
Accuracy(y_pred = pred_rf_votes, y_true = test$pressure)
MLmetrics::Accuracy(predict(fit_rf, test), y_true = test$pressure)

# XGBoost ####


# tuning


grid_tuning <- expand.grid(
  nrounds = 500,
  max_depth = 8,
  eta = 0.3,
  gamma = c(0, 1, 5),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "cv",
  number = 3, 
  classProbs = T,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_train <- caret::train(
  x = train[, c(-1, -2, -3, -9)],
  y = factor(train$pressure, labels = c("no_pressure", "pressure")),
  trControl = train_control,
  tuneGrid = grid_tuning,
  method = "xgbTree",
  verbose = TRUE
)


tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}


tuneplot(xgb_train)

save.image()
# Final params

params <- list(
  max_depth = 8,
  eta = 0.01,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "cv",
  number = 5, 
  classProbs = T,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

# final model

fit_xgb <- caret::train(
  y = factor(train$pressure, labels = c("no_pressure", "pressure")),
  x = train[, c(-2)],
  trControl = train_control,
  tuneGrid = params,
  method = "xgbTree",
  verbose = TRUE
) 



xgb.DMatrix(as.matrix(train[, c(-1, -2)] %>% select(-pressure)), label = train$pressure)

fit_xgb <- xgb.train(data = xgb.DMatrix(as.matrix(train %>% select(-c(pressure, game_id, play_id))),
                                        label = train$pressure),
                     nrounds = 500,
                     objective = 'binary:logistic',
                     watchlist = list(train = xgb.DMatrix(as.matrix(train %>% select(-c(pressure, game_id, play_id))), label = train$pressure),
                                      test = xgb.DMatrix(as.matrix(test %>% select(-c(pressure, game_id, play_id))), label = test$pressure)),
                     params = params, 
                     verbose = 1,
                     early_stopping_rounds = 3
)

fit_xgb$feature_names

for (i in fit_xgb$feature_names) {
  if (!i %in% names(test)) {
    print(i)
  }
}

pred_xgb <- predict(fit_xgb, 
                    xgb.DMatrix(as.matrix(test %>% select(-c(pressure, game_id, play_id))),
                                label = test$pressure), 
                    type = "prob")#[,2]
pred_xgb_votes <- ifelse(pred_xgb > .5, 1, 0)

MLmetrics::Accuracy(y_pred = pred_xgb_votes, y_true = test$pressure)
MLmetrics::LogLoss(y_pred = pred_xgb, y_true = test$pressure)

MLmetrics::LogLoss(y_pred = rep(.284, nrow(test)),
                   y_true = test$pressure)

df_target %>%
  inner_join(train %>% select(game_id, play_id) %>% unique()) %>%
  count(pressure) %>%
  ungroup() %>%
  mutate(pct = n/sum(n))

table(pred_xgb_votes, test$pressure)

vip::vip(fit_xgb, num_features = 15, include_type = T)




# final model  ------------------------------------------------------------

fit_xgb <- xgboost(as.matrix(df_mod %>% select(-c(pressure, game_id, play_id))), 
                   label = df_mod$pressure, 
                   nrounds = 390, 
                   params = params)
# shap_values <- shap.values(xgb_model = fit_xgb, X_train = as.matrix(train[, c(-2)]))
# shap_values$mean_shap_score
library(SHAPforxgboost)
shap_long <- shap.prep(xgb_model = fit_xgb, X_train = as.matrix(df_mod %>% select(-c(pressure, game_id, play_id))))
shap.plot.summary.wrap1(fit_xgb, X = as.matrix(train[, c(-1, -2, -3, -9)]), top_n = 15) 


shap_long <- tibble(shap_long)
attr(shap_long, which = "sorted")


pred_xgb_final <- predict(fit_xgb, xgb.DMatrix(as.matrix(df_mod %>% select(-c(pressure, game_id, play_id))),
                                               label = df_mod$pressure), 
                          type = "prob")

df_shap <- shap_long %>%
  group_by(ID) %>%
  mutate(shap_total = sum(abs(value)),
         shap_pct = value/sum(value)) %>%
  ggplot(., aes(x = shap_total)) +
  geom_density()


df_pred <- shap_wide %>%
  bind_cols(df_mod$game_id, df_mod$play_id, pred_xgb_final) %>%
  rename(game_id = ...275,
         play_id = ...276,
         pred_pressure = ...277)


shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, 
              names_from = variable, values_from = value_pct) %>%
  bind_cols(df_mod$game_id, df_mod$play_id, df_mod$frame_id) %>%
  rename(game_id = ...275,
         play_id = ...276,
         frame = ...277)



rm(shap_long_pct)

shap_long_pct <- shap_long %>%
  mutate(abs_value = abs(value)) %>%
  group_by(ID) %>%
  mutate(total_value = sum(abs_value)) %>%
  ungroup() %>%
  mutate(value_pct = abs_value / total_value,
         value_pct = ifelse(value < 0, -1*value_pct, value_pct)) %>%
  left_join(tibble(pred_pressure = pred_xgb_final) %>%
              mutate(ID = row_number())) %>%
  mutate(score = ifelse(value_pct < 0,
                        value_pct  * (1-pred_pressure),
                        value_pct * pred_pressure))

# Blockers Summary --------------------------------------------------------
blk_shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, names_from = variable, values_from = score) %>% 
  select(ID, starts_with("blk")) %>%
  bind_cols(df_mod %>% select(game_id, play_id, frame_id)) %>%
  group_by(game_id, play_id) %>%
  filter(between(frame_id - min(frame_id), 1, 35)) %>%
  pivot_longer(cols = starts_with("blk")) %>%
  mutate(blk_ = as.numeric(str_sub(name, start = -1))) %>%
  left_join(df_blk_number, 
            by = c('game_id', 'play_id', 'blk_')) %>%
  filter(!is.na(blk_nfl_id))




blk_shap_wide %>%
  filter(str_detect(name, 'blk_o_'), game_id == 2021090900, play_id == 97) %>%
  group_by(name) %>%
  mutate(cum_sum = cumsum(value)) %>%
  ggplot(., aes(x = ID, y = cum_sum, group = name, color = name)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_color_viridis_d() +
  theme_minimal() 


df %>%
  select(game_id, play_id, frame_id) %>%
  unique() %>%
  count(game_id, play_id) %>%
  ggplot(., aes(x = n)) +
  geom_density()

blk_summary <- blk_shap_wide %>%
  filter(blk_nfl_id != 42765) %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  group_by(game_id, play_id, frame_id, blk_nfl_id) %>%
  na.omit() %>%
  # summarise(blk_quality = logOddsToProb(sum(value, na.rm = T))) %>%
  summarise(pct = sum(value)) %>%
  group_by(game_id, play_id, blk_nfl_id) %>%
  summarise(pct = mean(pct)) %>%
  left_join(players, by = c('blk_nfl_id' = 'nfl_id')) 


blk_summary %>%
  group_by(blk_nfl_id) %>%
  summarise(pct = mean(pct, na.rm = T)) %>%
  ggplot(., aes(x = pct)) +
  # geom_histogram(bins = 50, color = "black") +
  geom_density()

blk_summary_plot <- bind_rows(blk_summary %>%  
                                group_by(blk_nfl_id, display_name) %>%
                                summarise(n = n(),
                                          pct = mean(pct, na.rm = T)) %>%
                                filter(n > 100) %>%
                                arrange(pct) %>%
                                head(10),
                              # tibble(blk_nfl_id = 11111, display_name = "", n = 0, pct = 0),
                              blk_summary %>%
                                group_by(blk_nfl_id, display_name) %>%
                                summarise(n = n(),
                                          pct = mean(pct, na.rm = T)) %>%
                                filter(n > 100) %>%
                                arrange(-pct) %>%
                                head(10))%>% 
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = 'white', fill = "#A63860") +
  coord_flip() +
  scale_y_reverse(labels = scales::percent_format()) +
  geom_vline(xintercept = 10.5) +
  theme_bw() +
  labs(y = "Avg. Pressure Contribution Score per Play",
       x = NULL,
       title = 'Best and Worst Pass Blockers',
       subtitle = "Min. 100 Plays")

rush_summary_plot <- bind_rows(rush_summary %>%  
                                 group_by(nfl_id, display_name) %>%
                                 summarise(n = n(),
                                           pct = mean(pct, na.rm = T)) %>%
                                 filter(n > 100) %>%
                                 arrange(pct) %>%
                                 head(10),
                               # tibble(nfl_id = 11111, display_name = "", n = 0, pct = 0),
                               rush_summary %>%
                                 group_by(nfl_id, display_name) %>%
                                 summarise(n = n(),
                                           pct = mean(pct, na.rm = T)) %>%
                                 filter(n > 100) %>%
                                 arrange(-pct) %>%
                                 head(10))%>% 
  ggplot(., aes(x = reorder(display_name, pct), y = pct)) +
  geom_col(color = 'white', fill = "#A63860") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_vline(xintercept = 10.5) +
  theme_bw() +
  labs(y = "Avg. Pressure Contribution Score per Play",
       x = NULL,
       title = 'Best and Worst Pass Rushers',
       subtitle = "Min. 100 Plays")

QB_summary_plot <- bind_rows(QB_summary %>%  
                               group_by(nfl_id, display_name) %>%
                               summarise(n = n(),
                                         pct = mean(pct, na.rm = T)) %>%
                               filter(n > 100) %>%
                               arrange(pct) %>%
                               head(10),
                             # tibble(nfl_id = 11111, display_name = "", n = 0, pct = 0),
                             QB_summary %>%
                               group_by(nfl_id, display_name) %>%
                               summarise(n = n(),
                                         pct = mean(pct, na.rm = T)) %>%
                               filter(n > 100) %>%
                               arrange(-pct) %>%
                               head(10))%>% 
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = 'white', fill = "#A63860") +
  coord_flip() +
  scale_y_reverse(labels = scales::percent_format()) +
  geom_vline(xintercept = 10.5) +
  theme_bw() +
  labs(y = "Avg. Pressure Contribution Score per Play",
       x = NULL,
       title = 'Best and Worst QBs',
       subtitle = "Min. 100 Plays")


rec_summary_plot <- bind_rows(rec_summary %>%  
                                group_by(nfl_id, display_name) %>%
                                summarise(n = n(),
                                          pct = mean(pct, na.rm = T)) %>%
                                filter(n > 100) %>%
                                arrange(pct) %>%
                                head(10),
                              # tibble(nfl_id = 11111, display_name = "", n = 0, pct = 0),
                              rec_summary %>%
                                group_by(nfl_id, display_name) %>%
                                summarise(n = n(),
                                          pct = mean(pct, na.rm = T)) %>%
                                filter(n > 100) %>%
                                arrange(-pct) %>%
                                head(10))%>% 
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = 'white', fill = "#A63860") +
  coord_flip() +
  scale_y_reverse(labels = scales::percent_format()) +
  geom_vline(xintercept = 10.5) +
  theme_bw() +
  labs(y = "Avg. Pressure Contribution Score per Play",
       x = NULL,
       title = 'Best and Worst Receivers',
       subtitle = "Min. 100 Plays")

cov_summary_plot <- bind_rows(cov_summary %>%  
                                group_by(cov_nfl_id, display_name) %>%
                                summarise(n = n(),
                                          pct = mean(pct, na.rm = T)) %>%
                                filter(n > 100) %>%
                                arrange(pct) %>%
                                head(10),
                              # tibble(cov_nfl_id = 11111, display_name = "", n = 0, pct = 0),
                              cov_summary %>%
                                group_by(cov_nfl_id, display_name) %>%
                                summarise(n = n(),
                                          pct = mean(pct, na.rm = T)) %>%
                                filter(n > 100) %>%
                                arrange(-pct) %>%
                                head(10))%>% 
  ggplot(., aes(x = reorder(display_name, pct), y = pct)) +
  geom_col(color = 'white', fill = "#A63860") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_vline(xintercept = 10.5) +
  theme_bw() +
  labs(y = "Avg. Pressure Contribution Score per Play",
       x = NULL,
       title = 'Best and Worst Coverage Players',
       subtitle = "Min. 100 Plays")

all_pos_leaderboard_1 <- blk_summary_plot / rush_summary_plot / QB_summary_plot 
all_pos_leaderboard_2 <- rec_summary_plot / cov_summary_plot


ggsave(filename = 'all_pos_leaderboard_1.png', plot = all_pos_leaderboard_1, 
       width = 8, height = 12)
ggsave(filename = 'all_pos_leaderboard_2.png', plot = all_pos_leaderboard_2, 
       width = 7, height = 8)

ggplot(., aes(x = reorder(display_name, -blk_quality), y = blk_quality)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_reverse()



# Pass Rusher Summary -----------------------------------------------------

rush_shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, names_from = variable, values_from = score) %>% 
  select(ID, starts_with("rush")) %>%
  bind_cols(df_mod %>% select(game_id, play_id, frame_id)) %>%
  group_by(game_id, play_id) %>%
  filter(between(frame_id - min(frame_id), 1, 35)) %>%
  pivot_longer(cols = starts_with("rush")) %>%
  mutate(rush_ = as.numeric(str_sub(name, start = -1))) %>%
  left_join(df_rush_number, 
            by = c('game_id', 'play_id', 'rush_'))%>%
  filter(!is.na(nfl_id))

(scores_over_time_plot <- rush_shap_wide %>%
    #filter(blk_nfl_id != 42765) %>%
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    na.omit() %>%
    # summarise(rush_quality =logOddsToProb(sum(value, na.rm = T))) %>%
    summarise(pct = sum(value)) %>%
    group_by(game_id, play_id) %>%
    mutate(time = frame_id - min(frame_id) + 1) %>%
    ungroup() %>%
    slice_sample(n = 100000) %>%
    ggplot(., aes(x = time/10, y = pct)) +
    # geom_point(alpha = .5) +
    geom_smooth(color = "#A63860") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = 'Seconds Since Ball Snap', y = 'Avg. Pressure Contribution Score',
         title = 'Pressure Contribution Score Over Time',
         subtitle = 'Pass Rushers') +
    theme_bw())
ggsave(filename = 'scores_over_time_plot.png', plot = scores_over_time_plot,
       height = 4, width = 10) 



rush_summary <- rush_shap_wide %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  #filter(blk_nfl_id != 42765) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  na.omit() %>%
  # summarise(rush_quality =logOddsToProb(sum(value, na.rm = T))) %>%
  summarise(pct = sum(value)) %>%
  group_by(game_id, play_id, nfl_id) %>%
  summarise(pct = mean(pct)) %>%
  left_join(players, by = c('nfl_id' = 'nfl_id')) 

rush_summary %>%
  group_by(nfl_id) %>%
  summarise(pct = mean(pct, na.rm = T)) %>%
  ggplot(., aes(x = pct)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_continuous()

rush_summary_by_player <- rush_summary %>%
  group_by(nfl_id, display_name) %>%
  summarise(n = n(),
            pct_tot = sum(pct),
            pct = mean(pct, na.rm = T)) %>%
  filter(n > 100) %>%
  left_join(pressures_by_player)

pressures_by_player <- pff %>%
  rowwise() %>%
  mutate(pressure = sum(pff_hurry, pff_hit, pff_sack, na.rm = T)) %>%
  filter(pff_role == 'Pass Rush') %>%
  select(game_id, play_id, nfl_id, pressure) %>%
  group_by(nfl_id) %>%
  summarise(pressures = sum(pressure))


cor(rush_summary_by_player$pct, rush_summary_by_player$pressures)**2
cor(rush_summary_by_player$pct_tot, rush_summary_by_player$pressures)**2

(score_vs_pressures_plot <- rush_summary_by_player %>%
  ggplot(., aes(x = pct, y = pressures)) +
  geom_point(color = "#95B3BB")  +
  geom_smooth(color = "#A63860", method = 'lm') +
  labs(title = "Pressure Contribution Score vs. Pressures for Pass Rushers",
       subtitle = glue::glue('R-Squared: {round(cor(rush_summary_by_player$pct, rush_summary_by_player$pressures)**2, 3)}'),
       x = 'Avg. Pressure Contribution Score',
       y = "Acutal Number of Pressures",
       caption = 'Min. 100 Plays') +
  theme_bw() +
  scale_x_continuous(labels = scales::percent_format()))
ggsave(filename = "score_vs_pressures_plot.png", plot = score_vs_pressures_plot,
       height = 5, width = 10)


rush_summary_by_player %>%
  ggplot(., aes(x = pct_tot, y = pressures)) +
  geom_point(color = "#95B3BB")  +
  geom_smooth(color = "#A63860", method = 'lm') +
  labs(title = "XXXX Value vs. Pressures for Pass Rushers",
       subtitle = glue::glue('R-Squared: {round(cor(rush_summary_by_player$pct_tot, rush_summary_by_player$pressures)**2, 3)}')) +
  theme_bw()



rush_summary_by_player %>%
  arrange(-pct) %>%
  # ungroup() %>%
  # mutate(rank = rank(-rush_quality)) 
  
  arrange(-pct) %>%
  head(20) %>%
  ggplot(., aes(x = reorder(display_name, pct), y = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format())


rush_summary_by_player %>%
  arrange(rush_quality) %>%
  head(20) %>%
  ggplot(., aes(x = reorder(display_name, rush_quality), y = rush_quality)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format())


# Receiver Summary -----------------------------------------------------

rec_shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, names_from = variable, values_from = score) %>% 
  select(ID, starts_with("rec")) %>%
  bind_cols(df_mod %>% select(game_id, play_id, frame_id)) %>%
  group_by(game_id, play_id) %>%
  filter(between(frame_id - min(frame_id), 1, 35)) %>%
  pivot_longer(cols = starts_with("rec")) %>%
  mutate(rec_ = as.numeric(str_sub(name, start = -1))) %>%
  left_join(df_rec_number, 
            by = c('game_id', 'play_id', 'rec_'))

rec_summary <- rec_shap_wide %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  #filter(blk_nfl_id != 42765) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  na.omit() %>%
  summarise(pct = sum(value)) %>%
  group_by(game_id, play_id, nfl_id) %>%
  summarise(pct = mean(pct)) %>%
  left_join(players, by = c('nfl_id' = 'nfl_id')) 

rec_summary %>%
  ggplot(., aes(x = pct)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_continuous()

rec_summary %>%
  group_by(nfl_id, display_name) %>%
  summarise(n = n(),
            pct = mean(pct)) %>%
  filter(n > 100) %>%
  arrange(-pct) %>%
  head(20) %>%
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_reverse(labels = scales::percent_format())

rec_summary %>%
  group_by(nfl_id, display_name) %>%
  summarise(n = n(),
            pct = mean(pct)) %>%
  filter(n > 100) %>%
  arrange(pct) %>%
  head(20) %>%
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_reverse()


# Coverage Summary -----------------------------------------------------

cov_shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, names_from = variable, values_from = score) %>% 
  select(ID, starts_with("cov")) %>%
  bind_cols(df_mod %>% select(game_id, play_id, frame_id)) %>%
  group_by(game_id, play_id) %>%
  filter(between(frame_id - min(frame_id), 1, 35)) %>%
  pivot_longer(cols = starts_with("cov")) %>%
  mutate(cov_ = str_sub(name, start = -2),
         cov_ = as.numeric(str_replace_all(cov_, "_", ""))) %>%
  left_join(df_cov_number, 
            by = c('game_id', 'play_id', 'cov_'))

cov_summary <- cov_shap_wide %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  #filter(blk_nfl_id != 42765) %>%
  group_by(game_id, play_id, frame_id, cov_nfl_id) %>%
  na.omit() %>%
  summarise(pct = sum(value)) %>%
  group_by(game_id, play_id, cov_nfl_id) %>%
  summarise(pct = mean(pct)) %>%
  left_join(players, by = c('cov_nfl_id' = 'nfl_id')) 

cov_summary %>%
  ggplot(., aes(x = pct)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_continuous(labels = scales::percent_format())

cov_summary %>%
  group_by(cov_nfl_id, display_name) %>%
  summarise(n = n(),
            pct = mean(pct)) %>%
  filter(n > 100) %>%
  arrange(-pct) %>%
  head(20) %>%
  ggplot(., aes(x = reorder(display_name, pct), y = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_continuous()

cov_summary %>%
  group_by(cov_nfl_id, display_name) %>%
  summarise(n = n(),
            cov_quality = mean(cov_quality)) %>%
  filter(n > 100) %>%
  arrange(cov_quality) %>%
  head(20) %>%
  ggplot(., aes(x = reorder(display_name, cov_quality), y = cov_quality)) +
  geom_col(color = "black") +
  coord_flip() 



# QB Summary -----------------------------------------------------

QB_shap_wide %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  summarise(value = sum(value)) %>%
  mutate(value = round(logOddsToProb(value)-0.5, 4)) %>%
  # left_join(df_target) %>%
  View()

QB_shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, names_from = variable, values_from = score) %>% 
  select(ID, starts_with("QB")) %>%
  bind_cols(df_mod %>% select(game_id, play_id, frame_id)) %>%
  group_by(game_id, play_id) %>%
  filter(between(frame_id - min(frame_id), 1, 35)) %>%
  pivot_longer(cols = starts_with("QB")) %>%
  left_join(df %>%
              filter(pff_role == "Pass") %>%
              select(game_id, play_id, nfl_id) %>%
              unique())

QB_summary <- QB_shap_wide %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  #filter(blk_nfl_id != 42765) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  na.omit() %>%
  summarise(pct = sum(value)) %>%
  group_by(game_id, play_id, nfl_id) %>%
  summarise(pct = mean(pct)) %>%
  left_join(players, by = c('nfl_id' = 'nfl_id')) 

QB_summary %>%
  ggplot(., aes(x = pct)) +
  geom_histogram(bins = 50, color = "black") +
  scale_x_continuous()

QB_summary %>%
  group_by(nfl_id, display_name) %>%
  summarise(n = n(),
            pct = mean(pct, na.rm = T)) %>%
  filter(n > 100) %>%
  arrange(pct) %>%
  head(15) %>%
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_reverse()

QB_summary %>%
  group_by(nfl_id, display_name) %>%
  summarise(n = n(),
            pct = mean(pct, na.rm = T)) %>%
  filter(n > 100) %>%
  arrange(-pct) %>%
  head(15) %>%
  ggplot(., aes(x = reorder(display_name, -pct), y = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_reverse()

df %>%
  tibble() %>%
  filter(event %in% c('pass_forward', 'autoevent_passforward')) %>%
  select(game_id, play_id, frame_id) %>%
  # count(frame_id, sort = T)
  unique() %>%
  filter(frame_id > 5) %>%
  summarise(med = median(frame_id))
ggplot(., aes(x = frame_id))+
  geom_density()


# Situation ---------------------------------------------------------------
rm(situ_shap_wide)
situ_shap_wide <- shap_long_pct %>%
  pivot_wider(id_cols = ID, names_from = variable, values_from = score) %>% 
  select(-starts_with("QB"), -starts_with('rec'), 
         -starts_with('rush'), -starts_with('cov'), -starts_with('blk')) %>%
  rename(frame = frame_id) %>%
  bind_cols(df_mod %>% select(game_id, play_id, frame_id)) %>%
  group_by(game_id, play_id) %>%
  filter(between(frame_id - min(frame_id), 1, 35)) %>%
  pivot_longer(cols = drop_back_type_SCRAMBLE:drop_back_type_UNKNOWN) %>%
  mutate(name = paste('situ', name, sep = "_"))


situ_summary <- situ_shap_wide %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  #filter(blk_nfl_id != 42765) %>%
  group_by(game_id, play_id, frame_id) %>%
  na.omit() %>%
  summarise(pct = sum(value)) %>%
  group_by(game_id, play_id) %>%
  summarise(pct = mean(pct))


# all position summary ---------------------------------------------------------

all_pos_summary <- blk_summary %>%
  mutate(pos = 'blk') %>%
  rename(nfl_id = blk_nfl_id,
         value = pct) %>%
  bind_rows(rush_summary %>%
              mutate(pos = 'rush') %>%
              rename(value = pct)) %>%
  bind_rows(cov_summary %>%
              mutate(pos = 'cov') %>%
              rename(nfl_id = cov_nfl_id,
                     value = pct)) %>%
  bind_rows(rec_summary %>%
              mutate(pos = 'rec') %>%
              rename(value = pct)) %>%
  bind_rows(QB_summary %>%
              mutate(pos = 'QB') %>%
              rename(value = pct)) 

(scores_by_pos_distplot <- all_pos_summary %>%
    left_join(df_target) %>%
    # filter(pressure == 1) %>%
    group_by(nfl_id, pos, pressure) %>%
    mutate(pressure = ifelse(pressure == 1, 'Pressure', 'No Pressure'),
           pos = case_when(
             pos == 'blk' ~ 'Pass Blocker',
             pos == 'rush' ~ 'Pass Rusher',
             pos == 'rec' ~ 'Receiver',
             pos == 'cov' ~ 'Coverage',
             pos == 'QB' ~ 'QB'
           )) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ggplot(., aes(x = value, fill = pos)) +
    geom_density(alpha = .6) +
    theme_bw() +
    scale_fill_viridis_d() +
    scale_x_continuous(labels = scales::percent_format()) +
    # facet_wrap(~pressure, nrow = 2) + 
    labs(x = 'Pressure Contribution Score',
         fill = 'Role', 
         title = "Distrbution of Pressure Contribution Scores Across Play Roles",
         subtitle = "QBs and Pass Rushers have strongest contribution to pressures")
)

ggsave(plot = scores_by_pos_distplot, 
       filename = "scores_by_pos_distplot.png",
       width = 10, height = 5.5, dpi = 300)


# logOddsToProb <- function(log_odds) {
#   odds <- exp(log_odds) 
#   prob <- odds / (1+odds)
#   return(prob - .5)
# }


all_pos_scores_per_frame <- bind_rows(
  blk_shap_wide %>%
    #filter(game_id == 2021091210, play_id == 574) %>%
    group_by(game_id, play_id, frame_id, blk_nfl_id) %>%
    summarise(pct = sum(value)) %>%
    # summarise(value = logOddsToProb(sum(value))) %>%
    rename(nfl_id = blk_nfl_id) %>%
    mutate(pos = 'blk'),
  
  rush_shap_wide %>%
    #filter(game_id == 2021091210, play_id == 574) %>%
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    summarise(pct = sum(value)) %>%
    mutate(pos = 'rush'),
  
  rec_shap_wide %>%
    #filter(game_id == 2021091210, play_id == 574) %>%
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    summarise(pct = sum(value)) %>%
    mutate(pos = 'rec'),
  
  cov_shap_wide %>%
    #filter(game_id == 2021091210, play_id == 574) %>%
    group_by(game_id, play_id, frame_id, cov_nfl_id) %>%
    summarise(pct = sum(value)) %>%
    rename(nfl_id = cov_nfl_id) %>%
    mutate(pos = 'cov'),
  
  QB_shap_wide %>%
    #filter(game_id == 2021091210, play_id == 574) %>%
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    summarise(pct = sum(value)) %>%
    mutate(pos = 'QB'),
  
  situ_shap_wide %>%
    #filter(game_id == 2021091210, play_id == 574) %>%
    group_by(game_id, play_id, frame_id) %>%
    summarise(pct = sum(value)) %>%
    mutate(pos = 'situ')
  
) 

all_pos_scores_per_frame %>%
  left_join(df_target) %>%
  filter(pressure == 1) %>%
  group_by(game_id, play_id, pos) %>%
  summarise(pct = mean(pct)) %>%
  filter(pos == 'blk') %>%
  arrange(pct)


