library(gganimate)
library(cowplot)
library(ggrepel)
library(transformr)
library(ggpubr)
library(magick)



# Dots Animation ----------------------------------------------------------



gid = 2021102410
pid = 1524

play_info <- plays %>%
  filter(game_id == gid, play_id == pid) %>%
  select(game_id, possession_team, defensive_team, quarter, down, yards_to_go,
         yardline_number, yardline_side, play_description) %>%
  head(1)

game_date <- paste(str_sub(play_info$game_id, 1, 4), 
                   str_sub(play_info$game_id, 5, 6), 
                   str_sub(play_info$game_id, 7, 8),sep = "-")

title_ <- glue::glue("{play_info$possession_team} (Off.) vs. {play_info$defensive_team} (Def.) - {game_date}")
subtitle_ <- glue::glue("{play_info$play_description}
                         Quarter: {play_info$quarter}
                         Down: {play_info$down}
                         Yards to Go: {play_info$yards_to_go}
                         Yardline: {play_info$yardline_side} {play_info$yardline_number}")



one_play_for_anim <- all_pos_scores_per_frame %>%
  ungroup() %>%
  filter(game_id == gid, play_id == pid) %>%
  #count(pos)
  inner_join(df %>%
               select(game_id, play_id, frame_id, nfl_id, x, y, team, jersey_number),
             by = c('game_id', 'play_id', 'frame_id', 'nfl_id')) %>%
  bind_rows(df_football %>%
              filter(game_id == gid, play_id == pid, frame_id - min(frame_id) <= 35)) %>%
  padr::fill_by_value(team, value = 'football') %>%
  padr::fill_by_value(nfl_id, value = 11111) %>%
  padr::fill_by_value(pos, value = 'football') %>%
  mutate(jersey_number = as.character(jersey_number )) %>%
  padr::fill_by_value(jersey_number, value = "") %>%
  mutate(pct = ifelse(pos == 'blk', pct, 0),
         pct = ifelse(is.na(pct), 0, pct))

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(one_play_for_anim$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(one_play_for_anim$x, na.rm = TRUE) + 10, -1), 120)
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)


animate_play <- ggplot() +
  scale_size_continuous(range = c(3, 6) ,guide = NULL) +
  # scale_color_manual(values = c("#A63860", "#2E34A6", "#253340", "#BF3434", "#95B3BB")) +
  
  scale_color_manual(values = c("#253340", "#A63860", "#2E34A6", "#95B3BB", "#BF3434")) +
  #scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  #scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  #scale_fill_manual(values = c("#e31837", "#654321", "#002244")) + 
  #scale_colour_manual(values = c("black", "#654321", "#c60c30")) + 
  annotate("text", x = df_hash$x[df_hash$x < 55/2], 
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(data = one_play_for_anim %>% 
               filter(pos != 'football') %>%
               mutate(pos = case_when(
                 pos == 'blk' ~ 'Pass Blocker',
                 pos == 'rush' ~ 'Pass Rusher',
                 pos == 'rec' ~ 'Receiver',
                 pos == 'cov' ~ 'Coverage',
                 pos == 'QB' ~ 'QB'
               )), aes(x = (xmax-y),
                       y = x, 
                       group = nfl_id, 
                       #size = team, 
                       size = pct,
                       colour = pos), alpha = 0.7) + 
  geom_point(data = one_play_for_anim %>% filter(pos == 'football'),
             aes(x = (xmax-y),
                 y = x, 
                 group = nfl_id, 
                 #size = team, 
                 colour = pos), 
             size = 2, color = 'black',alpha = 0.9) + 
  geom_text(data = one_play_for_anim, aes(x = (xmax-y), y = x, label = jersey_number),
            colour = "white",
            vjust = 0.36, size = 2.5) +
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(color = 'Role') +
  transition_time(frame_id)  +
  ease_aes('linear') + 
  NULL
title
subtitle

## Ensure timing of play matches 10 frames-per-second
play_length <- length(unique(one_play_for_anim$frame_id))
animate(animate_play, fps = 5, nframe = play_length,
        res = 300, height = 1800, width = 2000)
anim_save("one_play.gif")




# Lineplot animation ------------------------------------------------------


one_play_for_lineplot <- all_pos_scores_per_frame %>%
  filter(pos != 'football') %>%
  mutate(pos = case_when(
    pos == 'blk' ~ 'Pass Blocker',
    pos == 'rush' ~ 'Pass Rusher',
    pos == 'rec' ~ 'Receiver',
    pos == 'cov' ~ 'Coverage',
    pos == 'QB' ~ 'QB',
    pos == 'situ' ~ 'Situation'
  )) %>%
  ungroup() %>%
  # filter(game_id == 2021092605, play_id == 3093) %>%
  filter(game_id == gid, play_id == pid) %>%
  #count(pos)
  inner_join(df %>%
               select(game_id, play_id, frame_id, nfl_id, x, y, team, jersey_number),
             by = c('game_id', 'play_id', 'frame_id', 'nfl_id')) %>%
  bind_rows(df_football %>%
              filter(game_id == gid, play_id == pid, frame_id - min(frame_id) <= 34)) %>%
  padr::fill_by_value(team, value = 'football') %>%
  padr::fill_by_value(nfl_id, value = 11111) %>%
  padr::fill_by_value(pos, value = 'football') %>%
  mutate(jersey_number = as.character(jersey_number )) %>%
  padr::fill_by_value(jersey_number, value = "") %>%
  # filter(pos =='blk') %>%
  mutate(time = frame_id - min(frame_id)) %>%
  left_join(players) 



one_play_lineplot <- one_play_for_lineplot %>%
    filter(pos != 'football', pos != 'Situation', time <= 36) %>%
    ggplot(., aes(x = time, y = pct, color = pos, group = nfl_id)) +
    geom_line(alpha = .6) + 
    geom_text(data = one_play_for_lineplot %>%
                      filter(pos != 'football', time <= 36,
                             display_name %in% c('Samson Ebukam', 'Carson Wentz',
                                                 'Quenton Nelson', 'Nick Bosa')),
                    aes(x = 45, label = paste(display_name, jersey_number, sep = "-")),
                    size = 4, show.legend = FALSE, nudge_y = .01) +
    geom_segment(data = one_play_for_lineplot %>%
                   filter(pos != 'football', time <= 36,
                          display_name %in% c('Samson Ebukam', 'Carson Wentz',
                                              'Quenton Nelson', 'Nick Bosa')),
                 aes(xend = 45, yend = pct),
                 linetype = 2, colour = 'grey') +
    scale_color_manual(values = c("#253340", "#A63860", "#2E34A6", "#95B3BB", "#BF3434")) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(x = 'Frames Since Snap', y = 'Pressure Contribution Score',
         color = 'Role') +
    expand_limits(x = c(0, 50)) +
    transition_reveal(time) +
    ease_aes() +
  coord_fixed(ratio = 40)

play_length_2 <- length(unique(one_play_for_lineplot$time))
animate(one_play_lineplot, fps = 5, nframe = play_length_2, 
        res = 300, height = 1800, width = 2400)
anim_save("one_play_lineplot.gif")


# combine gifs into one side by side gif

a_mgif <- image_read('one_play.gif')
b_mgif <- image_read('one_play_lineplot.gif')

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = FALSE)
for(i in 2:35){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = FALSE)
  new_gif <- c(new_gif, combined)
}
image_write_gif(new_gif, 'combined.gif', delay = 1/5)


# ggsave(filename = 'one_play_lineplot.png', plot = one_play_lineplot, width = 10, height = 6)
# 
# lineplot_images <- image_read('one_play_lineplot.gif')
# 
# image_write(image = new_gif, path = 'test.png')


# force plot --------------------------------------------------------------


one_play_for_barplot <- one_play_for_lineplot %>%
  filter(between(frame_id - min(frame_id), 20, 35)) %>%
  group_by(nfl_id, display_name) %>%
  summarise(pct = round(mean(pct), 5)) %>%
  bind_rows(all_pos_scores_per_frame %>%
              ungroup() %>%
              filter(game_id == gid, play_id == pid, pos == 'situ') %>%
              filter(between(frame_id - min(frame_id), 20, 35)) %>%
              summarise(pct = mean(pct),
                        nfl_id = 22222,
                        display_name = 'Situation')) %>%
  mutate(neg = ifelse(pct <0, 'Negative', 'Positive')) %>% 
  na.omit() 
  

  


(one_play_barplot <- one_play_for_barplot %>%
  arrange(pct) %>%
  ggplot(., aes(x = 0, y = pct, fill = neg)) +
  geom_col(position = 'stack', alpha = .9, color = 'black') + 
  # geom_segment(aes(y = -.023, yend = 0, x = 1, xend = 1), color = 'black') +
  # geom_segment(aes(y = -.023, yend = -.023, x = 1, xend = .8), color = 'black') +
  # geom_segment(aes(y = 0, yend = 0, x = 1, xend = .8), color = 'black') +
  # geom_segment(aes(y = 0, yend = 0.392, x = 1, xend = 1), color = 'black') +
  # geom_segment(aes(y = 0.392, yend = 0.392, x = 1, xend = .8), color = 'black') +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = .2, x = 1.25, label = 'Contributes to\nPossible Pressure'), 
            color = '#A63860', size = 4.5) +
  geom_text(aes(y = -.05, x = 1.25, label = 'Prevents\nPossible Pressure'), 
            color = '#253340', size = 4.5) + 
  geom_text(data = one_play_for_barplot %>% 
              arrange(pct) %>%
              group_by(neg) %>%
              mutate(pct_ = cumsum(pct)) %>%
              filter(display_name %in% c('Samson Ebukam', 'Carson Wentz',
                                         'Quenton Nelson', 'Nick Bosa')) %>%
              mutate(pct_ = case_when(
                       display_name == 'Nick Bosa'~ pct_ + .008,
                       display_name == 'Samson Ebukam'~ pct_,
                       display_name == 'Quenton Nelson'~ pct_,
                       display_name == 'Carson Wentz'~ pct_),
                     display_name = str_replace(display_name, " ", "\n"),),
            aes(y = pct_-.025, x = 0, label = paste(display_name, sep = "-")),
            size = 4, color = 'black', show.legend = FALSE) +
  scale_fill_manual(values = c('#253340', '#A63860')) +
  scale_color_gradient2(low = 'black', high = 'black', guide = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  expand_limits(x = c(-1, 2), y = -.1) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = 'Cumulative Sum of Pressure Contribution Score'))
ggsave(filename = 'one_play_barplot.png', plot = one_play_barplot, width = 10, height = 3)


