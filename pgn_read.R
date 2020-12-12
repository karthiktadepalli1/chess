# Purpose:  
# Author: Karthik Tadepalli
# Created: Fri Nov 27 13:04:17 2020

#---------------SETUP------------------

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, tidyverse, janitor, data.table, bigchess, lubridate,
               scales, glue, zoo)

# set theme
if (!require("ggthemr")) {
  devtools::install_github("cttobin/ggthemr")
  library(ggthemr)
}
ggthemr("fresh")

path <- "~/Dropbox/projects/chess_analysis"

#-------------CLEAN DATA-----------------

# read in data with read.pgn from bigchess 
to_add <- c("WhiteElo", "BlackElo", "ECO", "SetUp", "TimeControl", "StartTime",
            "EndTime", "Termination", "Link")
chesscom <- read.pgn(glue("{path}/cleaned/chesscom_full.pgn"), add.tags = to_add) %>% clean_names()            
lichess <- read.pgn(glue("{path}/cleaned/lichess_therealslimkt_2020-12-11.pgn"), 
                    add.tags = to_add) %>% clean_names()

# clean discrepancies between them and append
lichess_clean <- lichess %>% 
  filter(grepl("Rated", event)) %>%
  mutate(termination_broad = termination,
         me = ifelse(white == "therealslimkt", "White", "Black"),
         link = site, 
         site = "Lichess")

chesscom_clean <- chesscom %>%
  mutate(event = case_when(
    time_control == "60" ~ "Rated Bullet game",
    time_control == "600" ~ "Rated Rapid game",
    TRUE ~ "Rated Blitz game"),
    termination_broad = ifelse(grepl("on time", termination), "Time forfeit", "Normal"),
    me = ifelse(white == "therealslim_kt", "White", "Black"))

# append and clean
all_games <- bind_rows(chesscom_clean, lichess_clean) %>%
  mutate(
    # formatting
    date = as_date(date, format = "%Y.%m.%d"),
    event = sub("Rated ", "", sub(" game", "", event)),
    event = ordered(event, levels = c("Bullet", "Blitz", "Rapid", "Classical", "Chess960")),
    # format results to me 
    win = as.numeric((me == "White" & result == "1-0") | (me == "Black" & result == "0-1")),
    draw = as.numeric(result == "1/2-1/2"),
    loss = (win == 0 & draw == 0),
    my_result = ifelse(win == 1, "Win", ifelse(draw == 1, "Draw", "Loss")),
    # ratings variables 
    elo_dif = (white_elo - black_elo) * ifelse(me == "White", 1, -1),
    my_elo = ifelse(me == "White", white_elo, black_elo)
  ) %>%
  # remove classical/960 games
  filter(!event %in% c("Chess960", "Classical"))

# output it for further analysis
write_csv(all_games, glue("{path}/cleaned/all_games.csv"))

#--------------GAMES PLAYED---------------

yr <- all_games %>%
  filter(date >= as_date("2020-01-01")) %>%
  group_by(date)

# graph of time spent on chess per day 
yr %>%
  filter(site == "Chess.com") %>%
  mutate(game_time = as.numeric(end_time - start_time)) %>%
  filter(game_time > 0) %>%
  group_by(event) %>% 
  summarise(mean_time = mean(game_time)) -> time_df
times <- time_df$mean_time
names(times) <- time_df$event
yr %>%
  mutate(time_event = times[event],
         time_day = sum(time_event)/60) %>%
  distinct(date, .keep_all = T) %>%
  arrange(date) %>%
  select(time_day) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = time_day)) + 
  geom_point() + geom_line() + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + 
  scale_y_continuous(labels = seq(0, 250, 50), breaks = seq(0, 250, 50)) + 
  labs(x = "", y = "", title = "Time (in minutes) playing chess every day",
       caption = "Game times are available only for Chess.com games. I imputed mean game times from those for Lichess games.") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
  

# total games over time
yr %>%
  mutate(n = n()) %>%
  ggplot(aes(x = date, y = n)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Date", y = "Number of games played", title = "Games played each day over time") + 
  theme(plot.title = element_text(hjust=0.5, size = 16))

# openings WR 
all_games %>%
  rename(ECO = eco, Result = result) %>%
  filter(me == "White") %>%
  browse_eco_opening(5) %>%
  rename(white = White_score, draw = Draws_percent, black = Black_score) %>%
  pivot_longer(cols = c("white", "draw", "black")) -> results
labs <- results$value
labs <- sapply(1:length(labs), function(i) { ifelse((i-2) %% 3 == 0, NA, paste0(labs[i], "%"))})
results %>%
  mutate(eco_n = paste0(ECO, " (", N, " games)")) %>%
  ggplot(aes(y = reorder(eco_n, N), x = value/100, fill = name)) + 
  geom_bar(stat = "identity", color = 'black') + 
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c("black" = "black", "white" = "white", "draw" = "grey50")) + 
  geom_text(aes(label = labs), position = position_stack(vjust = 0.5), color = "grey50") + 
  labs(x = "Winrate", y = "ECO Opening", title = "Winrate of my most common White openings", fill = "Winner") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
  

# win rate over time 
yr %>%
  mutate(n = n(),
         win = sum(win),
         winrate = win/n) %>%
  ggplot(aes(x = date, y = winrate)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(labels = percent) + 
  labs(x = "Date", y = "Winrate", title = "Winrate over time") + 
  theme(plot.title = element_text(hjust=0.5, size = 16))

# elo over time (by site/format)
yr %>%
  ungroup() %>%
  filter(event != "Rapid") %>%
  ggplot(aes(x = date, y = my_elo)) + 
  geom_point() + 
  geom_line()+ 
  facet_wrap(~site+event)

# my openings
all_games %>%
  filter(me == "White", event %in% c("Blitz", "Bullet")) %>%
  mutate(w1 = fct_lump_n(w1, n = 3)) %>%
  ggplot(aes(x = w1)) + 
  stat_count() + 
  facet_wrap(~event) + 
  labs(x = "First move as white", y = "Frequency")

all_games %>%
  filter(me == "Black", event %in% c("Blitz", "Bullet"), !is.na(b1)) %>%
  mutate(b1 = fct_lump_n(b1, n = 3)) %>%
  ggplot(aes(x = b1)) + 
  stat_count() + 
  facet_wrap(~event) + 
  labs(x = "First move as white", y = "Frequency")




  