library(pinnacle.API)
library(pinnacle.data)
library(odds.converter)
library(dplyr)
library(tidyr)
library(tidyverse)

library(devtools)
library(baseballr)
library(pitchRx)
library(Lahman)

head(Master)


head(MLB2016)
summary(MLB2016)
str(MLB2016)

summary(MLB2016$Lines)


SetCredentials("##", "##")

sport_data <- GetSports()
golf_id <- with(sport_data, id[name=="Golf"])


ugroup <- MLB2016 %>%
  ungroup %>%
  group_by(GameID) %>%
  arrange(desc(EventDateTimeUTC)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(TotalPoints) %>%
  summarize(Count = n())

unest <- MLB2016 %>% unnest() %>% group_by(GameID)

MLB2016 %>%
  unnest() %>%
  group_by(GameID) %>%
  arrange(desc(EventDateTimeUTC)) %>%
  slice(1) %>%
  ungroup() %>%
  select(GameID, TotalPoints, FinalScoreAway, FinalScoreHome) %>%
  mutate(TotalOutcome = case_when(
    FinalScoreAway + FinalScoreHome > TotalPoints ~ "Over",
    FinalScoreAway + FinalScoreHome < TotalPoints ~ "Under",
    FinalScoreAway + FinalScoreHome == TotalPoints ~ "Landed"
  )) %>%
  group_by(TotalPoints, TotalOutcome) %>%
  summarize(Count = n()) %>%
  print(n=100)

#getting park factors
fg_park(2016)
fg_park(2017)

#baseball savant april
batter_2016_apr <- scrape_statcast_savant_batter_all("2016-04-03", "2016-04-09")
batter_2016_apr1 <- scrape_statcast_savant_batter_all("2016-04-10", "2016-04-15")
batter_2016_apr2 <- scrape_statcast_savant_batter_all("2016-04-16", "2016-04-21")
batter_2016_apr3 <- scrape_statcast_savant_batter_all("2016-04-22", "2016-04-26")
batter_2016_apr4 <- scrape_statcast_savant_batter_all("2016-04-27", "2016-04-30")
batter_2016_ap <- rbind(batter_2016_apr, batter_2016_apr1, batter_2016_apr2, batter_2016_apr3, batter_2016_apr4)

pitcher_2016_apr <- scrape_statcast_savant_pitcher_all("2016-04-03", "2016-04-09")
pitcher_2016_apr1 <- scrape_statcast_savant_pitcher_all("2016-04-10", "2016-04-15")
pitcher_2016_apr2 <- scrape_statcast_savant_pitcher_all("2016-04-16", "2016-04-21")
pitcher_2016_apr3 <- scrape_statcast_savant_pitcher_all("2016-04-22", "2016-04-26")
pitcher_2016_apr4 <- scrape_statcast_savant_pitcher_all("2016-04-27", "2016-04-30")
pitcher_2016_ap <- rbind(pitcher_2016_apr, pitcher_2016_apr1, pitcher_2016_apr2, pitcher_2016_apr3, pitcher_2016_apr4)

player <- batter_2016_ap %>% group_by(player_name, pitch_type, bb_type, description) %>% 
  summarize(count = n())

donaldson <- player %>% filter(player_name == "Josh Donaldson")

#baseball savant may
batter_2016_may <- scrape_statcast_savant_batter_all("2016-05-01", "2016-05-06")
batter_2016_may1 <- scrape_statcast_savant_batter_all("2016-05-07", "2016-05-12")

batter_2016_june <- scrape_statcast_savant_batter_all("2016-06-01", "2016-06-30")
pitcher_2016 <- scrape_statcast_savant_pitcher_all("2016-07-12", "2016-10-02")  


edge_scrape("2016-04-07", "2016-04-10", "pitcher")
edge_scrape("2016-04-07", "2016-04-10", "batter")
edge_scrape("2016-04-07", "2016-04-10", "umpire")


daily_batter_bref("2017-04-07", "2017-04-10")


df <- fg_bat_leaders(2016, 2016)

woba <- df %>% 
  select("AB","PA","H","1B","2B","3B","HR","BB","HBP","IBB","SO","SF","SH","Seasons") %>%
  rename("uBB"= BB,"X1B"="1B", "X2B"="2B","X3B"="3B","Season"=Seasons) %>%
  mutate("SH" = SF + SH)

df_b_2014 <- daily_batter_bref("2014-03-22", "2014-09-28")
df_b_2015 <- daily_batter_bref("2015-04-05", "2015-10-04")
df_b_2016 <- daily_batter_bref("2016-04-03", "2016-10-02")

df_p_2014 <- daily_pitcher_bref("2014-03-22", "2014-09-28")
df_p_2015 <- daily_pitcher_bref("2015-04-05", "2015-10-04")
df_p_2016 <- daily_pitcher_bref("2016-04-03", "2016-10-02")

w_2014 <- woba_plus(df_b_2014) %>% arrange(desc(AB))
w_2015 <- woba_plus(df_b_2015) %>% arrange(desc(AB))
w_2016 <- woba_plus(df_b_2016) %>% arrange(desc(AB))
p_2014 <- fip_plus(df_p_2014) %>% arrange(desc(IP))
p_2015 <- fip_plus(df_p_2015) %>% arrange(desc(IP))
p_2016 <- fip_plus(df_p_2016) %>% arrange(desc(IP))

#team results
tor_2015 <- team_results_bref("TOR", 2015)
