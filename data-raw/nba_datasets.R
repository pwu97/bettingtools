####################################################################################
# Load packages
library(dplyr)
library(readr)
library(stringr)
library(teamcolors)
library(magrittr)
library(janitor)

####################################################################################
# NBA Team Names Data Frame
nba_teams <- teamcolors::teamcolors %>%
  filter(league == "nba") %>%
  select(location, sportslogos_name) %>%
  mutate(location = str_replace(location, " ", "")) %>%
  rename(Team = location)

####################################################################################
# Convert date functions
convert_date_to_full_2020 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2020", month, day), "%Y %m %d")
}

convert_date_to_full_2019 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2019", month, day), "%Y %m %d")
}

convert_date_to_full_2018 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2018", month, day), "%Y %m %d")
}

convert_date_to_full_2017 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2017", month, day), "%Y %m %d")
}

convert_date_to_full_2016 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2016", month, day), "%Y %m %d")
}

####################################################################################
# Wrangle 2019-2020 NBA Data
nba_2019_2020_data_2019 <- read_csv("~/Downloads/nba_odds_2020.csv") %>%
  left_join(nba_teams, by = "Team") %>%
  mutate(sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LALakers"),
                                   "Los Angeles Lakers", sportslogos_name),
         sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LAClippers"),
                                   "Los Angeles Clippers", sportslogos_name),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, sportslogos_name, Final, Open, Close, ML) %>%
  filter(Date > 1000) %>%
  mutate(Date = convert_date_to_full_2019(Date))

nba_2019_2020_data_2020 <- read_csv("~/Downloads/nba_odds_2020.csv") %>%
  left_join(nba_teams, by = "Team") %>%
  mutate(sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LALakers"),
                                   "Los Angeles Lakers", sportslogos_name),
         sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LAClippers"),
                                   "Los Angeles Clippers", sportslogos_name),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, sportslogos_name, Final, Open, Close, ML) %>%
  filter(Date < 1000) %>%
  mutate(Date = convert_date_to_full_2020(Date))

nba_odds_2019 <- rbind(nba_2019_2020_data_2019, nba_2019_2020_data_2020) %>%
  clean_names() %>%
  group_by(date, game) %>%
  mutate(home_name = rev(sportslogos_name),
         home_score = rev(final),
         home_ml = rev(ml),
         open = ifelse(open[1] > open[2], open, rev(open))) %>%
  ungroup() %>%
  filter(row_number() %% 2 == 1) %>%
  select(date, game, away_name = sportslogos_name, home_name,
         away_score = final, home_score, away_ml = ml,
         home_ml)

####################################################################################
# Wrangle 2018-2019 NBA Data
nba_2018_2019_data_2018 <- read_csv("~/Downloads/nba_odds_2019.csv") %>%
  left_join(nba_teams, by = "Team") %>%
  mutate(sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LALakers"),
                                   "Los Angeles Lakers", sportslogos_name),
         sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LAClippers"),
                                   "Los Angeles Clippers", sportslogos_name),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, sportslogos_name, Final, Open, Close, ML) %>%
  filter(Date > 1000) %>%
  mutate(Date = convert_date_to_full_2018(Date))

nba_2018_2019_data_2019 <- read_csv("~/Downloads/nba_odds_2019.csv") %>%
  left_join(nba_teams, by = "Team") %>%
  mutate(sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LALakers"),
                                   "Los Angeles Lakers", sportslogos_name),
         sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LAClippers"),
                                   "Los Angeles Clippers", sportslogos_name),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, sportslogos_name, Final, Open, Close, ML) %>%
  filter(Date < 1000) %>%
  mutate(Date = convert_date_to_full_2019(Date))

nba_odds_2019 <- rbind(nba_2018_2019_data_2018, nba_2018_2019_data_2019) %>%
  clean_names() %>%
  group_by(date, game) %>%
  mutate(home_name = rev(sportslogos_name),
         home_score = rev(final),
         home_ml = rev(ml)) %>%
  ungroup() %>%
  filter(row_number() %% 2 == 1) %>%
  select(date, game, away_name = sportslogos_name, home_name,
         away_score = final, home_score, away_ml = ml,
         home_ml)

####################################################################################
# Wrangle 2017-2018 NBA Data
nba_2017_2018_data_2017 <- read_csv("~/Downloads/nba_odds_2018.csv") %>%
  left_join(nba_teams, by = "Team") %>%
  mutate(sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LALakers"),
                                   "Los Angeles Lakers", sportslogos_name),
         sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LAClippers"),
                                   "Los Angeles Clippers", sportslogos_name),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, sportslogos_name, Final, Open, Close, ML) %>%
  filter(Date > 1000) %>%
  mutate(Date = convert_date_to_full_2017(Date))

nba_2017_2018_data_2018 <- read_csv("~/Downloads/nba_odds_2018.csv") %>%
  left_join(nba_teams, by = "Team") %>%
  mutate(sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LALakers"),
                                   "Los Angeles Lakers", sportslogos_name),
         sportslogos_name = ifelse((is.na(sportslogos_name) & Team == "LAClippers"),
                                   "Los Angeles Clippers", sportslogos_name),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, sportslogos_name, Final, Open, Close, ML) %>%
  filter(Date < 1000) %>%
  mutate(Date = convert_date_to_full_2018(Date))

nba_odds_2018 <- rbind(nba_2017_2018_data_2017, nba_2017_2018_data_2018) %>%
  clean_names() %>%
  group_by(date, game) %>%
  mutate(home_name = rev(sportslogos_name),
         home_score = rev(final),
         home_ml = rev(ml)) %>%
  ungroup() %>%
  filter(row_number() %% 2 == 1) %>%
  select(date, game, away_name = sportslogos_name, home_name,
         away_score = final, home_score, away_ml = ml,
         home_ml)

####################################################################################
# Save NBA Odds Datasets
usethis::use_data(nba_odds_2020, overwrite = TRUE)
usethis::use_data(nba_odds_2019, overwrite = TRUE)
usethis::use_data(nba_odds_2018, overwrite = TRUE)
