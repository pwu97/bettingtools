####################################################################################
# Load packages
library(Lahman)
library(janitor)
library(dplyr)
library(readr)

####################################################################################
# Create team abbreviation and full name data frame
team_abb <- Teams %>%
  data.frame() %>%
  filter(yearID == 2018) %>%
  select(teamID, name) %>%
  rename("Team" = "teamID",
         "Name" = "name")
team_abb$Team <- as.character(team_abb$Team)
team_abb$Team[5] <- "CWS"
team_abb$Team[6] <- "CUB"
team_abb$Team[12] <- "KAN"
team_abb$Team[14] <- "LAD"
team_abb$Team[18] <- "NYY"
team_abb$Team[19] <- "NYM"
team_abb$Team[23] <- "SDG"
team_abb$Team[25] <- "SFO"
team_abb$Team[26] <- "STL"
team_abb$Team[27] <- "TAM"
team_abb$Name[13] <- "Los Angeles Angels"

####################################################################################
# Functions to convert dates in historical MLB lines data frame
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

convert_date_to_full_2015 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2015", month, day), "%Y %m %d")
}

convert_date_to_full_2014 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2014", month, day), "%Y %m %d")
}

convert_date_to_full_2013 <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste("2013", month, day), "%Y %m %d")
}

####################################################################################
# Create MLB 2019 odds data frame
mlb_odds_2019 <- read_csv("~/Downloads/mlb_odds_2019.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full_2019(Date),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, Team, Name, Final, Open, Close, `Run Line`, `X19`,
         `Open OU`, `X21`, `Close OU`, `X23`) %>%
  rename(Open_ML = Open,
         Close_ML = Close,
         `Open OU Line` = `Open OU`,
         `Close OU Line` = `Close OU`,
         run_line_odds = `X19`,
         open_ou_odds = `X21`,
         close_ou_odds = `X23`) %>%
  clean_names() %>%
  mutate(open_ml = as.numeric(open_ml)) %>%
  group_by(date, game) %>%
  mutate(home_team = rev(team),
         home_name = rev(name),
         home_final = rev(final),
         home_open_ml = rev(open_ml),
         home_close_ml = rev(close_ml),
         home_run_line = rev(run_line),
         home_run_line_odds = rev(run_line_odds),
         open_ou_line = rev(open_ou_line),
         open_ou_odds = rev(open_ou_odds),
         close_ou_line = rev(close_ou_line),
         close_ou_odds = rev(close_ou_odds)) %>%
  ungroup() %>%
  select(date, game, away_abbrev = team, home_abbrev = home_team,
         away_name = name, home_name, away_score = final,
         home_score = home_final,
         away_open_ml = open_ml, home_open_ml,
         away_close_ml = close_ml, home_close_ml,
         away_run_line = run_line, home_run_line,
         away_run_line_odds = run_line_odds, home_run_line_odds,
         open_ou_line, open_ou_line,
         open_ou_odds, open_ou_odds,
         close_ou_line, close_ou_line,
         close_ou_odds,
         close_ou_odds) %>%
  filter(row_number() %% 2 == 1) %>%
  tibble()

####################################################################################
# Create MLB 2018 odds data frame
mlb_odds_2018 <- read_csv("~/Downloads/mlb_odds_2018.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full_2018(Date),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, Team, Name, Final, Open, Close, `Run Line`, `X19`,
         `Open OU`, `X21`, `Close OU`, `X23`) %>%
  rename(Open_ML = Open,
         Close_ML = Close,
         `Open OU Line` = `Open OU`,
         `Close OU Line` = `Close OU`,
         run_line_odds = `X19`,
         open_ou_odds = `X21`,
         close_ou_odds = `X23`) %>%
  clean_names() %>%
  mutate(open_ml = as.numeric(open_ml)) %>%
  group_by(date, game) %>%
  mutate(home_team = rev(team),
         home_name = rev(name),
         home_final = rev(final),
         home_open_ml = rev(open_ml),
         home_close_ml = rev(close_ml),
         home_run_line = rev(run_line),
         home_run_line_odds = rev(run_line_odds),
         open_ou_line = rev(open_ou_line),
         open_ou_odds = rev(open_ou_odds),
         close_ou_line = rev(close_ou_line),
         close_ou_odds = rev(close_ou_odds)) %>%
  ungroup() %>%
  select(date, game, away_abbrev = team, home_abbrev = home_team,
         away_name = name, home_name, away_score = final,
         home_score = home_final,
         away_open_ml = open_ml, home_open_ml,
         away_close_ml = close_ml, home_close_ml,
         away_run_line = run_line, home_run_line,
         away_run_line_odds = run_line_odds, home_run_line_odds,
         open_ou_line, open_ou_line,
         open_ou_odds, open_ou_odds,
         close_ou_line, close_ou_line,
         close_ou_odds,
         close_ou_odds) %>%
  filter(row_number() %% 2 == 1)

####################################################################################
# Create MLB 2017 odds data frame
mlb_odds_2017 <- read_csv("~/Downloads/mlb_odds_2017.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full_2017(Date),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, Team, Name, Final, Open, Close, `Run Line`, `X19`,
         `Open OU`, `X21`, `Close OU`, `X23`) %>%
  rename(Open_ML = Open,
         Close_ML = Close,
         `Open OU Line` = `Open OU`,
         `Close OU Line` = `Close OU`,
         run_line_odds = `X19`,
         open_ou_odds = `X21`,
         close_ou_odds = `X23`) %>%
  clean_names() %>%
  mutate(open_ml = as.numeric(open_ml)) %>%
  group_by(date, game) %>%
  mutate(home_team = rev(team),
         home_name = rev(name),
         home_final = rev(final),
         home_open_ml = rev(open_ml),
         home_close_ml = rev(close_ml),
         home_run_line = rev(run_line),
         home_run_line_odds = rev(run_line_odds),
         open_ou_line = rev(open_ou_line),
         open_ou_odds = rev(open_ou_odds),
         close_ou_line = rev(close_ou_line),
         close_ou_odds = rev(close_ou_odds)) %>%
  ungroup() %>%
  select(date, game, away_abbrev = team, home_abbrev = home_team,
         away_name = name, home_name, away_score = final,
         home_score = home_final,
         away_open_ml = open_ml, home_open_ml,
         away_close_ml = close_ml, home_close_ml,
         away_run_line = run_line, home_run_line,
         away_run_line_odds = run_line_odds, home_run_line_odds,
         open_ou_line, open_ou_line,
         open_ou_odds, open_ou_odds,
         close_ou_line, close_ou_line,
         close_ou_odds,
         close_ou_odds) %>%
  filter(row_number() %% 2 == 1)
# Edge case
mlb_odds_2017$away_name[is.na(mlb_odds_2017$away_name)] <- "Los Angeles Dodgers"
mlb_odds_2017$home_name[is.na(mlb_odds_2017$home_name)] <- "Los Angeles Dodgers"

####################################################################################
# Create MLB 2016 odds data frame
mlb_odds_2016 <- read_csv("~/Downloads/mlb_odds_2016.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full_2016(Date),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, Team, Name, Final, Open, Close, `Run Line`, `X19`,
         `Open OU`, `X21`, `Close OU`, `X23`) %>%
  rename(Open_ML = Open,
         Close_ML = Close,
         `Open OU Line` = `Open OU`,
         `Close OU Line` = `Close OU`,
         run_line_odds = `X19`,
         open_ou_odds = `X21`,
         close_ou_odds = `X23`) %>%
  clean_names() %>%
  mutate(open_ml = as.numeric(open_ml)) %>%
  group_by(date, game) %>%
  mutate(home_team = rev(team),
         home_name = rev(name),
         home_final = rev(final),
         home_open_ml = rev(open_ml),
         home_close_ml = rev(close_ml),
         home_run_line = rev(run_line),
         home_run_line_odds = rev(run_line_odds),
         open_ou_line = rev(open_ou_line),
         open_ou_odds = rev(open_ou_odds),
         close_ou_line = rev(close_ou_line),
         close_ou_odds = rev(close_ou_odds)) %>%
  ungroup() %>%
  select(date, game, away_abbrev = team, home_abbrev = home_team,
         away_name = name, home_name, away_score = final,
         home_score = home_final,
         away_open_ml = open_ml, home_open_ml,
         away_close_ml = close_ml, home_close_ml,
         away_run_line = run_line, home_run_line,
         away_run_line_odds = run_line_odds, home_run_line_odds,
         open_ou_line, open_ou_line,
         open_ou_odds, open_ou_odds,
         close_ou_line, close_ou_line,
         close_ou_odds,
         close_ou_odds) %>%
  filter(row_number() %% 2 == 1)
# Edge case
mlb_odds_2016$away_name[is.na(mlb_odds_2016$away_name)] <- "Los Angeles Dodgers"
mlb_odds_2016$home_name[is.na(mlb_odds_2016$home_name)] <- "Los Angeles Dodgers"

####################################################################################
# Create MLB 2015 odds data frame
mlb_odds_2015 <- read_csv("~/Downloads/mlb_odds_2015.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full_2015(Date),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, Team, Name, Final, Open, Close, `Run Line`, `X19`,
         `Open OU`, `X21`, `Close OU`, `X23`) %>%
  rename(Open_ML = Open,
         Close_ML = Close,
         `Open OU Line` = `Open OU`,
         `Close OU Line` = `Close OU`,
         run_line_odds = `X19`,
         open_ou_odds = `X21`,
         close_ou_odds = `X23`) %>%
  clean_names() %>%
  mutate(open_ml = as.numeric(open_ml)) %>%
  group_by(date, game) %>%
  mutate(home_team = rev(team),
         home_name = rev(name),
         home_final = rev(final),
         home_open_ml = rev(open_ml),
         home_close_ml = rev(close_ml),
         home_run_line = rev(run_line),
         home_run_line_odds = rev(run_line_odds),
         open_ou_line = rev(open_ou_line),
         open_ou_odds = rev(open_ou_odds),
         close_ou_line = rev(close_ou_line),
         close_ou_odds = rev(close_ou_odds)) %>%
  ungroup() %>%
  select(date, game, away_abbrev = team, home_abbrev = home_team,
         away_name = name, home_name, away_score = final,
         home_score = home_final,
         away_open_ml = open_ml, home_open_ml,
         away_close_ml = close_ml, home_close_ml,
         away_run_line = run_line, home_run_line,
         away_run_line_odds = run_line_odds, home_run_line_odds,
         open_ou_line, open_ou_line,
         open_ou_odds, open_ou_odds,
         close_ou_line, close_ou_line,
         close_ou_odds,
         close_ou_odds) %>%
  filter(row_number() %% 2 == 1)
# Edge case
mlb_odds_2015$away_name[is.na(mlb_odds_2015$away_name)] <- "Los Angeles Dodgers"
mlb_odds_2015$home_name[is.na(mlb_odds_2015$home_name)] <- "Los Angeles Dodgers"

####################################################################################
# Create MLB 2014 odds data frame
mlb_odds_2014 <- read_csv("~/Downloads/mlb_odds_2014.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full_2014(Date),
         Game = rep(c(1:(n()/2)), 1, each = 2)) %>%
  select(Date, Game, Team, Name, Final, Open, Close, `Run Line`, `X19`,
         `Open OU`, `X21`, `Close OU`, `X23`) %>%
  rename(Open_ML = Open,
         Close_ML = Close,
         `Open OU Line` = `Open OU`,
         `Close OU Line` = `Close OU`,
         run_line_odds = `X19`,
         open_ou_odds = `X21`,
         close_ou_odds = `X23`) %>%
  clean_names() %>%
  mutate(open_ml = as.numeric(open_ml)) %>%
  group_by(date, game) %>%
  mutate(home_team = rev(team),
         home_name = rev(name),
         home_final = rev(final),
         home_open_ml = rev(open_ml),
         home_close_ml = rev(close_ml),
         home_run_line = rev(run_line),
         home_run_line_odds = rev(run_line_odds),
         open_ou_line = rev(open_ou_line),
         open_ou_odds = rev(open_ou_odds),
         close_ou_line = rev(close_ou_line),
         close_ou_odds = rev(close_ou_odds)) %>%
  ungroup() %>%
  select(date, game, away_abbrev = team, home_abbrev = home_team,
         away_name = name, home_name, away_score = final,
         home_score = home_final,
         away_open_ml = open_ml, home_open_ml,
         away_close_ml = close_ml, home_close_ml,
         away_run_line = run_line, home_run_line,
         away_run_line_odds = run_line_odds, home_run_line_odds,
         open_ou_line, open_ou_line,
         open_ou_odds, open_ou_odds,
         close_ou_line, close_ou_line,
         close_ou_odds,
         close_ou_odds) %>%
  filter(row_number() %% 2 == 1)
# Edge case
mlb_odds_2014$away_name[is.na(mlb_odds_2014$away_name)] <- "Los Angeles Dodgers"
mlb_odds_2014$home_name[is.na(mlb_odds_2014$home_name)] <- "Los Angeles Dodgers"

####################################################################################
# MLB Odds Datasets
usethis::use_data(mlb_odds_2019, overwrite = TRUE)
usethis::use_data(mlb_odds_2018, overwrite = TRUE)
usethis::use_data(mlb_odds_2017, overwrite = TRUE)
usethis::use_data(mlb_odds_2016, overwrite = TRUE)
usethis::use_data(mlb_odds_2015, overwrite = TRUE)
usethis::use_data(mlb_odds_2014, overwrite = TRUE)
