library(Lahman)
library(janitor)
library(dplyr)

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

# Set year
year <- "2019"

# Function to convert dates in historical MLB lines data frame
convert_date_to_full <- function(date) {
  month <- date %/% 100
  day <- date - 100 * (date %/% 100)

  parse_date(paste(year, month, day), "%Y %m %d")
}

# Create MLB 2019 odds data frame
mlb_odds_2019 <- read_csv("~/Downloads/mlb_odds_2019.csv") %>%
  left_join(team_abb, by = "Team") %>%
  mutate(Date = convert_date_to_full(Date),
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

# 2019 MLB Odds Dataset
usethis::use_data(mlb_odds_2019, overwrite = TRUE)

