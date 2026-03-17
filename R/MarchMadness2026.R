# Created: 2026-03-10
# Author: Alex Zajichek
# Project: March Machine Learning Mania 2026
# Description: Build models and gather predictions
# Source: https://www.kaggle.com/competitions/march-machine-learning-mania-2026/overview

# Load packages
library(tidyverse)

# Set root directory (download zip file, then change working directory to that folder)
root <- "march-machine-learning-mania-2026/"

# Load sample submission file (makes it easy to get submission rows)
sample_submission <-
  read_csv(
    file = str_c(root, "SampleSubmissionStage2.csv")
  )

# Import all datasets
dat <-
  list(
    Mens = "M",
    Womens = "W"
  ) |>

  # For each group
  map(
    function(.tourney) {
      # Extract files names
      files <- str_subset(list.files(root), pattern = paste0("^", .tourney))

      # Set names
      names(files) <-
        files |>

        # Remove extension from the file name
        str_remove(
          pattern = "\\.csv$"
        ) |>

        # Remove the leading letter
        str_remove(
          pattern = "^(M|W)"
        )

      # Read in each file
      files |>

        # For each file name
        map(
          ~ read_csv(
            file = str_c(root, .x)
          )
        )
    }
  )

# Add sample submissions
dat$Mens$SampleSubmission <- sample_submission |>
  filter(str_detect(ID, "^2026_1"))
dat$Womens$SampleSubmission <- sample_submission |>
  filter(str_detect(ID, "^2026_3"))

### Lookup table for regular season stats

# All unique teams
reg_season <-
  dat |>
  map_df(
    ~ pluck(.x, "RegularSeasonDetailedResults") |>

      # Get all distinct teams for each season
      select(
        Season,
        WTeamID,
        LTeamID
      ) |>

      # Send teams down rows
      pivot_longer(
        cols = c(
          WTeamID,
          LTeamID
        ),
        names_to = "key",
        values_to = "TeamID"
      ) |>

      # Get unique teams
      select(
        -key
      ) |>
      distinct()
  )

# List of stats from every game for each team
reg_season <-
  reg_season |>

  # Get data for winning team
  inner_join(
    y = dat |>

      # Combine datasets across tournaments
      map_df(
        pluck,
        "RegularSeasonDetailedResults"
      ) |>

      # Remove unwanted column
      select(
        -DayNum
      ) |>

      # Replace leading L with O for opponent
      rename_all(
        str_replace,
        pattern = "^L",
        replacement = "O"
      ) |>

      # Remove leading W
      rename_all(
        str_remove,
        pattern = "^W"
      ),
    by = c("Season", "TeamID")
  ) |>

  # Bind rows for when teams lost
  bind_rows(
    reg_season |>

      # Get data for losing team
      inner_join(
        y = dat |>

          # Combine datasets across tournaments
          map_df(
            pluck,
            "RegularSeasonDetailedResults"
          ) |>

          # Remove unwanted column
          select(
            -DayNum
          ) |>

          # Replace leading W with O for opponent
          rename_all(
            str_replace,
            pattern = "^W",
            replacement = "O"
          ) |>

          # Remove leading L
          rename_all(
            str_remove,
            pattern = "^L"
          ) |>

          # Manually rename the location
          rename(
            Loc = OLoc
          ) |>

          #Convert to losing team's location
          mutate(
            Loc = case_when(
              Loc == "H" ~ "A",
              Loc == "A" ~ "H",
              TRUE ~ Loc
            )
          ),
        by = c("Season", "TeamID")
      )
  ) |>

  # Remove the opponents Team ID
  select(
    -OTeamID
  ) |>

  # Add record
  mutate(
    Wins = Score > OScore
  ) |>

  # Send statistics down the rows
  pivot_longer(
    cols = -c(
      Season,
      TeamID,
      Loc
    ),
    names_to = "key",
    values_to = "value",
  ) |>

  # Compute average within each location
  summarize(
    value = mean(value),
    .by = c(
      Season,
      TeamID,
      key
    )
  ) |>

  # Send over columns
  pivot_wider(
    names_from = key,
    values_from = value
  ) |>

  # Impute missing values with median
  mutate(
    across(
      where(is.numeric),
      \(x) coalesce(x, median(x, na.rm = T))
    )
  )

# Build modeling datasets
model_dat <-
  dat |>
  map(
    ~ .x |>

      # Extract tournament match-ups
      pluck("NCAATourneyCompactResults") |>
      mutate(PredSet = FALSE) |>

      # Bind to get sample prediction
      bind_rows(
        .x |>
          pluck("SampleSubmission") |>

          # Parse out teams
          separate(
            col = ID,
            into = c("Season", "WTeamID", "LTeamID"),
            sep = "_"
          ) |>
          select(
            -Pred
          ) |>
          mutate_all(
            as.numeric
          ) |>

          # Indicate prediction set
          add_column(
            PredSet = TRUE
          )
      ) |>

      # Rearrange to lower ID vs. higher ID
      transmute(
        Season,
        PredSet,
        Score_1 = case_when(
          WTeamID < LTeamID ~ WScore,
          TRUE ~ LScore
        ),
        TeamID_1 = case_when(
          WTeamID < LTeamID ~ WTeamID,
          TRUE ~ LTeamID
        ),
        Score_2 = case_when(
          WTeamID < LTeamID ~ LScore,
          TRUE ~ WScore
        ),
        TeamID_2 = case_when(
          WTeamID < LTeamID ~ LTeamID,
          TRUE ~ WTeamID
        )
      ) |>

      # Add game index
      mutate(
        ID = row_number()
      ) |>

      # Send down rows
      pivot_longer(
        cols = -c(
          ID,
          Season,
          PredSet
        ),
        names_to = "key",
        values_to = "value"
      ) |>

      # Separate into multiple columns
      separate(
        col = key,
        into = c("key", "Team")
      ) |>

      # Send the key back over the columns
      pivot_wider(
        names_from = key,
        values_from = value
      ) |>

      # Rename to avoid duplicate
      rename(Points = Score) |>

      # Get regular season state
      inner_join(
        y = reg_season,
        by = c("Season", "TeamID")
      )
  )

# Reformat to have single row per game
model_dat <-
  model_dat |>
  map(
    ~ .x |>

      # Send numeric variables down rows
      pivot_longer(
        cols = -c(
          Season,
          ID,
          Team,
          PredSet
        ),
        names_to = "key",
        values_to = "value"
      ) |>

      # Concatenate team number
      mutate(
        key = str_c(key, "_", Team)
      ) |>

      # Remove the team
      select(
        -Team
      ) |>

      # Send keys back over columns
      pivot_wider(
        names_from = key,
        values_from = value
      ) |>

      # Define outcome
      mutate(
        Target = as.numeric(Points_1 > Points_2)
      ) |>

      # Remove unwanted columns
      select(
        -Points_1,
        -Points_2
      ) |>

      # Rearrange
      select(
        ID,
        Season,
        PredSet,
        TeamID_1,
        TeamID_2,
        Target,
        everything()
      )
  )

# Get the seeds for the tournament
model_dat <-
  # For each data set
  map2(
    .x = model_dat,
    .y = as.list(names(model_dat)),
    ~ .x |>

      # Join to get seeds in tournament (Team 1)
      left_join(
        # <-- Maintain prediction data
        y = pluck(dat, .y, "NCAATourneySeeds") |>

          # Extract numeric seed
          mutate(
            Seed = Seed |>
              str_remove_all("[A-Za-z]") |>
              as.numeric()
          ) |>

          # Rename
          rename(
            TeamID_1 = TeamID,
            Seed_1 = Seed
          ),
        by = c("Season", "TeamID_1")
      ) |>

      # Join to get seeds in tournament (Team 2)
      left_join(
        # <-- Maintain prediction data
        y = pluck(dat, .y, "NCAATourneySeeds") |>

          # Extract numeric seed
          mutate(
            Seed = Seed |>
              str_remove_all("[A-Za-z]") |>
              as.numeric()
          ) |>

          # Rename
          rename(
            TeamID_2 = TeamID,
            Seed_2 = Seed
          ),
        by = c("Season", "TeamID_2")
      ) |>

      # Impute seeds for those not in tourney
      mutate(
        across(
          c(Seed_1, Seed_2),
          \(x) coalesce(x, 17)
        )
      ) |>

      # Rearrange
      select(
        ID,
        Season,
        PredSet,
        TeamID_1,
        TeamID_2,
        Seed_1,
        Seed_2,
        everything()
      )
  ) |>

  # Bind rows together
  bind_rows(.id = "Tourney")

### Add on some more predictors

## Conference tourney win counts

# Make lookup table
conf_tourney_wins <-
  dat |>

  # For each tourney
  map_df(
    ~ .x |>

      # Extract conf tourney games
      pluck("ConferenceTourneyGames") |>

      # Count the number of wins for each team
      summarize(
        ConferenceTourneyWins = n(),
        .by = c(
          Season,
          WTeamID
        )
      ),
    .id = "Tourney"
  )

# Attach to the modeling dataset
model_dat <-
  model_dat |>

  # Team 1
  left_join(
    y = conf_tourney_wins |>
      rename(
        TeamID_1 = WTeamID,
        ConferenceTourneyWins_1 = ConferenceTourneyWins
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = conf_tourney_wins |>
      rename(
        TeamID_2 = WTeamID,
        ConferenceTourneyWins_2 = ConferenceTourneyWins
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_2"
    )
  ) |>

  # Fill in with no wins
  mutate(
    across(
      starts_with("ConferenceTourneyWins"),
      \(x) coalesce(x, 0)
    )
  )

## Last year's tourney seed

# Make lookup
last_years_seed <-
  dat |>

  # For each tourney
  map_df(
    ~ .x |>

      # Extract seeds
      pluck("NCAATourneySeeds") |>

      # Extract numeric seed
      mutate(
        Seed = Seed |>
          str_remove_all("[A-Za-z]") |>
          as.numeric()
      ) |>

      # Keep a few columns
      transmute(
        Season = Season + 1,
        LastYearsSeed = Seed,
        TeamID
      ),
    .id = "Tourney"
  ) |>

  # Covid year
  mutate(
    Season = case_when(
      Season == 2020 ~ 2021,
      TRUE ~ Season
    )
  )

# Attach to the modeling dataset
model_dat <-
  model_dat |>

  # Team 1
  left_join(
    y = last_years_seed |>
      rename(
        TeamID_1 = TeamID,
        LastYearsSeed_1 = LastYearsSeed
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = last_years_seed |>
      rename(
        TeamID_2 = TeamID,
        LastYearsSeed_2 = LastYearsSeed
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_2"
    )
  ) |>

  # Fill in no seeds
  mutate(
    across(
      starts_with("LastYearsSeed"),
      \(x) coalesce(x, 17)
    )
  )

## Number of teams in conference

# Make lookup
teams_in_conference <-
  dat |>

  # For each tourney
  map_df(
    ~ .x |>

      # Extract conference
      pluck("TeamConferences") |>

      # Keep a few columns
      mutate(
        TeamsInConference = n_distinct(TeamID),
        .by = c(Season, ConfAbbrev)
      ) |>
      select(-ConfAbbrev),
    .id = "Tourney"
  )

# Attach to the modeling dataset
model_dat <-
  model_dat |>

  # Team 1
  left_join(
    y = teams_in_conference |>
      rename(
        TeamID_1 = TeamID,
        TeamsInConference_1 = TeamsInConference
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = teams_in_conference |>
      rename(
        TeamID_2 = TeamID,
        TeamsInConference_2 = TeamsInConference
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_2"
    )
  )

## Number of wins in last years tourney

# Make lookup
wins_in_last_years_tourney <-
  dat |>

  # For each tourney
  map_df(
    ~ .x |>

      pluck("NCAATourneyCompactResults") |>

      # Count the number of wins for each team
      summarize(
        Wins = n(),
        .by = c(
          Season,
          WTeamID
        )
      ) |>

      # Keep a few columns
      transmute(
        Season = Season + 1,
        LastYearTourneyWins = Wins,
        TeamID = WTeamID
      ),
    .id = "Tourney"
  ) |>

  # Covid year
  mutate(
    Season = case_when(
      Season == 2020 ~ 2021,
      TRUE ~ Season
    )
  )

# Attach to the modeling dataset
model_dat <-
  model_dat |>

  # Team 1
  left_join(
    y = wins_in_last_years_tourney |>
      rename(
        TeamID_1 = TeamID,
        LastYearTourneyWins_1 = LastYearTourneyWins
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = wins_in_last_years_tourney |>
      rename(
        TeamID_2 = TeamID,
        LastYearTourneyWins_2 = LastYearTourneyWins
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_2"
    )
  ) |>

  # Fill in with no wins
  mutate(
    across(
      starts_with("LastYearTourneyWins"),
      \(x) coalesce(x, 0)
    )
  )

## Number of unique cities (travel burden)

# Make a lookup table
number_of_unique_cities <-
  dat |>

  # For each tourney
  map_df(
    ~ .x |>

      pluck("GameCities") |>

      # Send down the rows
      pivot_longer(
        cols = c(WTeamID, LTeamID),
        names_to = "Team",
        values_to = "TeamID"
      ) |>

      # Count the number of unique cities per team
      summarize(
        UniqueCities = n_distinct(CityID),
        .by = c(
          Season,
          TeamID
        )
      ),
    .id = "Tourney"
  )

# Attach to the modeling dataset
model_dat <-
  model_dat |>

  # Team 1
  left_join(
    y = number_of_unique_cities |>
      rename(
        TeamID_1 = TeamID,
        UniqueCities_1 = UniqueCities
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = number_of_unique_cities |>
      rename(
        TeamID_2 = TeamID,
        UniqueCities_2 = UniqueCities
      ),
    by = c(
      "Tourney",
      "Season",
      "TeamID_2"
    )
  ) |>

  # Fill in missing
  mutate(
    UniqueCities_1 = coalesce(
      UniqueCities_1,
      median(UniqueCities_1, na.rm = TRUE)
    ),
    .by = c(Tourney, Seed_1)
  ) |>
  mutate(
    UniqueCities_2 = coalesce(
      UniqueCities_2,
      median(UniqueCities_2, na.rm = TRUE)
    ),
    .by = c(Tourney, Seed_2)
  )

### Make a nested data frame (CHANGE OF STRUCTURE HERE!)
model_dat <- model_dat |> cheese::divide(Tourney)

## AP Poll Rankings (Mens only)

# Make lookup table
ap_rankings <-
  dat$Mens$MasseyOrdinals |>

  # Filter to AP
  filter(SystemName == "AP") |>

  # Get min/max ranking
  summarize(
    APRankingMax = min(OrdinalRank),
    APRankingMin = max(OrdinalRank),
    .by = c(Season, TeamID)
  )

# Add to dataset
model_dat$Mens <-
  model_dat$Mens |>

  # Team 1
  left_join(
    y = ap_rankings |>
      rename(
        TeamID_1 = TeamID,
        APRankingMax_1 = APRankingMax,
        APRankingMin_1 = APRankingMin
      ),
    by = c(
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = ap_rankings |>
      rename(
        TeamID_2 = TeamID,
        APRankingMax_2 = APRankingMax,
        APRankingMin_2 = APRankingMin
      ),
    by = c(
      "Season",
      "TeamID_2"
    )
  ) |>

  # Fill in missing values
  mutate(
    across(
      starts_with("APRanking"),
      \(x) coalesce(x, 26)
    )
  )

## Momentum / trajectory

# Make a lookup table
team_momentum <-
  dat$Mens$MasseyOrdinals |>

  # Scale measures within seasons
  mutate(
    across(
      c(RankingDayNum, OrdinalRank),
      \(x) (x - mean(x)) / sd(x)
    ),
    .by = c(Season, SystemName)
  ) |>

  # Filter to those with at least 2 rankings
  filter(
    n() > 1,
    .by = c(
      Season,
      SystemName,
      TeamID
    )
  ) |>

  # Capture the rate of change (slope of rankings)
  summarize(
    Slope = lm(OrdinalRank ~ RankingDayNum)$coefficients[[2]],
    .by = c(
      Season,
      SystemName,
      TeamID
    )
  ) |>

  # Average over the ranking systems
  summarize(
    Momentum = mean(Slope),
    .by = c(
      Season,
      TeamID
    )
  )

# Add to dataset
model_dat$Mens <-
  model_dat$Mens |>

  # Team 1
  left_join(
    y = team_momentum |>
      rename(
        TeamID_1 = TeamID,
        Momentum_1 = Momentum
      ),
    by = c(
      "Season",
      "TeamID_1"
    )
  ) |>

  # Team 2
  left_join(
    y = team_momentum |>
      rename(
        TeamID_2 = TeamID,
        Momentum_2 = Momentum
      ),
    by = c(
      "Season",
      "TeamID_2"
    )
  ) |>

  # Fill in missing values
  mutate(
    across(
      starts_with("Momentum"),
      \(x) coalesce(x, 0)
    )
  )

### Make multiple test set variations
model_dat <-
  model_dat |>

  # Fpr each dataset
  map(
    function(.tourney) {
      .tourney |>

        # Remove pred set
        filter(!PredSet) |>

        # Bind to get test set variations
        bind_rows(
          # 1. Correct data (65% of model weight)
          .tourney |> filter(PredSet) |> add_column(Method = "Correct"),

          # 2. Opposite seeds (10% of model weight)
          .tourney |>
            filter(PredSet) |>
            mutate(temp_seed = Seed_1, Seed_1 = Seed_2, Seed_2 = temp_seed) |>
            select(-temp_seed) |>
            add_column(Method = "Opposite"),

          # 3. If teams had the same seed (average over possibilies, then 25% of model weight)
          1:16 |>
            map_df(
              ~ .tourney |>
                filter(PredSet) |>
                mutate(
                  across(
                    c(Seed_1, Seed_2),
                    \(x) .x
                  )
                )
            ) |>
            add_column(Method = "Same")
        )
    }
  )

### Modeling

# Load package
library(h2o)
h2o.init(nthreads = 50)

# Fit model and gather predictions for current season
all_predictions <-
  model_dat |>

  # For each data set
  map(
    function(.model_dat) {
      ## 1. Extract train/test data
      train <- .model_dat |> filter(!PredSet)
      test <- .model_dat |> filter(PredSet)

      # Extract predictor/outcome names
      x <- setdiff(
        names(train),
        c("ID", "PredSet", "TeamID_1", "TeamID_2", "Target", "Method")
      )
      y <- "Target"

      # Make a factor
      train$Target <- factor(train$Target)

      ## 2. Fit and extract the best model

      # Random shuffling
      train <- train[sample(1:nrow(train), nrow(train), replace = F), ]

      # Sending data to H20 cluster
      train <- as.h2o(train)

      # Run an auto ML process with cross-validation
      auto_mods <-
        h2o.automl(
          x = x,
          y = y,
          training_frame = train,
          nfolds = 10,
          distribution = "bernoulli",
          stopping_metric = "MSE",
          sort_metric = "MSE",
          seed = 11223344,
          max_runtime_secs = 3600 * 2
        )

      # Extract the top model (already trained on full training set)
      best_model <- auto_mods@leader

      ## 3. Make predictions on new dataset

      # Random shuffling
      test <- test[sample(1:nrow(test), nrow(test), replace = F), ]

      # Sending data to H20 cluster
      test <- as.h2o(test)

      # Get predictions for all records
      pred <- h2o.predict(best_model, newdata = test)

      # Return a data frame of the prediction sets
      h2o.cbind(test, pred) |>

        # Convert to data frame
        as.data.frame() |>
        as_tibble() |>

        # Keep a couple columns
        select(
          ID,
          Season,
          TeamID_1,
          TeamID_2,
          Method,
          Prediction = p1
        )
    }
  )

### Obtain final weighted predictions
final_predictions <-
  all_predictions |>

  # For each tournament
  map_df(
    function(.tourney) {
      bind_rows(
        .tourney |>

          # Filter to complete ones
          filter(Method %in% c("Correct", "Opposite")),
        .tourney |>

          # Filter to same ones
          filter(Method == "Same") |>

          # Average for each matchup
          summarize(
            Prediction = mean(Prediction),
            .by = c(
              ID,
              Season,
              TeamID_1,
              TeamID_2,
              Method
            )
          )
      ) |>

        # Send over the columns
        pivot_wider(
          names_from = Method,
          values_from = Prediction
        ) |>

        # Make final prediction
        mutate(Prediction = .65 * Correct + .25 * Same + .10 * Opposite) |>

        # Change location
        relocate(Prediction, .after = TeamID_2)
    },
    .id = "Tourney"
  )

### Write submission files

# Kaggle submission
sample_submission |>

  # Remove placeholder
  select(-Pred) |>

  # Join to get actual predictions
  inner_join(
    y = final_predictions |>

      # Format correctly
      transmute(
        ID = paste0(Season, "_", TeamID_1, "_", TeamID_2),
        Pred = Prediction
      ),
    by = "ID"
  ) |>

  # Write to file
  write_csv(file = paste0("MMLM_2026_", Sys.Date(), ".csv"))

# Attach names for lookup (bracket fill out)
sample_submission |>

  # Remove placeholder
  select(-Pred) |>

  # Join to get names
  inner_join(
    y = final_predictions |>

      # Filter to mens tournament
      filter(Tourney == "Mens") |>

      # Join to get team names
      inner_join(
        y = dat$Mens$Teams |> select(TeamID_1 = TeamID, TeamName_1 = TeamName),
        by = "TeamID_1"
      ) |>
      inner_join(
        y = dat$Mens$Teams |> select(TeamID_2 = TeamID, TeamName_2 = TeamName),
        by = "TeamID_2"
      ) |>

      # Keep some columns
      transmute(
        ID = paste0(Season, "_", TeamID_1, "_", TeamID_2),
        TeamName_1,
        TeamName_2,
        Prediction,
        Correct,
        Opposite,
        Same
      ),
    by = "ID"
  ) |>

  # Filter to teams in tournament
  filter(
    ID %in%
      (model_dat$Mens |>

        # Filter to teams in tournament
        filter(
          Season == 2026,
          PredSet,
          Method == "Correct",
          between(Seed_1, 1, 16),
          between(Seed_2, 1, 16)
        ) |>

        # Make an ID
        transmute(
          ID = paste0(Season, "_", TeamID_1, "_", TeamID_2)
        ) |>

        # Extract the ID
        pull("ID"))
  ) |>

  # Write to file
  write_csv(file = paste0("MMLM_2026_WithNames_", Sys.Date(), ".csv"))


###### Extra misc. (not run for submission)

model_dat |>

  # For each dataset
  map_df(
    ~ .x |>

      # Filter to prior years
      filter(Season < 2026) |>

      # Keep some columns
      select(
        Seed_1,
        Seed_2,
        Target
      ),
    .id = "Tourney"
  ) |>

  # Count the wins / games
  summarize(
    Wins = sum(Target),
    Games = n(),
    .by = c(
      Tourney,
      Seed_1,
      Seed_2
    )
  ) |>

  # Flip to consolidate
  mutate(
    Wins = case_when(
      Seed_1 >= Seed_2 ~ Games - Wins,
      TRUE ~ Wins
    ),
    SeedGood = pmin(Seed_1, Seed_2),
    SeedBad = pmax(Seed_1, Seed_2)
  ) |>

  # Now combine
  summarize(
    across(
      c(Wins, Games),
      sum
    ),
    .by = c(
      Tourney,
      SeedGood,
      SeedBad
    )
  ) |>

  # Sort the data
  arrange(Tourney, SeedGood, SeedBad) |>

  # Compute probability
  mutate(
    Win = Wins / Games,
    Loss = (Games - Wins) / Games
  ) |>
  select(-Wins) |>
  View()

library(dplyr)

adjust_march_madness_prob <- function(
  game_tbl,
  hist_tbl,
  hist_shrink_n = 20,
  prior_scale = 0.5,
  model_strength = 20
) {
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(x) 1 / (1 + exp(-x))

  global_mean <- hist_tbl %>%
    summarise(global_mean = sum(fav_wins) / sum(games)) %>%
    pull(global_mean)

  hist_tbl2 <- hist_tbl %>%
    rename(n_matchup = games)

  game_tbl %>%
    mutate(
      seed_fav = pmin(seed_a, seed_b),
      seed_dog = pmax(seed_a, seed_b),
      favored_is_a = seed_a < seed_b
    ) %>%
    left_join(hist_tbl2, by = c("seed_fav", "seed_dog")) %>%
    mutate(
      raw_hist_mean = fav_wins / n_matchup,
      shrunk_hist_mean = (n_matchup *
        raw_hist_mean +
        hist_shrink_n * global_mean) /
        (n_matchup + hist_shrink_n),
      p_hist = if_else(favored_is_a, shrunk_hist_mean, 1 - shrunk_hist_mean),
      m_ab = prior_scale * sqrt(n_matchup),
      w_model = model_strength / (model_strength + m_ab),
      p_adj = inv_logit(
        w_model * logit(p_model) + (1 - w_model) * logit(p_hist)
      )
    )
}
