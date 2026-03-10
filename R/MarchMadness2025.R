# Created: 2025-03-10
# Author: Alex Zajichek
# Project: March Machine Learning Mania 2025
# Description: Build models and gather predictions
# Source: https://www.kaggle.com/competitions/march-machine-learning-mania-2025/overview

# Load packages
library(tidyverse)

# Set root directory (download zip file, then change working directory to that folder)
root <- "march-machine-learning-mania-2025/"

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
      files %>%
        
        # For each file name
        map(
          ~
            read_csv(
              file = str_c(root, .x)
            )
        )
      
    }
    
  )

### Lookup table for regular season stats

# All unique teams
reg_season <-
  dat |>
  map_df(
    ~
      pluck(.x, "RegularSeasonDetailedResults") |>
      
      # Get all distinct teams for each season
      select(
        Season,
        WTeamID,
        LTeamID
      ) |>
      
      # Send teams down rows
      pivot_longer(
        cols = 
          c(
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
    y =
      dat |>
      
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
        y =
          dat |>
          
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
            Loc = 
              case_when(
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
  
  # Send statistics down the rows
  pivot_longer(
    cols = 
      -c(
        Season,
        TeamID,
        Loc
      ),
    names_to = "key",
    values_to = "value",
  ) |>
  
  # Compute average within each location
  summarise(
    value = mean(value),
    .by = 
      c(
        Season,
        TeamID,
        Loc,
        key
      )
  ) |>
  
  # Send over columns
  pivot_wider(
    names_from =
      c(
        key,
        Loc
      ),
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
    ~
      .x |>
      
      # Extract tournament match-ups
      pluck("NCAATourneyCompactResults") |> mutate(PredSet = FALSE) |>
      
      # Bind to get sample prediction
      bind_rows(
        sample_submission |>
          
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
        Score_1 =
          case_when(
            WTeamID < LTeamID ~ WScore,
            TRUE ~ LScore
          ),
        TeamID_1 = 
          case_when(
            WTeamID < LTeamID ~ WTeamID,
            TRUE ~ LTeamID
          ),
        Score_2 = 
          case_when(
            WTeamID < LTeamID ~ LScore,
            TRUE ~ WScore
          ),
        TeamID_2 =
          case_when(
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
        cols = 
          -c(
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
    ~
      .x |>
      
      # Send numeric variables down rows
      pivot_longer(
        cols = 
          -c(
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
        Target = as.numeric(Score_1 > Score_2)
      ) |>
      
      # Remove unwanted columns
      select(
        -Score_1,
        -Score_2
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
    ~
      .x |>
      
      # Join to get seeds in tournament (Team 1)
      left_join( # <-- Maintain prediction data
        y = 
          pluck(dat, .y, "NCAATourneySeeds") |>
          
          # Extract numeric seed
          mutate(
            Seed = 
              Seed |>
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
      left_join( # <-- Maintain prediction data
        y = 
          pluck(dat, .y, "NCAATourneySeeds") |>
          
          # Extract numeric seed
          mutate(
            Seed = 
              Seed |>
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
      
      # Alternate scenario 1: What are the 2025 predictions if all teams were 1 seed?
      #mutate(
      #  across(
       #   c(Seed_1, Seed_2),
        #  \(x) case_when(Season == 2025 ~ 1, TRUE ~ x)
        #)
      #) |>
      
      # Alternate scenario 2: What if the 2025 seeds are opposite for each matchup?
      #mutate(
      #  temp = Seed_1,
       # Seed_1 = case_when(Season == 2025 ~ Seed_2, TRUE ~ Seed_1),
        #Seed_2 = case_when(Season == 2025 ~ temp, TRUE ~ Seed_2)
      #) |>
      #select(-temp) |>
      
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
  )

# Filter out opposing tourney from prediction sets
model_dat$Mens <-
  model_dat$Mens |>
  
  # Indicates Men's teams
  filter(
    str_detect(TeamID_1, "^1"),
    str_detect(TeamID_2, "^1")
  )
model_dat$Womens <-
  model_dat$Womens |>
  
  # Indicates Women's teams
  filter(
    str_detect(TeamID_1, "^3"),
    str_detect(TeamID_2, "^3")
  )

### Modeling

# Load package
library(h2o)
h2o.init()

# Fit model and gather predictions for current season
all_predictions <- 
  model_dat |>
  
  # For each data set
  map(
    function(.model_dat) {
      
      ## 1. Extract train/test data
      train <- .model_dat |> filter(!PredSet) 
      test <- .model_dat  |> filter(PredSet)
      
      # Extract predictor/outcome names
      x <- setdiff(names(train), c("ID", "PredSet", "TeamID_1", "TeamID_2", "Target"))
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
          balance_classes = TRUE,
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
          Prediction = p1
        )
      
    }
  )

### Write submission file

# Kaggle submission
sample_submission |> 
  
  # Remove placeholder
  select(-Pred) |>
  
  # Join to get predictions from model
  inner_join(
    y = 
      all_predictions |>
      map_df(
        ~
          .x |>
          transmute(
            ID = paste0(Season, "_", TeamID_1, "_", TeamID_2),
            Pred = Prediction
          )
      ),
    by = "ID"
  ) |>
  
  # Write to file
  write_csv(file = paste0("MMLM_2025_", Sys.Date(), "_WithSeeds.csv"))

# Attach names for lookup (bracket fill out)
sample_submission |> 
  
  # Remove placeholder
  select(-Pred) |>
  
  # Join to get predictions from model
  inner_join(
    y = 
      map2(
        .x = all_predictions,
        .y = as.list(names(all_predictions)),
        ~
          .x |>
          
          # Join to get the team name
          inner_join(
            y = pluck(dat, .y, "Teams") |> select(TeamID_1 = TeamID, TeamName_1 = TeamName),
            by = "TeamID_1"
          ) |>
          
          # Join to get the team name
          inner_join(
            y = pluck(dat, .y, "Teams") |> select(TeamID_2 = TeamID, TeamName_2 = TeamName),
            by = "TeamID_2"
          ) |>
          
          # Keep some columns
          transmute(
            ID = paste0(Season, "_", TeamID_1, "_", TeamID_2),
            TeamName_1,
            TeamName_2,
            Pred = Prediction
          )
      ) |>
      bind_rows(.id = "Tourney"),
    by = "ID"
  ) |>
  
  # Write to file
  write_csv(file = paste0("MMLM_2025_WithNames_", Sys.Date(), "_WithSeeds.csv"))
