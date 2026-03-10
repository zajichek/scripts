# Created: 2024-03-19
# Author: Alex Zajichek
# Description: Run model and get predictions for all possible matchups in the 2024 men's NCAA tournament
# Data source: https://www.kaggle.com/competitions/march-machine-learning-mania-2024/data

# Load packages
require(tidyverse)

# Path to data directories (set active directory to a folder containing a subdirectory called 'Mens' with all files for men's tournament)
root <- paste0(getwd(), "/Mens/")

# Extract files names
files <- list.files(root)

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

# Process data
dat <- 
    
    # Read in each file
    files %>%
    
    # For each file name
    map(
        ~
            read_csv(
                file = str_c(root, "/", .x)
            )
    )

### Lookup table of regular season
# All unique teams
reg_season <-
    dat |> 
    
    pluck("RegularSeasonDetailedResults") %>%
    
    #Get all distinct teams for each season
    select(
        Season,
        WTeamID,
        LTeamID
    ) %>%
    
    #Send teams down rows
    gather(
        key = "key",
        value = "TeamID",
        WTeamID,
        LTeamID
    ) %>%
    
    #Get unique teams
    select(
        -key
    ) %>%
    distinct()

#List of stats from every game for each team
reg_season <-
    reg_season |>
    
    # Get data for winning team
    inner_join(
        y = 
            dat$RegularSeasonDetailedResults %>%
            
            #Replace leading L with O for opponent
            rename_all(
                str_replace,
                pattern = "^L",
                replacement = "O"
            ) %>%
            
            #Remove leading W
            rename_all(
                str_remove,
                pattern = "^W"
            ),
        by = c("Season", "TeamID")
    ) |>
    
    #Bind rows for when teams lost
    bind_rows(
        reg_season |>
            
            # Get data for losing team
            inner_join(
                y = 
                    dat$RegularSeasonDetailedResults %>%
                    
                    #Replace leading W with O for opponent
                    rename_all(
                        str_replace,
                        pattern = "^W",
                        replacement = "O"
                    ) %>%
                    
                    #Remove leading L
                    rename_all(
                        str_remove,
                        pattern = "^L"
                    ) %>%
                    
                    #Manually rename the location
                    rename(
                        Loc = OLoc
                    ) %>%
                    
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
    
    #Remove the opponents Team ID
    select(
        -OTeamID
    ) %>%
    
    #Send statistics down the rows
    gather(
        key = "key",
        value = "value",
        -Season,
        -TeamID,
        -Loc,
        -DayNum
    ) %>%
    
    #Compute average within each location
    group_by(
        Season,
        TeamID,
        Loc,
        key
    ) %>%
    summarise(
        avg = mean(value),
        slope = lm(value~DayNum)$coefficients[[2]],
    ) %>%
    ungroup()

# Send over columns 
reg_season <-
    reg_season |>
    
    #Send over columns
    pivot_wider(
        names_from =
            c(
                key,
                Loc
            ),
        values_from = c(avg, slope)
    ) |>
    
    #Impute missing values with median
    mutate_if(
        is.numeric,
        ~coalesce(.x, median(.x, na.rm = T))
    )

# Make a list of all seeds in tournaments
all_seeds <-
    dat$NCAATourneySeeds |> 
    
    # Remove letters
    mutate(
        Seed = 
            Seed |>
            str_remove_all("^[A-Z]|[a-z]$") |>
            as.numeric()
    ) |>
    
    # Bind to get the current seeds
    bind_rows(
        read_csv(
            file = paste0(root, "2024_tourney_seeds.csv") # Make sure this contains 2024 teams
        ) |>
            
            # Filter to mens
            filter(Tournament == "M") |>
            
            # Remove letters
            mutate(
                Seed = 
                    Seed |>
                    str_remove_all("^[A-Z]|[a-z]$") |>
                    as.numeric(),
                Season = 2024
            ) |>
            select(
                Season,
                Seed,
                TeamID
            )
    )

# Build modeling data set
model_dat <- 
    dat$NCAATourneyCompactResults %>%
    
    #Rearrange to lower ID vs. higher ID
    transmute(
        Season,
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
            ),
        PredSet = FALSE
    ) %>%
    
    # Bind to get current season
    bind_rows(
        expand_grid(
            TeamID_1 = unique(all_seeds$TeamID[all_seeds$Season==2024]),
            TeamID_2 = unique(all_seeds$TeamID[all_seeds$Season==2024])
        ) |>
            
            # Keep unique combos
            filter(
                TeamID_1 < TeamID_2
            ) |>
            
            # Rearrange
            arrange(
                TeamID_1,
                TeamID_2
            ) |>
            mutate(
                Season = 2024,
                PredSet = TRUE
            )
    ) %>%
    
    #Add game index
    mutate(
        .id = 1:nrow(.)
    ) %>%
    
    #Send down rows
    gather(
        key = "key",
        value = "value",
        -.id,
        -Season,
        -PredSet
    ) %>%
    
    #Separate into multiple columns
    separate(
        col = key,
        into = c("key", "Team")
    ) %>%
    
    #Send the key back over the columns
    spread(
        key = key,
        value = value
    ) %>%
    
    #Get regular season state
    inner_join(
        y = reg_season,
        by = c("Season", "TeamID")
    ) |>
    
    # Get seed
    inner_join(
        y = all_seeds,
        by = c("Season", "TeamID")
    ) %>%
    
    #Send numeric variables down rows
    gather(
        key = "key",
        value = "value",
        -Season,
        -.id,
        -Team,
        -PredSet
    ) %>%
    
    #Concatenate team number
    mutate(
        key = str_c(key, "_", Team)
    ) %>%
    
    #Remove the team
    select(
        -Team
    ) %>%
    
    #Send keys back over columns
    spread(
        key = key,
        value = value
    ) %>%
    
    #Define outcome
    mutate(
        Target = as.numeric(Score_1 > Score_2)
    ) %>%
    
    #Remove unwanted columns
    select(
        -Score_1,
        -Score_2
    ) %>%
    
    #Rearrange
    select(
        .id,
        Season,
        PredSet,
        TeamID_1,
        TeamID_2,
        Target,
        everything()
    ) |>
    
    # Convert to factor
    mutate(
        across(
            c(Seed_1, Seed_2, TeamID_1, TeamID_2),
            factor
        )
    )

# Split the data sets
train <- model_dat |> filter(!PredSet) |> select(-PredSet, -.id) |> mutate(Target = factor(Target))
test <- model_dat |> filter(PredSet) |> select(-PredSet, -.id, -Target)

# Set predictors
X <- setdiff(names(train), "Target")
Y <- "Target"

# Initialize h2o
library(h2o)
h2o.init()

# Make h2o frames
train <- as.h2o(train)

# Run an auto ML process with cross-validation
auto_mods <- 
    h2o.automl(
        x = X,
        y = Y,
        training_frame = train,
        nfolds = 10,
        distribution = "bernoulli",
        seed = 11223344
    )

# Extract the top model (already trained on full training set)
best_model <- auto_mods@leader

# Get predictions for all records
test <- as.h2o(test)
pred <- h2o.predict(best_model, newdata = test)

# Final predictions for each team
final_preds <- 
    h2o.cbind(test, pred) |>
    
    # Convert to data frame
    as.data.frame() |>
    as_tibble() |>
    
    # Keep a couple columns
    select(
        TeamID_1,
        TeamID_2,
        Prediction = p1
    ) 

# Write to file
final_preds |> write_csv(file = "MM2024_FinalPreds_20240318.csv")

# Get teams for lookup
final_preds |> 
    
    # Re-convert to numeric for joining
    mutate(
        across(
            c(TeamID_1, TeamID_2),
            \(x) as.numeric(as.character(x))
        )
    ) |>
    
    # Get Team 1's name
    left_join(
        y = dat$Teams |> select(TeamID_1 = TeamID, Team1 = TeamName),
        by = "TeamID_1"
    ) |>
    
    # Get Team 2's name
    left_join(
        y = dat$Teams |> select(TeamID_2 = TeamID, Team2 = TeamName),
        by = "TeamID_2"
    ) |>
    
    # Keep a few columns
    select(
        Team1,
        Team2,
        Prediction
    ) |> 
    
    # Write to a file
    write_csv(file = "MM2024_FinalPreds_Names_20240319.csv")