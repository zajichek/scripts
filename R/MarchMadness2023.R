# Created: 2023-03-10
# Author: Alex Zajichek
# Project: Kaggle March Madness Competition 2023
# Description: Builds models and writes predictions for the men's and women's tournament (now one competition only)

# Load packages
require(tidyverse)

# Path to data directories
root <- "Data/March Madness 2023/"

# Make a vector referencing mens/womens directories
dat <- list.files(root) %>% str_subset("^(Mens|Womens)$")
names(dat) <- dat

# Load sample submission file
sample_submission <-
  read_csv(
    file = str_c(root, "SampleSubmission2023.csv")
  )

# Process data
dat <-
  dat %>%
  
  # Read in data for each
  map(
    function(.tourney) {
      
      # Extract files names
      files <-
        .tourney %>%
        
        # Concatenate to get full path
        str_c(root, .) %>%
        
        # Get all available files
        list.files()
      
      # Set names 
      names(files) <- 
        files %>% 
        
        # Remove extension from the file name
        str_remove(
          pattern = "\\.csv$"
        ) %>% 
        
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
              file = str_c(root, .tourney, "/", .x)
            )
        )
      
    }
    
  )

### Lookup table for regular season stats
# All unique teams
reg_season <-
  dat %>%
  map_df(
    ~
      pluck(.x, "RegularSeasonDetailedResults") %>%
      
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
      distinct
  )

#List of stats from every game for each team
reg_season <-
  reg_season %>%
  
  #Get data for winning team
  inner_join(
    y =
      dat %>%
      
      #Combine datasets across tournaments
      map_df(
        pluck,
        "RegularSeasonDetailedResults"
      ) %>%
      
      #Remove unwanted column
      select(
        -DayNum
      ) %>%
      
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
  ) %>%
  
  #Bind rows for when teams lost
  bind_rows(
    
    reg_season %>%
      
      #Get data for losing team
      inner_join(
        y =
          dat %>%
          
          #Combine datasets across tournaments
          map_df(
            pluck,
            "RegularSeasonDetailedResults"
          ) %>%
          
          #Remove unwanted column
          select(
            -DayNum
          ) %>%
          
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
    
  ) %>%
  
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
    -Loc
  ) %>%
  
  #Compute average within each location
  group_by(
    Season,
    TeamID,
    Loc,
    key
  ) %>%
  summarise(
    value = mean(value)
  ) %>%
  ungroup %>%
  
  #Send over columns
  pivot_wider(
    names_from =
      c(
        key,
        Loc
      ),
    values_from = value
  ) %>%
  
  #Impute missing values with median
  mutate_if(
    is.numeric,
    ~coalesce(.x, median(.x, na.rm = T))
  )

# Build modeling datasets
model_dat <-
  dat %>%
  map(
    ~
      .x %>% 
      
      # Extract tournament match-ups
      pluck("NCAATourneyCompactResults") %>% mutate(PredSet = FALSE) %>%
      
      # Bind to get sample prediction
      bind_rows(
        sample_submission %>%
          
          #Parse out teams
          separate(
            col = ID,
            into = c("Season", "WTeamID", "LTeamID"),
            sep = "_"
          ) %>%
          select(
            -Pred
          ) %>%
          mutate_all(
            as.numeric 
          ) %>%
          
          #Indicate prediction set
          add_column(
            PredSet = TRUE
          )
      ) %>%
      
      #Rearrange to lower ID vs. higher ID
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
      
      #Team names
      inner_join(
        y = pluck(.x, "Teams"),
        by = "TeamID"  
      ) %>%
      
      #Make a state variable
      mutate(
        State = 
          TeamName %>%
          str_detect(
            pattern = "(St|State)$"
          ) %>%
          as.numeric()
      ) %>%
      
      #Remove team name
      select(
        -TeamName
      ) %>%
      
      #Get regular season state
      inner_join(
        y = reg_season,
        by = c("Season", "TeamID")
      )
  )

#Reformat to have single row per game
model_dat <-
  model_dat %>%
  map(
    ~
      .x %>%
      
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
      )
  )

#Split to train/test
train_model_dat <-
  model_dat %>%
  map(
    ~
      .x %>%
      filter(
        !PredSet
      ) %>%
      select(
        -PredSet
      )
  )
test_model_dat <-
  model_dat %>%
  map(
    ~
      .x %>%
      filter(
        PredSet
      ) %>%
      select(
        -PredSet,
        -Target
      )
  )

###Run a cross-validation

#Load some packages
require(randomForestSRC)
require(gbm)

#Set a seed
set.seed(123)

#Set up hyperparameters
params <- 
  list(
    GBM =
      expand.grid(
        interaction.depth = c(1, 2, 3),
        n.minobsinnode = c(1, 5, 10),
        shrinkage = c(.001, .01, .1),
        n.trees = c(50, 100),
        bag.fraction = c(.2, .5, 1)
      ),
    RF = 
      expand.grid(
        ntree = c(50, 100, 500),
        mtry = c(1, 12, 100),
        nodedepth = c(1, 5, 10)
      )
  ) %>%
  
  #Convert to list
  map(
    ~
      .x %>%
      as_tibble %>%
      split(1:nrow(.x))
  )

#Cross validation results
cv_results <-
  train_model_dat %>%
  
  #For each data set
  map(
    function(.dat) {
      
      #Set folds
      folds <- caret::createFolds(1:nrow(.dat), k = 5)
      
      folds %>%
        
        #For each fold
        map(
          function(.fold) {
            
            #Split to train/test
            train <- .dat[-.fold,]
            test <- .dat[.fold,]
            
            cat("GBM\n")
            
            #Run all GBM model
            gbm_mods <-
              params$GBM %>%
              
              #For each gbm parameterization
              map_df(
                function(.gbm) {
                  
                  cat("\tInteract: ", .gbm$interaction.depth, "\tNodes: ", .gbm$n.minobsinnode, "\tshrinkage: ", .gbm$shrinkage, "\tTrees: ", .gbm$n.trees, "\tBag: ", .gbm$bag.fraction, "\n")
                  
                  #Fit model
                  temp_gbm <-
                    gbm(
                      formula = Target ~ .,
                      data = train %>% select(-.id, -TeamID_1, -TeamID_2),
                      distribution = "bernoulli",
                      interaction.depth = .gbm$interaction.depth,
                      n.minobsinnode = .gbm$n.minobsinnode,
                      shrinkage = .gbm$shrinkage,
                      n.trees = .gbm$n.trees,
                      bag.fraction = .gbm$bag.fraction
                    )
                  
                  #Get predictions
                  temp_gbm_pred <-
                    predict(
                      temp_gbm,
                      newdata = test,
                      type = "response",
                      n.trees = .gbm$n.trees
                    )
                  
                  #Append to data
                  test %>%
                    select(
                      .id,
                      Target
                    ) %>%
                    add_column(
                      Prediction = temp_gbm_pred
                    )
                  
                },
                .id = "Params"
              )
            
            cat("RF\n")
            
            #For each random forest parameterization
            rf_mods <-
              params$RF %>%
              
              #For each gbm parameterization
              map_df(
                function(.rf) {
                  
                  cat("\tTree: ", .rf$ntree, "\tmtry: ", .rf$mtry, "\tnodedepth: ", .rf$nodedepth, "\n")
                  
                  #Fit model
                  temp_rf <-
                    rfsrc(
                      formula = Target ~ .,
                      data = train %>% select(-.id, -TeamID_1, -TeamID_2) %>% as.data.frame(),
                      ntree = .rf$ntree,
                      mtry = .rf$mtry,
                      nodedepth = .rf$nodedepth,
                      forest = TRUE
                    )
                  
                  #Get predictions
                  temp_rf_pred <-
                    predict(
                      temp_rf,
                      newdata = test %>% as.data.frame()
                    ) %>% pluck("predicted") %>% as.numeric
                  
                  #Append to data
                  test %>%
                    select(
                      .id,
                      Target
                    ) %>%
                    add_column(
                      Prediction = temp_rf_pred
                    )
                  
                },
                .id = "Params"
              )
            
            print("RF")
            
            #Add to list
            list(
              GBM = gbm_mods,
              RF = rf_mods
            )
            
          }
        )
      
    }
  )

save(cv_results, file = str_c(root, "MM2023_cv_results.RData"))

#Get top RF and GBM parmeterization
best_params <-
  cv_results %>%
  map(
    map,
    bind_rows,
    .id = "Model"
  ) %>%
  map(
    bind_rows,
    .id = "Fold"
  ) %>%
  bind_rows(
    .id = "Tourney"
  ) %>%
  group_by(
    Tourney,
    Fold,
    Model,
    Params
  ) %>%
  summarise(
    Brier = mean((Target - Prediction)^2)
  ) %>%
  ungroup %>%
  group_by(
    Tourney,
    Model,
    Params
  ) %>%
  summarise(
    Brier = mean(Brier)
  ) %>%
  ungroup %>%
  group_by(
    Tourney,
    Model
  ) %>%
  top_n(
    -1,
    Brier
  ) %>%
  ungroup

#Run final models
final_models <-
  best_params %>%
  cheese::divide(
    Tourney,
    Model,
    remove = FALSE
  ) %>%
  
  #For each model
  map_depth(
    .depth = 2,
    function(.x) {
      
      #Extract the data set
      temp_dat <- train_model_dat %>% pluck(.x$Tourney)
      
      #Extract parameters
      temp_params <- params %>% pluck(.x$Model, .x$Params)
      
      #Fit model
      if(.x$Model == "GBM") {
        
        #Fit model
        temp_mod <-
          gbm(
            formula = Target ~ .,
            data = temp_dat %>% select(-.id, -TeamID_1, -TeamID_2),
            distribution = "bernoulli",
            interaction.depth = temp_params$interaction.depth,
            n.minobsinnode = temp_params$n.minobsinnode,
            shrinkage = temp_params$shrinkage,
            n.trees = temp_params$n.trees,
            bag.fraction = temp_params$bag.fraction
          )
        
      } else {
        
        #Fit model
        temp_mod <-
          rfsrc(
            formula = Target ~ .,
            data = temp_dat %>% select(-.id, -TeamID_1, -TeamID_2) %>% as.data.frame(),
            ntree = temp_params$ntree,
            mtry = temp_params$mtry,
            nodedepth = temp_params$nodedepth,
            forest = TRUE
          )
        
      }
      
      list(
        Params = temp_params,
        Model = temp_mod
      )
      
    }
  )

save(final_models, file = str_c(root, "MM2023_final_models.RData"))
