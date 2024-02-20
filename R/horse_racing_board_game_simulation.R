# Created: 2024-02-18
# Author: Alex Zajichek
# Description: Simulates horse races (board game) to assess the true probability of each horse winning

# Load packages
library(tidyverse)

# Define a function to simulate one race (no money, just based on sequential dice rolls until winner)
race_horses <-
    function(board) { # The board has a set number of "jumps" each horse needs to make to win
        
        # Set parameters
        board$Score <- 0
        game_over <- FALSE
        winner <- NA
        total_spins <- 0
        
        # Roll until there is a winner
        while(!game_over) {
            
            # Spin the die
            d1 <- sample(1:6, 1)
            d2 <- sample(1:6, 1)
            
            # Compute the sum
            total <- d1 + d2

            # Compute spins
            total_spins <- total_spins + 1

            # Indicate the win
            board <-
                board |>
                mutate(
                    Score =
                        case_when(
                            Horse == total ~ Score + 1,
                            TRUE ~ Score
                        )
                )

            # Check if there is a winner
            is_winner <- filter(board, Needs == Score)
            
            if(nrow(is_winner) > 0) {
                
                game_over <- TRUE
                
                winner <- pluck(is_winner, "Horse")

            }

        }

        # Return results
        tibble(
            Horse = winner,
            Spins = total_spins
        )
    }

# Define a function to aggregate and plot the results
plot_horse_races <-
    function(race_results, board_desc) {
        
        race_results |>
            
            # Compute summaries
            summarize(
                Wins = n(),
                Spins = mean(Spins),
                .by = Horse
            ) |>
            
            # Compute rates
            mutate(
                Rate = Wins / sum(Wins),
                Horse = factor(Horse)
            ) |>
            
            # Make a plot
            ggplot(aes(x = Horse)) +
            geom_col(
                aes(y = Rate),
                width = .5,
                color = "black",
                fill = "#020b4f",
                linewidth = 1.25,
                alpha = .5
            ) +
            geom_text(
                aes(
                    y = Rate,
                    label = paste0(round(100*Rate, 1), "%")
                ),
                vjust = -.5
            ) +
            geom_point(
                aes(
                    y = Spins / 500,
                    group = 1
                ),
                size = 3
            ) +
            geom_line(
                aes(
                    y = Spins / 500,
                    group = 1
                )
            ) +
            scale_y_continuous(
                name = "Win Percentage (%)",
                labels = scales::percent,
                sec.axis = sec_axis(~.*500, name = "Avg. Total Rolls to Win")
            ) +
            theme(
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                plot.title = element_text(size = 16),
                plot.subtitle = element_text(size = 12, face = "italic")
            ) +
            labs(
                title = paste0(games, " Horse Races"),
                subtitle = paste0("Without scratches; ", board_desc)
            )
        
    }

### Run some simulations

# Number of games
games <- 10000

### Scenario 1: Actual board (proportional to relative die roll probabilities)

# Make the board
board1 <-
    tibble(
        Horse = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Needs = c(3, 6, 9, 12, 15, 18, 15, 12, 9, 6, 3)
    )

# Play the games
set.seed(123)
played_games1 <-
    1:games |>

    # Play the game many times
    map_df(
        function(x) {
            cat("Game: ", x, "\n")
            race_horses(board1) |> add_column(Game = x)
        }
    )

# Plot the results
played_games1 |> plot_horse_races("standard board")

### Scenario 2: Adjusted board

# Make the board
board2 <-
    tibble(
        Horse = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Needs = c(5, 7, 9, 11, 13, 15, 13, 11, 9, 7, 5)
    )

# Play the games
set.seed(123)
played_games2 <-
    1:games |>
    
    # Play the game many times
    map_df(
        function(x) {
            cat("Game: ", x, "\n")
            race_horses(board2) |> add_column(Game = x)
        }
    )

# Plot the results
played_games2 |> plot_horse_races("adjusted board")