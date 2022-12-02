# Created: 2022-12-02
# Author: Alex Zajichek
# Topic: Ordering dimensions within facets in a ggplot
# Objective: The goal is to make a plot showing the top/bottom five (5) U.S. states for hospital Associated Infections by the Standardized Infection Ratio
# Dataset: https://data.cms.gov/provider-data/dataset/k2ze-bqvw

## Load packages
require(tidyverse)
require(tidytext)

## 1. Import and clean the dataset
dat <-
  
  # Import dataset from website (Copied link from "Download full dataset")
  read_csv(
    file = "https://data.cms.gov/provider-data/sites/default/files/resources/b69e66780afdb7cbd0208ac9ae45d13e_1665414607/Healthcare_Associated_Infections-State.csv",
    na = c("", "Not Available")
  ) %>%
  
  # Filter to 50 U.S. states
  filter(State %in% state.abb) %>%
  
  # Parse out measures and components
  mutate(
    Component = 
      case_when(
        str_detect(`Measure ID`, "LOWER") ~ "Lower",
        str_detect(`Measure ID`, "UPPER") ~ "Upper",
        str_detect(`Measure ID`, "SIR") ~ "Estimate"
      ),
    Measure = 
      `Measure Name` %>%
      str_remove_all(
        pattern = ":.+"
      ) %>%
      str_remove_all(
        pattern = "\\s[(].+"
      )
  ) %>%
  
  # Send over the columns
  pivot_wider(
    id_cols = 
      c(
        "State",
        "Measure"
      ),
    names_from = Component,
    values_from = Score
  )

## 2. Identify top/bottom 5 states

# Set the selection number
n <- 5

# Make the plotting data set
plot_dat <-
  dat %>%
  
  # For each measure
  group_by(Measure) %>%
  
  # Get the best SIR's (lowest)
  slice_min(
    n = n,
    order_by = Upper,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  
  # Add group indicator
  add_column(
    Group = "Best"
  ) %>%
  
  # Bind with the worst states
  bind_rows(
    dat %>%
      
      # For each measure
      group_by(Measure) %>%
      
      # Get the worst SIR's (highest)
      slice_max(
        n = n,
        order_by = Lower,
        with_ties = FALSE
      ) %>%
      ungroup() %>%
      
      # Add group indicator
      add_column(
        Group = "Worst"
      )
  )

## 3. Make the plot
plot_dat %>% 
  
  # Make a plot
  ggplot(
    aes(
      reorder_within( # From tidytext
        x = State,
        by = -Estimate,
        within = Measure
      )
    )
  ) +
  geom_point(
    aes(
      y = Estimate,
      color = Group
    ),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = Lower,
      ymax = Upper,
      color = Group
    )
  ) +
  scale_x_reordered() + # From tidytext
  facet_wrap(
    ~Measure,
    scales = "free",
    labeller = label_wrap_gen()
  ) +
  coord_flip() +
  scale_color_manual(
    values = 
      c(
        "#5cbd55",
        "#f56751"
      )
  ) +
  scale_y_continuous(
    labels = function(x) paste0((x - 1) * 100, "%")
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray"),
    strip.background = element_rect(color = "gray", fill = "white", linewidth = 1.2),
    strip.text = element_text(size = 10, face = "italic"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Courier"),
    legend.position = "top",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14, family = "Courier"),
    plot.title = element_text(size = 18, face = "bold.italic"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 7, face = "italic")
  ) +
  xlab("State") +
  ylab("Infection Rate Over Expected (95% CI)") +
  labs(
    title = "Top 5 Best/Worst U.S. States",
    subtitle = "Hospital Associated Infections; CY 2021",
    caption =
      paste0(
        "Best/worst hospitals ranked by the minimum/maximum of the upper/lower confidence limit, respectively",
        "\n",
        "Infection Rate Over Expected is 100 X (SIR - 1)",
        "\n",
        "SIR = Standardized Infection Ratio",
        "\n",
        "Data source: https://data.cms.gov/provider-data/dataset/k2ze-bqvw"
      )
  )
